module StringSet = Set.Make (String)

type item_kind =
  | Let_item
  | Module_item
  | Type_item
  | Open_item
  | Include_item
  | Other_item

type top_level_item = {
  kind : item_kind;
  name : string;
  byte_start : int;
  byte_end : int;
}

type extraction_result = {
  has_client_components : bool;
  extracted_source : string;
  module_references : StringSet.t;
}

let has_client_component source =
  let summary = Analysis.analyze source in
  summary.react_client_component_origins <> []

let depth_delta = function
  | "{" | "struct" | "sig" | "begin" -> 1
  | "}" | "end" -> -1
  | _ -> 0

let is_top_level_keyword = function
  | "let" | "module" | "type" | "open" | "include" | "exception" | "external" ->
      true
  | _ -> false

let skip_attributes tokens_array idx =
  let len = Array.length tokens_array in
  let i = ref idx in
  while
    !i < len
    && (tokens_array.(!i) : Analysis.token).text = "["
    && !i + 1 < len
    && (tokens_array.(!i + 1) : Analysis.token).text = "@"
  do
    let bracket_depth = ref 1 in
    i := !i + 2;
    while !i < len && !bracket_depth > 0 do
      let t = (tokens_array.(!i) : Analysis.token).text in
      if t = "[" then incr bracket_depth else if t = "]" then decr bracket_depth;
      incr i
    done
  done;
  !i

let item_name tokens_array keyword_idx =
  let len = Array.length tokens_array in
  let keyword = (tokens_array.(keyword_idx) : Analysis.token).text in
  match keyword with
  | "let" ->
      let i = ref (keyword_idx + 1) in
      i := skip_attributes tokens_array !i;
      if !i < len && (tokens_array.(!i) : Analysis.token).text = "rec" then
        incr i;
      i := skip_attributes tokens_array !i;
      if !i < len then (tokens_array.(!i) : Analysis.token).text else "_"
  | "module" ->
      let i = keyword_idx + 1 in
      if i < len && (tokens_array.(i) : Analysis.token).text = "type" then
        if i + 1 < len then (tokens_array.(i + 1) : Analysis.token).text
        else "_"
      else if i < len then (tokens_array.(i) : Analysis.token).text
      else "_"
  | "type" ->
      let i = keyword_idx + 1 in
      if i < len && (tokens_array.(i) : Analysis.token).text = "nonrec" then
        if i + 1 < len then (tokens_array.(i + 1) : Analysis.token).text
        else "_"
      else if i < len then (tokens_array.(i) : Analysis.token).text
      else "_"
  | "open" ->
      let i = keyword_idx + 1 in
      let i =
        if i < len && (tokens_array.(i) : Analysis.token).text = "!" then i + 1
        else i
      in
      if i < len then (tokens_array.(i) : Analysis.token).text else "_"
  | "include" ->
      if keyword_idx + 1 < len then
        (tokens_array.(keyword_idx + 1) : Analysis.token).text
      else "_"
  | _ -> "_"

let item_kind_of_keyword = function
  | "let" -> Let_item
  | "module" -> Module_item
  | "type" -> Type_item
  | "open" -> Open_item
  | "include" -> Include_item
  | _ -> Other_item

(* Check if a `let` at depth 0 is a local let-in expression (not a new
   top-level binding). Scans forward from the `let` to find `in` at depth 0
   before the next top-level keyword at depth 0. *)
let is_local_let_in tokens_array start_idx =
  let len = Array.length tokens_array in
  let depth = ref 0 in
  let i = ref (start_idx + 1) in
  let found_eq = ref false in
  let result = ref false in
  (try
     while !i < len do
       let t = (tokens_array.(!i) : Analysis.token).text in
       let delta = depth_delta t in
       depth := !depth + delta;
       if !depth = 0 then
         if t = "=" && not !found_eq then found_eq := true
         else if t = "in" && !found_eq then (
           result := true;
           raise Exit)
         else if is_top_level_keyword t then raise Exit;
       incr i
     done
   with Exit -> ());
  !result

let parse_top_level_items tokens_array source_len =
  let len = Array.length tokens_array in
  let depth = ref 0 in
  let items = ref [] in
  let current_keyword_idx = ref (-1) in
  let has_current = ref false in
  for i = 0 to len - 1 do
    let token = (tokens_array.(i) : Analysis.token) in
    let delta = depth_delta token.text in
    depth := !depth + delta;
    if !depth = 0 && is_top_level_keyword token.text then
      (* Skip local let-in expressions (OCaml syntax: let x = ... in ...) *)
      if not (token.text = "let" && is_local_let_in tokens_array i) then (
        if !has_current then
          items :=
            {
              kind =
                item_kind_of_keyword
                  (tokens_array.(!current_keyword_idx) : Analysis.token).text;
              name = item_name tokens_array !current_keyword_idx;
              byte_start =
                (tokens_array.(!current_keyword_idx) : Analysis.token)
                  .byte_offset;
              byte_end = token.byte_offset;
            }
            :: !items;
        current_keyword_idx := i;
        has_current := true)
  done;
  if !has_current then
    items :=
      {
        kind =
          item_kind_of_keyword
            (tokens_array.(!current_keyword_idx) : Analysis.token).text;
        name = item_name tokens_array !current_keyword_idx;
        byte_start =
          (tokens_array.(!current_keyword_idx) : Analysis.token).byte_offset;
        byte_end = source_len;
      }
      :: !items;
  List.rev !items

let client_component_pattern =
  [| "["; "@"; "react"; "."; "client"; "."; "component"; "]" |]

let item_has_client_component tokens_array item =
  let pattern_len = Array.length client_component_pattern in
  let token_count = Array.length tokens_array in
  let max_start = token_count - pattern_len in
  let rec loop start =
    if start > max_start then false
    else
      let token = (tokens_array.(start) : Analysis.token) in
      if token.byte_offset >= item.byte_end then false
      else if token.byte_offset < item.byte_start then loop (start + 1)
      else
        let rec matches offset =
          if offset = pattern_len then true
          else if
            (tokens_array.(start + offset) : Analysis.token).text
            = client_component_pattern.(offset)
          then matches (offset + 1)
          else false
        in
        if matches 0 then true else loop (start + 1)
  in
  loop 0

let collect_identifiers_in_range tokens_array byte_start byte_end =
  let idents = ref StringSet.empty in
  Array.iter
    (fun (token : Analysis.token) ->
      if
        token.byte_offset >= byte_start
        && token.byte_offset < byte_end
        && String.length token.text > 0
        && Analysis.is_identifier_start token.text.[0]
      then idents := StringSet.add token.text !idents)
    tokens_array;
  !idents

(* Collect names defined by let/type/module bindings and labeled arguments
   within a byte range. Used to exclude locally-defined identifiers when
   computing the external dependency closure. *)
let collect_internal_definitions tokens_array byte_start byte_end =
  let names = ref StringSet.empty in
  let len = Array.length tokens_array in
  for i = 0 to len - 1 do
    let token = (tokens_array.(i) : Analysis.token) in
    if token.byte_offset >= byte_start && token.byte_offset < byte_end then (
      (* let/type/module bindings *)
      if
        i + 1 < len
        && (token.text = "let" || token.text = "type" || token.text = "module")
      then (
        let j = ref (i + 1) in
        j := skip_attributes tokens_array !j;
        if !j < len && (tokens_array.(!j) : Analysis.token).text = "rec" then
          incr j;
        j := skip_attributes tokens_array !j;
        if !j < len then
          let name = (tokens_array.(!j) : Analysis.token).text in
          if String.length name > 0 && Analysis.is_identifier_start name.[0]
          then names := StringSet.add name !names);
      (* Labeled arguments: ~name or ~(name : type) *)
      if token.text = "~" && i + 1 < len then
        let next = (tokens_array.(i + 1) : Analysis.token) in
        if next.text = "(" && i + 2 < len then (
          let name = (tokens_array.(i + 2) : Analysis.token).text in
          if String.length name > 0 && Analysis.is_identifier_start name.[0]
          then names := StringSet.add name !names)
        else if
          String.length next.text > 0
          && Analysis.is_identifier_start next.text.[0]
        then names := StringSet.add next.text !names)
  done;
  !names

let collect_module_references_in_range tokens_array byte_start byte_end =
  let modules = ref StringSet.empty in
  let len = Array.length tokens_array in
  for i = 0 to len - 2 do
    let token = (tokens_array.(i) : Analysis.token) in
    if
      token.byte_offset >= byte_start
      && token.byte_offset < byte_end
      && String.length token.text > 0
      && token.text.[0] >= 'A'
      && token.text.[0] <= 'Z'
      && (tokens_array.(i + 1) : Analysis.token).text = "."
    then modules := StringSet.add token.text !modules
  done;
  !modules

let compute_closure items client_items tokens_array =
  let item_by_name = Hashtbl.create 32 in
  List.iter
    (fun item ->
      if not (Hashtbl.mem item_by_name item.name) then
        Hashtbl.replace item_by_name item.name item)
    items;
  let included = Hashtbl.create 32 in
  (* Always include opens and includes *)
  List.iter
    (fun item ->
      match item.kind with
      | Open_item | Include_item ->
          Hashtbl.replace included item.byte_start item
      | _ -> ())
    items;
  (* Seed with client component items *)
  List.iter
    (fun item -> Hashtbl.replace included item.byte_start item)
    client_items;
  (* Iteratively expand by finding identifier references *)
  let changed = ref true in
  while !changed do
    changed := false;
    let current = Hashtbl.fold (fun _ item acc -> item :: acc) included [] in
    List.iter
      (fun item ->
        let idents =
          collect_identifiers_in_range tokens_array item.byte_start
            item.byte_end
        in
        (* Subtract locally-defined names to avoid false positives
            (e.g. let make inside a module matching top-level let make) *)
        let internal_defs =
          collect_internal_definitions tokens_array item.byte_start
            item.byte_end
        in
        let external_idents = StringSet.diff idents internal_defs in
        StringSet.iter
          (fun ident ->
            match Hashtbl.find_opt item_by_name ident with
            | Some dep_item when not (Hashtbl.mem included dep_item.byte_start)
              ->
                Hashtbl.replace included dep_item.byte_start dep_item;
                changed := true
            | _ -> ())
          external_idents)
      current
  done;
  Hashtbl.fold (fun _ item acc -> item :: acc) included []
  |> List.sort (fun a b -> compare a.byte_start b.byte_start)

let extract_regions source items =
  items
  |> List.map (fun item ->
      String.sub source item.byte_start (item.byte_end - item.byte_start))
  |> String.concat ""

let extract_client_code source =
  if not (has_client_component source) then
    {
      has_client_components = false;
      extracted_source = "";
      module_references = StringSet.empty;
    }
  else
    let tokens = Analysis.scan_code_tokens source in
    let tokens_array = Array.of_list tokens in
    let source_len = String.length source in
    let items = parse_top_level_items tokens_array source_len in
    let client_items =
      items
      |> List.filter (fun item ->
          (item.kind = Module_item || item.kind = Let_item)
          && item_has_client_component tokens_array item)
    in
    if client_items = [] then
      (* Client component detected by Analysis but not inside any top-level
         item we can identify. Fall back to including full source. *)
      {
        has_client_components = true;
        extracted_source = source;
        module_references =
          collect_module_references_in_range tokens_array 0 source_len;
      }
    else
      let closure = compute_closure items client_items tokens_array in
      let extracted_source = extract_regions source closure in
      let module_references =
        List.fold_left
          (fun acc item ->
            StringSet.union acc
              (collect_module_references_in_range tokens_array item.byte_start
                 item.byte_end))
          StringSet.empty closure
      in
      (* Also include module names from open/include statements *)
      let module_references =
        List.fold_left
          (fun acc item ->
            match item.kind with
            | Open_item | Include_item -> StringSet.add item.name acc
            | _ -> acc)
          module_references closure
      in
      { has_client_components = true; extracted_source; module_references }
