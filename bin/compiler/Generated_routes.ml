open Utopia_types

type tree = {
  route_entry : Routes.route_entry option;
  children : (route_segment * tree) list;
}

let empty_tree = { route_entry = None; children = [] }

let route_segments entry =
  if entry.Routes.route = "" then []
  else
    entry.Routes.route |> String.split_on_char '/'
    |> List.map (fun segment ->
        match Routes.parse_param_segment segment with
        | Ok parsed -> parsed
        | Error message -> invalid_arg message)

let specificity_of_segment = function
  | Static _ -> 4
  | Param (_, Single) -> 3
  | Param (_, Catch_all) -> 2
  | Param (_, Optional_catch_all) -> 1

let compare_route_specificity left right =
  let rec compare_scores left_scores right_scores =
    match (left_scores, right_scores) with
    | [], [] -> 0
    | _ :: _, [] -> -1
    | [], _ :: _ -> 1
    | left_score :: left_rest, right_score :: right_rest ->
        if left_score > right_score then -1
        else if left_score < right_score then 1
        else compare_scores left_rest right_rest
  in
  let result =
    compare_scores
      (List.map specificity_of_segment (route_segments left))
      (List.map specificity_of_segment (route_segments right))
  in
  if result = 0 then String.compare left.Routes.route right.Routes.route
  else result

let rec add_entry tree segments entry =
  match segments with
  | [] -> { tree with route_entry = Some entry }
  | segment :: rest ->
      let existing_child =
        tree.children
        |> List.find_opt (fun (candidate, _child) -> candidate = segment)
      in
      let child_tree =
        existing_child |> Option.map snd |> Option.value ~default:empty_tree
      in
      let updated_child = add_entry child_tree rest entry in
      let other_children =
        tree.children
        |> List.filter (fun (candidate, _child) -> candidate <> segment)
      in
      { tree with children = other_children @ [ (segment, updated_child) ] }

let build_tree route_entries =
  route_entries
  |> List.sort (fun left right ->
      String.compare left.Routes.route right.Routes.route)
  |> List.fold_left
       (fun tree entry -> add_entry tree (route_segments entry) entry)
       empty_tree

let module_name_of_segment = function
  | Static segment ->
      Names.sanitize_module_component (String.lowercase_ascii segment)
  | Param (name, Single) -> Names.sanitize_module_component ("param_" ^ name)
  | Param (name, Catch_all) ->
      Names.sanitize_module_component ("catch_all_" ^ name)
  | Param (name, Optional_catch_all) ->
      Names.sanitize_module_component ("optional_catch_all_" ^ name)

let module_path_of_entry entry =
  route_segments entry |> List.map module_name_of_segment |> String.concat "."

let constructor_name_of_entry entry =
  Names.native_module_name_of_source entry.Routes.source_file

let ocaml_expr_of_param_kind = function
  | Single -> "Utopia_types.Single"
  | Catch_all -> "Utopia_types.Catch_all"
  | Optional_catch_all -> "Utopia_types.Optional_catch_all"

let params_module_ref entry =
  let path = module_path_of_entry entry in
  if path = "" then "Route_params" else path ^ ".Route_params"

let segments_expr entry =
  let render_piece = function
    | Static segment -> Printf.sprintf "[%S]" (String.lowercase_ascii segment)
    | Param (name, kind) when entry.Routes.route_schema_has_params ->
        Printf.sprintf
          "(Utopia_route.Params.segments_exn ~route:%S ~name:%S ~kind:%s \
           encoded_params)"
          (Routes.pp_route entry.Routes.route)
          name
          (ocaml_expr_of_param_kind kind)
    | Param (name, Single) -> Printf.sprintf "[%s]" name
    | Param (name, Catch_all) ->
        Printf.sprintf "Utopia_route.Nonempty.to_list %s" name
    | Param (name, Optional_catch_all) ->
        Printf.sprintf "(match %s with None -> [] | Some value -> value)" name
  in
  match route_segments entry |> List.map render_piece with
  | [] -> "[]"
  | piece :: rest ->
      List.fold_left (fun acc piece -> acc ^ " @ " ^ piece) piece rest

let make_signature entry =
  let param_args =
    if entry.Routes.route_schema_has_params then
      if entry.Routes.params = [] then [] else [ "~params" ]
    else
      entry.Routes.params
      |> List.map (fun (name, kind) ->
          match kind with
          | Single | Catch_all -> "~" ^ name
          | Optional_catch_all -> "?" ^ name)
  in
  let query_args =
    if entry.Routes.route_schema_has_query then [ "?query" ] else []
  in
  let hash_args =
    if entry.Routes.route_schema_has_hash then [ "?hash" ] else []
  in
  String.concat " " (param_args @ query_args @ hash_args @ [ "()" ])

let route_value_available entry =
  (not entry.Routes.route_schema_has_params)
  && entry.Routes.params
     |> List.for_all (fun (_name, kind) -> kind = Optional_catch_all)

let render_route_entry indent entry =
  let lines = ref [] in
  let add line = lines := !lines @ [ indent ^ line ] in
  let add_body line = lines := !lines @ [ indent ^ "  " ^ line ] in
  (match entry.Routes.route_schema_module with
  | Some module_name when entry.Routes.route_schema_has_params ->
      add (Printf.sprintf "module Route_params = %s.Params" module_name)
  | _ -> ());
  (match entry.Routes.route_schema_module with
  | Some module_name when entry.Routes.route_schema_has_query ->
      add (Printf.sprintf "module Route_query = %s.Query" module_name)
  | _ -> ());
  (match entry.Routes.route_schema_module with
  | Some module_name when entry.Routes.route_schema_has_hash ->
      add (Printf.sprintf "module Route_hash = %s.Hash" module_name)
  | _ -> ());
  add (Printf.sprintf "let make %s =" (make_signature entry));
  if entry.Routes.route_schema_has_params then
    add_body "let encoded_params = Route_params.encode params in";
  if entry.Routes.route_schema_has_query then
    add_body
      "let query = match query with None -> [] | Some value -> \
       Route_query.encode value in";
  if entry.Routes.route_schema_has_hash then
    add_body
      "let hash = match hash with None -> None | Some value -> Some \
       (Route_hash.encode value) in";
  let call =
    let base =
      Printf.sprintf "Utopia_route.from_segments ~segments:(%s)"
        (segments_expr entry)
    in
    let with_query =
      if entry.Routes.route_schema_has_query then base ^ " ~query" else base
    in
    let with_hash =
      if entry.Routes.route_schema_has_hash then with_query ^ " ?hash"
      else with_query
    in
    with_hash ^ " ()"
  in
  add_body call;
  if route_value_available entry then add "let route = make ()";
  String.concat "\n" !lines

let rec render_tree indent tree =
  let rendered_route =
    match tree.route_entry with
    | None -> []
    | Some entry -> [ render_route_entry indent entry ]
  in
  let rendered_children =
    tree.children
    |> List.sort (fun (left, _left_child) (right, _right_child) ->
        String.compare
          (module_name_of_segment left)
          (module_name_of_segment right))
    |> List.map (fun (segment, child) ->
        let module_name = module_name_of_segment segment in
        let body = render_tree (indent ^ "  ") child in
        String.concat "\n"
          [
            indent ^ Printf.sprintf "module %s = struct" module_name;
            body;
            indent ^ "end";
          ])
  in
  String.concat "\n\n" (rendered_route @ rendered_children)

let list_pattern items = "[" ^ String.concat "; " items ^ "]"

let pattern_check pattern =
  Printf.sprintf "(match lowercase_segments with | %s -> true | _ -> false)"
    pattern

let rec cons_pattern heads tail =
  match heads with
  | [] -> ( match tail with Some tail -> tail | None -> "[]")
  | head :: rest -> head ^ " :: " ^ cons_pattern rest tail

let indent_block prefix text =
  text |> String.split_on_char '\n'
  |> List.map (fun line -> prefix ^ line)
  |> String.concat "\n"

let query_module_ref entry =
  let path = module_path_of_entry entry in
  if path = "" then "Route_query" else path ^ ".Route_query"

let hash_module_ref entry =
  let path = module_path_of_entry entry in
  if path = "" then "Route_hash" else path ^ ".Route_hash"

let current_param_field_type (name, kind) =
  match kind with
  | Single -> Printf.sprintf "%s : string" name
  | Catch_all -> Printf.sprintf "%s : string Utopia_route.Nonempty.t" name
  | Optional_catch_all -> Printf.sprintf "%s : string list option" name

let raw_params_of_exact_segments segments =
  segments
  |> List.filter_map (function
    | Param (name, Single) ->
        Some (Printf.sprintf "(%S, Utopia_route.Params.one %s)" name name)
    | _ -> None)

let current_field_exprs_of_exact_segments segments =
  segments
  |> List.filter_map (function
    | Param (name, Single) -> Some (Printf.sprintf "%s = %s" name name)
    | _ -> None)

let exact_patterns segments =
  let original =
    segments
    |> List.map (function
      | Static _ -> "_"
      | Param (name, Single) -> name
      | _ -> "_")
  in
  let lowercase =
    segments
    |> List.map (function
      | Static segment -> Printf.sprintf "%S" (String.lowercase_ascii segment)
      | Param (_, Single) -> "_"
      | _ -> "_")
  in
  (list_pattern original, list_pattern lowercase)

let render_current_success entry field_exprs =
  let constructor = constructor_name_of_entry entry in
  if field_exprs = [] then Printf.sprintf "Some %s" constructor
  else
    Printf.sprintf "Some (%s { %s })" constructor
      (String.concat "; " field_exprs)

let wrap_expression text = "(\n" ^ text ^ "\n)"

let render_current_decode entry ~field_exprs ~raw_params =
  let path_field_exprs =
    if entry.Routes.route_schema_has_params then [ "params = params" ]
    else field_exprs
  in
  let success =
    render_current_success entry
      (path_field_exprs
      @ (if entry.Routes.route_schema_has_query then [ "query = query" ] else [])
      @ if entry.Routes.route_schema_has_hash then [ "hash = hash" ] else [])
  in
  let success =
    if entry.Routes.route_schema_has_params then
      wrap_expression
        (Printf.sprintf
           "let params = %s.decode [%s] in\n\
            match params with\n\
            | Some params -> %s\n\
            | None -> None"
           (params_module_ref entry)
           (String.concat "; " raw_params)
           success)
    else success
  in
  match
    (entry.Routes.route_schema_has_query, entry.Routes.route_schema_has_hash)
  with
  | false, false -> success
  | true, false ->
      wrap_expression
        (Printf.sprintf
           "let query_entries = Utopia_route.query_entries route in\n\
            let query = if query_entries = [] then Some None else Option.map \
            (fun value -> Some value) (%s.decode query_entries) in\n\
            match query with\n\
            | Some query -> %s\n\
            | None -> None"
           (query_module_ref entry) success)
  | false, true ->
      wrap_expression
        (Printf.sprintf
           "let hash_value = Utopia_route.hash route in\n\
            let hash = match hash_value with None -> Some None | Some value -> \
            Option.map (fun decoded -> Some decoded) (%s.decode value) in\n\
            match hash with\n\
            | Some hash -> %s\n\
            | None -> None"
           (hash_module_ref entry) success)
  | true, true ->
      wrap_expression
        (Printf.sprintf
           "let query_entries = Utopia_route.query_entries route in\n\
            let hash_value = Utopia_route.hash route in\n\
            let query = if query_entries = [] then Some None else Option.map \
            (fun value -> Some value) (%s.decode query_entries) in\n\
            let hash = match hash_value with None -> Some None | Some value -> \
            Option.map (fun decoded -> Some decoded) (%s.decode value) in\n\
            match (query, hash) with\n\
            | Some query, Some hash -> %s\n\
            | _ -> None"
           (query_module_ref entry) (hash_module_ref entry) success)

let render_inner_match original_pattern body =
  Printf.sprintf "match path_segments with\n| %s ->\n%s\n| _ -> None"
    original_pattern (indent_block "    " body)

let render_exact_current_branch entry =
  let original_pattern, lowercase_pattern =
    exact_patterns (route_segments entry)
  in
  let body =
    render_current_decode entry
      ~field_exprs:
        (current_field_exprs_of_exact_segments (route_segments entry))
      ~raw_params:(raw_params_of_exact_segments (route_segments entry))
  in
  Printf.sprintf "if %s then\n%s"
    (pattern_check lowercase_pattern)
    (indent_block "  " (render_inner_match original_pattern body))

let render_catch_all_current_branch entry name prefix =
  let prefix_original =
    prefix
    |> List.map (function
      | Static _ -> "_"
      | Param (param, Single) -> param
      | _ -> "_")
  in
  let prefix_lowercase =
    prefix
    |> List.map (function
      | Static segment -> Printf.sprintf "%S" (String.lowercase_ascii segment)
      | Param (_, Single) -> "_"
      | _ -> "_")
  in
  let original_pattern =
    cons_pattern (prefix_original @ [ name ^ "_head" ]) (Some (name ^ "_tail"))
  in
  let lowercase_pattern =
    cons_pattern (prefix_lowercase @ [ "_" ]) (Some "_")
  in
  let body =
    render_current_decode entry
      ~field_exprs:
        (current_field_exprs_of_exact_segments prefix
        @ [
            Printf.sprintf
              "%s = Utopia_route.Nonempty.make ~head:%s_head ~tail:%s_tail ()"
              name name name;
          ])
      ~raw_params:
        (raw_params_of_exact_segments prefix
        @ [
            Printf.sprintf "(%S, Utopia_route.Params.many (%s_head :: %s_tail))"
              name name name;
          ])
  in
  Printf.sprintf "if %s then\n%s"
    (pattern_check lowercase_pattern)
    (indent_block "  " (render_inner_match original_pattern body))

let render_optional_catch_all_current_branches entry name prefix =
  let prefix_original =
    prefix
    |> List.map (function
      | Static _ -> "_"
      | Param (param, Single) -> param
      | _ -> "_")
  in
  let prefix_lowercase =
    prefix
    |> List.map (function
      | Static segment -> Printf.sprintf "%S" (String.lowercase_ascii segment)
      | Param (_, Single) -> "_"
      | _ -> "_")
  in
  let absent_body =
    render_current_decode entry
      ~field_exprs:
        (current_field_exprs_of_exact_segments prefix
        @ [ Printf.sprintf "%s = None" name ])
      ~raw_params:
        (raw_params_of_exact_segments prefix
        @ [ Printf.sprintf "(%S, Utopia_route.Params.many [])" name ])
  in
  let present_body =
    render_current_decode entry
      ~field_exprs:
        (current_field_exprs_of_exact_segments prefix
        @ [ Printf.sprintf "%s = Some (%s_head :: %s_tail)" name name name ])
      ~raw_params:
        (raw_params_of_exact_segments prefix
        @ [
            Printf.sprintf "(%S, Utopia_route.Params.many (%s_head :: %s_tail))"
              name name name;
          ])
  in
  [
    Printf.sprintf "if %s then\n%s"
      (pattern_check (list_pattern prefix_lowercase))
      (indent_block "  "
         (render_inner_match (list_pattern prefix_original) absent_body));
    Printf.sprintf "if %s then\n%s"
      (pattern_check (cons_pattern (prefix_lowercase @ [ "_" ]) (Some "_")))
      (indent_block "  "
         (render_inner_match
            (cons_pattern
               (prefix_original @ [ name ^ "_head" ])
               (Some (name ^ "_tail")))
            present_body));
  ]

let render_current_branches entry =
  match List.rev (route_segments entry) with
  | Param (name, Catch_all) :: prefix_rev ->
      [ render_catch_all_current_branch entry name (List.rev prefix_rev) ]
  | Param (name, Optional_catch_all) :: prefix_rev ->
      render_optional_catch_all_current_branches entry name
        (List.rev prefix_rev)
  | _ -> [ render_exact_current_branch entry ]

let render_current_constructor entry =
  let fields =
    (if entry.Routes.route_schema_has_params then
       [ Printf.sprintf "params : %s.t" (params_module_ref entry) ]
     else entry.Routes.params |> List.map current_param_field_type)
    |> fun fields ->
    fields
    @ (if entry.Routes.route_schema_has_query then
         [ Printf.sprintf "query : %s.t option" (query_module_ref entry) ]
       else [])
    @
    if entry.Routes.route_schema_has_hash then
      [ Printf.sprintf "hash : %s.t option" (hash_module_ref entry) ]
    else []
  in
  let constructor = constructor_name_of_entry entry in
  if fields = [] then Printf.sprintf "  | %s" constructor
  else Printf.sprintf "  | %s of { %s }" constructor (String.concat "; " fields)

let render_current_module route_entries =
  let constructors =
    match route_entries with
    | [] -> [ "  | No_match" ]
    | _ -> route_entries |> List.map render_current_constructor
  in
  let branches =
    route_entries
    |> List.sort compare_route_specificity
    |> List.concat_map render_current_branches
  in
  let matcher =
    match branches with
    | [] -> "    None"
    | first :: rest ->
        String.concat "\n"
          ([ "    " ^ first ]
          @ (rest |> List.map (fun branch -> "    else " ^ branch))
          @ [ "    else None" ])
  in
  String.concat "\n"
    ([ "module Current = struct"; "  type t =" ]
    @ constructors
    @ [
        "";
        "  let of_route route =";
        "    let path_segments = Utopia_route.path_segments route in";
        "    let lowercase_segments = List.map String.lowercase_ascii \
         path_segments in";
        matcher;
        "end";
        "";
        "let current = Current.of_route";
      ])

let generate route_entries =
  [
    build_tree route_entries |> render_tree "";
    render_current_module route_entries;
  ]
  |> List.filter (fun section -> section <> "")
  |> String.concat "\n\n"
