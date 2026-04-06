open Utopia_types

let pages_directory = "pages"

type route_entry = {
  route : string;
  matcher : string;
  conflict_key : string;
  params : (string * param_kind) list;
  layouts : string list;
  kind : page_kind;
  source_file : string;
  has_metadata : bool;
  static : bool;
  has_static_paths : bool;
  route_schema_source : string option;
  route_schema_has_params : bool;
  route_schema_module : string option;
  route_schema_has_query : bool;
  route_schema_has_hash : bool;
}

let is_identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_valid_identifier name =
  String.length name > 0
  && String.for_all is_identifier_char name
  && match name.[0] with '0' .. '9' -> false | _ -> true

let parse_param_segment segment =
  let len = String.length segment in
  if len < 3 || segment.[0] <> '[' || segment.[len - 1] <> ']' then
    Ok (Static segment)
  else
    let inner = String.sub segment 1 (len - 2) in
    let inner_len = String.length inner in
    if inner_len >= 5 && String.sub inner 0 4 = "[...]" then
      Error (Printf.sprintf "Invalid segment '%s'" segment)
    else if
      inner_len >= 6
      && inner.[0] = '['
      && inner.[inner_len - 1] = ']'
      && String.sub inner 1 3 = "..."
    then
      let name = String.sub inner 4 (inner_len - 5) in
      if is_valid_identifier name then Ok (Param (name, Optional_catch_all))
      else
        Error (Printf.sprintf "Invalid optional catch-all parameter '%s'" name)
    else if inner_len >= 4 && String.sub inner 0 3 = "..." then
      let name = String.sub inner 3 (inner_len - 3) in
      if is_valid_identifier name then Ok (Param (name, Catch_all))
      else Error (Printf.sprintf "Invalid catch-all parameter '%s'" name)
    else if is_valid_identifier inner then Ok (Param (inner, Single))
    else Error (Printf.sprintf "Invalid parameter '%s'" inner)

let is_group_segment segment =
  let len = String.length segment in
  len >= 2 && segment.[0] = '(' && segment.[len - 1] = ')'

let is_parallel_slot_segment segment =
  String.length segment > 0 && segment.[0] = '@'

let normalize_path_segments path_without_extension =
  let segments = String.split_on_char '/' path_without_extension in
  let visible_segments =
    segments
    |> List.filter (fun segment ->
        segment <> ""
        && (not (is_group_segment segment))
        && not (is_parallel_slot_segment segment))
  in
  match List.rev visible_segments with
  | "index" :: rest -> List.rev rest
  | _ -> visible_segments

let render_route_segment = function
  | Static segment -> String.lowercase_ascii segment
  | Param (name, Single) -> Printf.sprintf "[%s]" name
  | Param (name, Catch_all) -> Printf.sprintf "[...%s]" name
  | Param (name, Optional_catch_all) -> Printf.sprintf "[[...%s]]" name

let render_matcher_segment = function
  | Static segment -> String.lowercase_ascii segment
  | Param (name, Single) -> ":" ^ name
  | Param (name, Catch_all) -> "*" ^ name
  | Param (name, Optional_catch_all) -> "**" ^ name

let render_conflict_segment = function
  | Static segment -> String.lowercase_ascii segment
  | Param (_name, Single) -> ":"
  | Param (_name, Catch_all) -> "*"
  | Param (_name, Optional_catch_all) -> "**"

let route_of_segments segments =
  segments |> List.map render_route_segment |> String.concat "/"

let matcher_of_segments segments =
  segments |> List.map render_matcher_segment |> String.concat "/"

let conflict_key_of_segments segments =
  segments |> List.map render_conflict_segment |> String.concat "/"

let validate_segment_rules segments =
  let has_non_terminal_spread =
    segments
    |> List.mapi (fun index segment -> (index, segment))
    |> List.exists (fun (index, segment) ->
        let is_last = index = List.length segments - 1 in
        match segment with
        | Param (_, (Catch_all | Optional_catch_all)) -> not is_last
        | _ -> false)
  in
  if has_non_terminal_spread then
    Error "Catch-all and optional catch-all segments must be the last segment"
  else
    let names =
      segments
      |> List.filter_map (function
        | Param (name, kind) -> Some (name, kind)
        | _ -> None)
    in
    let unique_names = names |> List.map fst |> List.sort_uniq String.compare in
    if List.length unique_names <> List.length names then
      Error "Route has duplicated parameter names"
    else Ok names

let relative_directory file =
  let dir = Filename.dirname file in
  if dir = "." then "" else dir

let basename_without_extension file =
  file |> Filename.basename |> Filename.remove_extension

let is_layout_file file = basename_without_extension file = "layout"

let ancestor_directories dir =
  if dir = "" then [ "" ]
  else
    let parts = String.split_on_char '/' dir in
    let rec build prefixes remaining =
      match remaining with
      | [] -> prefixes
      | segment :: rest ->
          let next =
            match List.rev prefixes with
            | [] | "" :: _ -> segment
            | prev :: _ -> Filename.concat prev segment
          in
          build (prefixes @ [ next ]) rest
    in
    build [ "" ] parts

let route_entry_of_file file kind =
  let without_extension = Filename.remove_extension file in
  let source_file = Filename.concat pages_directory file in
  let normalized_segments = normalize_path_segments without_extension in
  let parsed_segments_result =
    normalized_segments
    |> List.map parse_param_segment
    |> List.fold_left
         (fun acc result ->
           match (acc, result) with
           | (Error _ as error), _ -> error
           | Ok _, Error message -> Error message
           | Ok segments, Ok segment -> Ok (segment :: segments))
         (Ok [])
    |> Result.map List.rev
  in
  match parsed_segments_result with
  | Error message ->
      Error
        (Printf.sprintf "In %s: %s (segment path: %s)" source_file message
           without_extension)
  | Ok segments -> (
      match validate_segment_rules segments with
      | Error message ->
          Error
            (Printf.sprintf "In %s: %s (segment path: %s)" source_file message
               without_extension)
      | Ok params ->
          Ok
            {
              route = route_of_segments segments;
              matcher = matcher_of_segments segments;
              conflict_key = conflict_key_of_segments segments;
              params;
              layouts = [];
              kind;
              source_file;
              has_metadata = false;
              static = false;
              has_static_paths = false;
              route_schema_source = None;
              route_schema_has_params = false;
              route_schema_module = None;
              route_schema_has_query = false;
              route_schema_has_hash = false;
            })

let collect_layouts files =
  let layout_by_dir = Hashtbl.create 32 in
  let errors = ref [] in
  files
  |> List.iter (fun file ->
      let extension = Filename.extension file in
      match (kind_of_extension extension, is_layout_file file) with
      | Some Code_page, true -> (
          let dir = relative_directory file in
          let source_file = Filename.concat pages_directory file in
          let previous = Hashtbl.find_opt layout_by_dir dir in
          match previous with
          | None -> Hashtbl.replace layout_by_dir dir source_file
          | Some existing ->
              errors :=
                Printf.sprintf
                  "Layout conflict in pages/%s: both %s and %s define a layout"
                  dir existing source_file
                :: !errors)
      | _ -> ());
  (layout_by_dir, List.rev !errors)

let layouts_for_file layout_by_dir file =
  let dir = relative_directory file in
  ancestor_directories dir
  |> List.filter_map (fun segment -> Hashtbl.find_opt layout_by_dir segment)

let route_entries_of_files files =
  let layout_by_dir, layout_errors = collect_layouts files in
  files
  |> List.fold_left
       (fun (entries, errors) file ->
         let extension = Filename.extension file in
         match kind_of_extension extension with
         | None -> (entries, errors)
         | Some Code_page when is_layout_file file -> (entries, errors)
         | Some kind -> (
             match route_entry_of_file file kind with
             | Ok entry ->
                 let entry =
                   { entry with layouts = layouts_for_file layout_by_dir file }
                 in
                 (entry :: entries, errors)
             | Error message -> (entries, message :: errors)))
       ([], layout_errors)
  |> fun (entries, errors) -> (List.rev entries, List.rev errors)

let pp_route route = if route = "" then "/" else "/" ^ route

let strip_pages_prefix source_file =
  let prefix = pages_directory ^ "/" in
  let prefix_len = String.length prefix in
  if
    String.length source_file >= prefix_len
    && String.sub source_file 0 prefix_len = prefix
  then String.sub source_file prefix_len (String.length source_file - prefix_len)
  else source_file

let layout_route_path source_file =
  let relative = strip_pages_prefix source_file |> Filename.remove_extension in
  let dir = relative_directory relative in
  let segments =
    dir |> normalize_path_segments
    |> List.map (fun segment ->
        match parse_param_segment segment with
        | Ok parsed -> parsed
        | Error message -> invalid_arg message)
  in
  segments |> matcher_of_segments |> pp_route
