let app_directory = Names.app_directory

let find_route_conflicts (entries : Routes.route_entry list) =
  let grouped = Hashtbl.create 32 in
  List.iter
    (fun (entry : Routes.route_entry) ->
      let current =
        match Hashtbl.find_opt grouped entry.Routes.conflict_key with
        | Some entries -> entries
        | None -> []
      in
      Hashtbl.replace grouped entry.Routes.conflict_key (entry :: current))
    entries;
  Hashtbl.fold
    (fun _key (grouped_entries : Routes.route_entry list) acc ->
      if List.length grouped_entries > 1 then
        let reversed = List.rev grouped_entries in
        ((List.hd reversed).Routes.route, reversed) :: acc
      else acc)
    grouped []

let source_basename source = Filename.basename source

let preferred_source_for_route route (entries : Routes.route_entry list) =
  let route_directory = if route = "" then "" else route in
  let page_path extension =
    if route_directory = "" then
      Printf.sprintf "%s/page%s" app_directory extension
    else Printf.sprintf "%s/%s/page%s" app_directory route_directory extension
  in
  let preferred_order =
    [ page_path ".ml"; page_path ".re"; page_path ".mlx"; page_path ".md" ]
  in
  let sources =
    List.map
      (fun (entry : Routes.route_entry) -> entry.Routes.source_file)
      entries
  in
  match
    List.find_opt (fun candidate -> List.mem candidate sources) preferred_order
  with
  | Some source -> source
  | None -> List.hd sources

let report_route_conflicts conflicts =
  Printf.eprintf "\n  Route conflicts detected:\n";
  conflicts
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)
  |> List.iter (fun (route, grouped_entries) ->
      let preferred_source = preferred_source_for_route route grouped_entries in
      let ordered_sources =
        grouped_entries
        |> List.map (fun (entry : Routes.route_entry) ->
            entry.Routes.source_file)
        |> List.sort String.compare
      in
      Printf.eprintf "\n    - %s has %d competing page files:\n"
        (Routes.pp_route route)
        (List.length ordered_sources);
      ordered_sources
      |> List.iter (fun source -> Printf.eprintf "        * %s\n" source);
      let alternatives =
        ordered_sources
        |> List.filter (fun source -> source <> preferred_source)
      in
      Printf.eprintf "      Suggested canonical file: %s\n" preferred_source;
      Printf.eprintf "      Rename/remove the others: %s\n"
        (String.concat ", " alternatives);
      let duplicate_names =
        ordered_sources |> List.map source_basename
        |> List.sort_uniq String.compare
      in
      if List.length duplicate_names = 1 then
        Printf.eprintf
          "      Note: these files differ only by directory/casing; choose one \
           canonical path.\n");
  Printf.eprintf "\n  Rule: exactly one source file must map to each route.\n\n";
  Printf.eprintf
    "  Recommended convention:\n\
    \    * app/<route>/page.ml (or app/page.ml for /)\n"

let report_route_parse_errors errors =
  Printf.eprintf "\n  Invalid page declarations:\n";
  errors |> List.iter (fun error -> Printf.eprintf "    - %s\n" error);
  Printf.eprintf
    "\n\
    \  Supported segments:\n\
    \    * [id]\n\
    \    * [...slug]\n\
    \    * [[...slug]]\n\
    \    * route groups: (marketing)\n\
    \    * parallel slots: @slot (ignored for URL path)\n"

let report_route_schema_errors errors =
  Printf.eprintf "\n  Invalid route schema declarations:\n";
  errors |> List.iter (fun error -> Printf.eprintf "    - %s\n" error);
  Printf.eprintf
    "\n\
    \  Route schema files live under routes/ and mirror collected route paths \
     (for example routes/index.re, routes/notes/index.re, \
     routes/users/[id].re).\n"

let starts_with_at text index prefix =
  let prefix_len = String.length prefix in
  index + prefix_len <= String.length text
  && String.sub text index prefix_len = prefix

let extract_params_accesses source =
  let rec read_ident index =
    if index < String.length source && Routes.is_identifier_char source.[index]
    then read_ident (index + 1)
    else index
  in
  let rec loop index acc =
    if index >= String.length source - 6 then List.rev acc
    else if starts_with_at source index "params." then
      let start = index + 7 in
      let stop = read_ident start in
      if stop > start then
        let name = String.sub source start (stop - start) in
        loop stop (name :: acc)
      else loop (index + 1) acc
    else loop (index + 1) acc
  in
  loop 0 [] |> List.sort_uniq String.compare

let has_metadata_export source =
  source |> String.split_on_char '\n'
  |> List.exists (fun line ->
      let trimmed = String.trim line in
      let is_top_level = String.length line > 0 && line.[0] = 'l' in
      is_top_level
      && (starts_with_at trimmed 0 "let metadata "
         || starts_with_at trimmed 0 "let metadata("
         || starts_with_at trimmed 0 "let metadata="
         || String.equal trimmed "let metadata"))

let detect_metadata_for_entry (entry : Routes.route_entry) =
  match entry.Routes.kind with
  | Utopia_types.Markdown_page -> { entry with Routes.has_metadata = false }
  | Utopia_types.Code_page ->
      let source =
        In_channel.with_open_bin entry.Routes.source_file (fun channel ->
            In_channel.input_all channel)
      in
      { entry with Routes.has_metadata = has_metadata_export source }

let detect_static_for_entry (entry : Routes.route_entry) =
  match entry.Routes.kind with
  | Utopia_types.Markdown_page -> { entry with Routes.static = true }
  | Utopia_types.Code_page ->
      let source =
        In_channel.with_open_bin entry.Routes.source_file (fun channel ->
            In_channel.input_all channel)
      in
      let analysis = Analysis.analyze source in
      {
        entry with
        Routes.static = analysis.before_export_origin = None;
        Routes.has_paths = analysis.paths_origin <> None;
        Routes.before_export_origin = analysis.before_export_origin;
        Routes.paths_export_origin = analysis.paths_origin;
      }

let report_missing_paths (entries : Routes.route_entry list) =
  let issues =
    entries
    |> List.filter (fun entry ->
        entry.Routes.static && entry.Routes.params <> []
        && not entry.Routes.has_paths)
  in
  if issues = [] then false
  else (
    Printf.eprintf
      "\n  Static pages with dynamic segments require a paths export:\n";
    issues
    |> List.iter (fun (entry : Routes.route_entry) ->
        Printf.eprintf
          "    - %s is static but has params [%s] without a paths export\n"
          entry.Routes.source_file
          (entry.Routes.params |> List.map fst |> String.concat ", "));
    Printf.eprintf
      "\n\
      \  Fix: add `let paths () = [ [(\"param\", \"value\")] ]` to each file.\n\
      \  Or add `let before request = ...` to make the page dynamic.\n";
    true)

let unknown_params_for_entry (entry : Routes.route_entry) =
  match entry.Routes.kind with
  | Utopia_types.Markdown_page -> []
  | Utopia_types.Code_page ->
      let source =
        In_channel.with_open_bin entry.Routes.source_file (fun channel ->
            In_channel.input_all channel)
      in
      let used = extract_params_accesses source in
      let declared = entry.Routes.params |> List.map fst in
      used |> List.filter (fun name -> not (List.mem name declared))

let report_unknown_param_accesses (entries : Routes.route_entry list) =
  let issues =
    entries
    |> List.filter_map (fun entry ->
        let unknown = unknown_params_for_entry entry in
        if unknown = [] then None else Some (entry, unknown))
  in
  if issues = [] then false
  else (
    Printf.eprintf "\n  Unknown route parameter access detected:\n";
    issues
    |> List.iter (fun ((entry : Routes.route_entry), unknown) ->
        let declared =
          entry.Routes.params |> List.map fst |> function
          | [] -> "(none)"
          | values -> String.concat ", " values
        in
        Printf.eprintf
          "    - %s references unknown params [%s]; allowed params for %s are \
           [%s]\n"
          entry.Routes.source_file
          (String.concat ", " unknown)
          (Routes.pp_route entry.Routes.route)
          declared);
    Printf.eprintf
      "\n\
      \  Fix: rename the param access or update the filename segment to \
       declare it.\n";
    true)
