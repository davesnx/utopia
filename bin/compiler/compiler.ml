let pages_directory = "pages"

let generated_files project =
  [
    Utopia_path.generated_dune project;
    Utopia_path.routes_manifest project;
    Utopia_path.generated_esbuild_config project;
    Utopia_path.generated_routes_source project;
    Utopia_path.generated_server_source project;
  ]

let file_ref_path file_ref =
  file_ref |> Utopia_path.file_path |> Utopia_path.to_string

let clear_generated_files project =
  generated_files project
  |> List.iter (fun file_ref ->
      file_ref |> file_ref_path |> Filesystem.remove_file_if_exists)

let run () =
  print_endline "\n\nUtopia compiler";
  let project = Project.project_paths () in
  Filesystem.ensure_directory
    (Utopia_path.project_generated_directory project |> Utopia_path.to_string);
  Runtime_files.copy_runtime_files ();
  clear_generated_files project;
  match Filesystem.read_files pages_directory with
  | Error (`Page_directory_doesnt_exist path) ->
      Printf.eprintf "  Error reading the '%s' directory\n" path
  | Ok pages ->
      Printf.printf "  Pages: %s\n" (String.concat ", " (Array.to_list pages));
      let recursive_pages =
        match Filesystem.read_files_recursive pages_directory with
        | Error (`Page_directory_doesnt_exist _path) -> []
        | Ok files -> files
      in
      let route_entries, route_parse_errors =
        Routes.route_entries_of_files recursive_pages
      in
      let route_schemas, route_schema_errors =
        Route_schemas.load route_entries
      in
      let route_entries = Route_schemas.attach route_entries route_schemas in
      let route_entries =
        route_entries |> List.map Diagnostics.detect_metadata_for_entry
      in
      let conflicts = Diagnostics.find_route_conflicts route_entries in
      let has_unknown_param_accesses =
        Diagnostics.report_unknown_param_accesses route_entries
      in
      let has_errors =
        route_parse_errors <> [] || route_schema_errors <> [] || conflicts <> []
        || has_unknown_param_accesses
      in
      if has_errors then (
        if route_parse_errors <> [] then
          Diagnostics.report_route_parse_errors route_parse_errors;
        if route_schema_errors <> [] then
          Diagnostics.report_route_schema_errors route_schema_errors;
        if conflicts <> [] then Diagnostics.report_route_conflicts conflicts;
        exit 1)
      else (
        print_endline "\n  Generating rules\n";
        let dune_rules =
          Generated_dune.generate recursive_pages route_entries
        in
        let route_manifest = Manifest.generate route_entries in
        let esbuild_config = Esbuild.generate () in
        let generated_routes = Generated_routes.generate route_entries in
        let server_main = Server_main.generate route_entries in
        print_endline dune_rules;
        print_endline "\n  Generating route manifest\n";
        print_endline route_manifest;
        Filesystem.write_to_file
          (file_ref_path (Utopia_path.generated_dune project))
          dune_rules;
        Filesystem.write_to_file
          (file_ref_path (Utopia_path.routes_manifest project))
          (route_manifest ^ "\n");
        Filesystem.write_to_file
          (file_ref_path (Utopia_path.generated_esbuild_config project))
          esbuild_config;
        Filesystem.write_to_file
          (file_ref_path (Utopia_path.generated_routes_source project))
          generated_routes;
        Filesystem.write_to_file
          (file_ref_path (Utopia_path.generated_server_source project))
          server_main)

let () = run ()
