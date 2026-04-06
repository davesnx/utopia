let pages_directory = "pages"

let generated_files project =
  [
    Utopia_path.generated_dune project;
    Utopia_path.routes_manifest project;
    Utopia_path.generated_esbuild_paths project;
    Utopia_path.generated_routes_source project;
    Utopia_path.generated_server_source project;
  ]

let file_ref_path file_ref =
  file_ref |> Utopia_path.file_path |> Utopia_path.to_string

let clear_generated_files project =
  generated_files project
  |> List.iter (fun file_ref ->
      file_ref |> file_ref_path |> Filesystem.remove_file_if_exists);
  let generated_dir =
    Utopia_path.project_generated_directory project |> Utopia_path.to_string
  in
  [ "Utopia.re"; "Utopia_routes.ml"; "support/Utopia_call_server.re" ]
  |> List.iter (fun relative ->
      Filename.concat generated_dir relative |> Filesystem.remove_file_if_exists)

let parse_build_mode argv =
  let rec loop (mode : Esbuild.build_mode) = function
    | [] -> mode
    | [ "--mode" ] ->
        Printf.eprintf
          "Missing value for --mode (expected development|production)\n%!";
        exit 1
    | "--mode" :: value :: rest ->
        let mode =
          match String.lowercase_ascii value with
          | "development" -> Esbuild.development
          | "production" -> Esbuild.production
          | _ ->
              Printf.eprintf
                "Invalid --mode value '%s' (expected development|production)\n\
                 %!"
                value;
              exit 1
        in
        loop mode rest
    | unknown :: _ ->
        Printf.eprintf "Unknown compiler flag: %s\n%!" unknown;
        exit 1
  in
  loop Esbuild.development argv

let run ~build_mode =
  print_endline "\n\nutopia compiler";
  let project = Project.project_paths () in
  Filesystem.ensure_directory
    (Utopia_path.project_generated_directory project |> Utopia_path.to_string);
  clear_generated_files project;
  Runtime_files.copy_runtime_files ();
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
      let route_entries =
        route_entries |> List.map Diagnostics.detect_static_for_entry
      in
      let conflicts = Diagnostics.find_route_conflicts route_entries in
      let has_unknown_param_accesses =
        Diagnostics.report_unknown_param_accesses route_entries
      in
      let has_missing_static_paths =
        Diagnostics.report_missing_static_paths route_entries
      in
      let has_errors =
        route_parse_errors <> [] || route_schema_errors <> [] || conflicts <> []
        || has_unknown_param_accesses || has_missing_static_paths
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
        let source_support_dune =
          Generated_source_dune.generate recursive_pages route_entries
        in
        let runtime_dune =
          Generated_dune.generate recursive_pages route_entries
        in
        let dune_rules =
          [ source_support_dune; runtime_dune ]
          |> List.filter (fun value -> String.trim value <> "")
          |> String.concat "\n\n"
        in
        let route_manifest = Manifest.generate route_entries in
        let esbuild_paths = Esbuild.generate_paths ~build_mode () in
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
          (file_ref_path (Utopia_path.generated_esbuild_paths project))
          esbuild_paths;
        Filesystem.write_to_file
          (file_ref_path (Utopia_path.generated_routes_source project))
          generated_routes;
        Filesystem.write_to_file
          (file_ref_path (Utopia_path.generated_server_source project))
          server_main)

let () = run ~build_mode:(parse_build_mode (Array.to_list Sys.argv |> List.tl))
