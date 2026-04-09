let app_directory = Routes.app_directory
let pages_directory = Routes.pages_directory
let api_directory = Routes.api_directory

let generated_files project =
  [
    Utopia_path.generated_dune project;
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
  [ "Utopia.re"; "Utopia_routes.ml" ]
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
  let has_app_directory = Filesystem.directory_exists app_directory in
  let has_pages_directory = Filesystem.directory_exists pages_directory in
  let has_api_directory = Filesystem.directory_exists api_directory in
  if (not has_app_directory) && not has_pages_directory then (
    Printf.eprintf "  Error reading route roots: expected '%s/' or '%s/'\n"
      app_directory pages_directory;
    exit 1)
  else
    let recursive_pages, recursive_api, route_parse_errors, use_app_directory =
      if has_app_directory then (
        let recursive_app =
          match Filesystem.read_files_recursive app_directory with
          | Error (`Page_directory_doesnt_exist _path) -> []
          | Ok files -> files
        in
        let collection = Routes.collect_app_files recursive_app in
        let ignored_legacy_roots =
          [
            (pages_directory, has_pages_directory);
            (api_directory, has_api_directory);
          ]
          |> List.filter_map (fun (root, present) ->
              if present then Some root else None)
        in
        if ignored_legacy_roots <> [] then
          Printf.eprintf
            "  Warning: app/ detected; ignoring legacy route roots: %s\n"
            (String.concat ", " ignored_legacy_roots);
        (collection.page_files, collection.api_files, collection.errors, true))
      else
        let recursive_pages =
          match Filesystem.read_files_recursive pages_directory with
          | Error (`Page_directory_doesnt_exist _path) -> []
          | Ok files -> files
        in
        let recursive_api =
          match Filesystem.read_files_recursive api_directory with
          | Error (`Page_directory_doesnt_exist _path) -> []
          | Ok files -> files
        in
        (recursive_pages, recursive_api, [], false)
    in
    Printf.printf "  Pages: %s\n" (String.concat ", " recursive_pages);
    let route_entries, page_parse_errors =
      if use_app_directory then
        Routes.app_route_entries_of_files recursive_pages
      else Routes.route_entries_of_files recursive_pages
    in
    let route_parse_errors = route_parse_errors @ page_parse_errors in
    let reserved_api_namespace_errors =
      Routes.reserved_api_namespace_errors route_entries
    in
    let api_entries, api_parse_errors =
      if use_app_directory then
        Routes.app_api_route_entries_of_files recursive_api
      else Routes.api_route_entries_of_files recursive_api
    in
    let route_schemas, route_schema_errors = Route_schemas.load route_entries in
    let route_entries = Route_schemas.attach route_entries route_schemas in
    let route_entries =
      route_entries |> List.map Diagnostics.detect_metadata_for_entry
    in
    let route_entries =
      route_entries |> List.map Diagnostics.detect_static_for_entry
    in
    let route_entries, markdown_warnings =
      Routes.attach_markdown_payloads route_entries
    in
    markdown_warnings
    |> List.iter (fun warning -> Printf.eprintf "  Warning: %s\n" warning);
    let conflicts = Diagnostics.find_route_conflicts route_entries in
    let api_conflicts = Routes.find_api_conflicts api_entries in
    let api_param_kind_conflicts =
      Routes.api_param_kind_conflicts api_entries
    in
    let has_unknown_param_accesses =
      Diagnostics.report_unknown_param_accesses route_entries
    in
    let has_missing_paths = Diagnostics.report_missing_paths route_entries in
    let has_errors =
      route_parse_errors <> []
      || reserved_api_namespace_errors <> []
      || api_parse_errors <> [] || route_schema_errors <> [] || conflicts <> []
      || api_conflicts <> []
      || api_param_kind_conflicts <> []
      || has_unknown_param_accesses || has_missing_paths
    in
    if has_errors then (
      if route_parse_errors <> [] then
        Diagnostics.report_route_parse_errors route_parse_errors;
      if reserved_api_namespace_errors <> [] then (
        Printf.eprintf
          "\n  Page routes cannot use the reserved /api namespace:\n";
        reserved_api_namespace_errors
        |> List.iter (fun message -> Printf.eprintf "    - %s\n" message));
      if api_parse_errors <> [] then (
        Printf.eprintf "\n  Invalid API declarations:\n";
        api_parse_errors
        |> List.iter (fun error -> Printf.eprintf "    - %s\n" error));
      if route_schema_errors <> [] then
        Diagnostics.report_route_schema_errors route_schema_errors;
      if conflicts <> [] then Diagnostics.report_route_conflicts conflicts;
      if api_conflicts <> [] then (
        Printf.eprintf "\n  API route conflicts detected:\n";
        api_conflicts
        |> List.iter (fun grouped_entries ->
            let route = (List.hd grouped_entries).Routes.route in
            Printf.eprintf "\n    - %s has %d competing API files:\n"
              (Routes.pp_route route)
              (List.length grouped_entries);
            grouped_entries
            |> List.map (fun (entry : Routes.api_route_entry) ->
                entry.source_file)
            |> List.sort String.compare
            |> List.iter (fun source -> Printf.eprintf "        * %s\n" source)));
      if api_param_kind_conflicts <> [] then (
        Printf.eprintf "\n  Invalid API param accessor shapes:\n";
        api_param_kind_conflicts
        |> List.iter (fun error -> Printf.eprintf "    - %s\n" error));
      exit 1)
    else (
      print_endline "\n  Generating rules\n";
      let source_root =
        if use_app_directory then app_directory else pages_directory
      in
      let api_root =
        if use_app_directory then Routes.app_api_directory else api_directory
      in
      (* Scan pages for client components and compute melange optimization *)
      let client_component_pages, melange_lib_modules =
        let shared_lib_dir =
          Fpath.to_string Utopia_path.shared_lib_directory_name
        in
        let lib_files = Build_inputs.shared_lib_files_for_build () in
        let lib_module_map = Client_graph.build_lib_module_map lib_files in
        let pages_with_cc = ref [] in
        let all_refs = ref Client_component_scan.StringSet.empty in
        recursive_pages
        |> List.iter (fun relative_file ->
            let source_file = Filename.concat source_root relative_file in
            if Sys.file_exists source_file then
              let source =
                In_channel.with_open_bin source_file (fun ch ->
                    In_channel.input_all ch)
              in
              let result = Client_component_scan.extract_client_code source in
              if result.has_client_components then (
                pages_with_cc := relative_file :: !pages_with_cc;
                all_refs :=
                  Client_component_scan.StringSet.union !all_refs
                    result.module_references));
        let melange_lib_closure =
          Client_graph.compute_lib_closure ~seed_refs:!all_refs ~lib_module_map
            ~shared_lib_directory:shared_lib_dir
        in
        let cc_set =
          List.fold_left
            (fun acc f -> Client_component_scan.StringSet.add f acc)
            Client_component_scan.StringSet.empty !pages_with_cc
        in
        (cc_set, melange_lib_closure)
      in
      let is_client_component_page relative_file =
        Client_component_scan.StringSet.mem relative_file client_component_pages
      in
      let is_melange_lib_module module_name =
        Client_component_scan.StringSet.mem module_name melange_lib_modules
      in
      Printf.printf "  Client component pages: %d\n"
        (Client_component_scan.StringSet.cardinal client_component_pages);
      Printf.printf "  Melange lib modules: %d\n"
        (Client_component_scan.StringSet.cardinal melange_lib_modules);
      let source_support_dune =
        Generated_source_dune.generate ~source_root recursive_pages
          route_entries
      in
      let _dev_mode = build_mode = Esbuild.development in
      (* TODO: pass ~dev_mode once dev overlay dune rules are ready *)
      let runtime_dune =
        Generated_dune.generate ~source_root ~api_root ~is_client_component_page
          ~is_melange_lib_module recursive_pages recursive_api route_entries
          api_entries
      in
      let dune_rules =
        [ source_support_dune; runtime_dune ]
        |> List.filter (fun value -> String.trim value <> "")
        |> String.concat "\n\n"
      in
      let esbuild_paths = Esbuild.generate_paths ~build_mode () in
      let generated_routes =
        Generated_routes.generate route_entries api_entries
      in
      let server_main = Server_main.generate route_entries api_entries in
      print_endline dune_rules;
      Filesystem.write_to_file
        (file_ref_path (Utopia_path.generated_dune project))
        dune_rules;
      Filesystem.write_to_file
        (file_ref_path (Utopia_path.generated_esbuild_paths project))
        esbuild_paths;
      Filesystem.write_to_file
        (file_ref_path (Utopia_path.generated_routes_source project))
        generated_routes;
      Filesystem.write_to_file
        (file_ref_path (Utopia_path.generated_server_source project))
        server_main)

let extract_client_main source_file =
  let source =
    In_channel.with_open_bin source_file (fun ch -> In_channel.input_all ch)
  in
  let result = Client_component_scan.extract_client_code source in
  if result.has_client_components then print_string result.extracted_source

let () =
  let argv = Array.to_list Sys.argv |> List.tl in
  match argv with
  | "--extract-client" :: source_file :: _ -> extract_client_main source_file
  | _ -> run ~build_mode:(parse_build_mode argv)
