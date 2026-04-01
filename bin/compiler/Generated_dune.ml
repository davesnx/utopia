let generated_library_module = "Lib"
let client_entry_target = Utopia_runtime.client_entry_melange_target_name
let generated_server_name = "server_main"
let native_subdir_name = "native"
let melange_target_name = "target"
let pages_directory = "pages"
let shared_lib_directory = Fpath.to_string Utopia_path.shared_lib_directory_name
let generated_routes_source = "Utopia_routes.ml"
let generated_routes_module = "Utopia_routes"

let generated_esbuild_config_path () =
  Utopia_path.generated_esbuild_config (Project.project_paths ())
  |> Utopia_path.file_display

let melange_libraries =
  [
    "reason-react";
    "melange-webapi";
    "melange-fetch";
    "server-reason-react.runtime";
    "server-reason-react.url_js";
    "melange-json";
  ]

let native_library_libraries =
  [
    "utopia.markdown_runtime";
    "server-reason-react.runtime";
    "server-reason-react.react";
    "server-reason-react.reactDom";
    "server-reason-react.fetch";
    "server-reason-react.url_native";
    "server-reason-react.webapi";
    "melange-json";
  ]

let native_library_flags = [ ":standard"; "-w"; "-26-27-39" ]
let server_executable_modules = [ generated_server_name; "Utopia_server" ]

let server_executable_libraries pages_library_name =
  [
    pages_library_name;
    "utopia.markdown_runtime";
    "cmarkit";
    "server-reason-react.react";
    "server-reason-react.reactDom";
    "unix";
    "logs.fmt";
    "fmt";
    "dream";
    "lwt";
    "lwt.unix";
  ]

let melange_preprocess_pps =
  [
    "server-reason-react.browser_ppx";
    "-js";
    "server-reason-react.ppx";
    "-shared-folder-prefix=_utopia/";
    "-melange";
    "melange.ppx";
    "reason-react-ppx";
    "melange-json.ppx";
  ]

let native_preprocess_pps =
  [
    "server-reason-react.ppx";
    "-shared-folder-prefix=_utopia/native/";
    "server-reason-react.browser_ppx";
    "server-reason-react.melange_ppx";
    "melange-json-native.ppx";
  ]

let open_statement extension module_name =
  if extension = ".re" then Printf.sprintf "open! %s;" module_name
  else Printf.sprintf "open! %s" module_name

let run program args =
  let open Dune_sexp in
  form "run" (atom program :: List.map atom args)

let rule ?(deps = []) ?target ?alias ~action () =
  let open Dune_sexp in
  let deps_field = if deps = [] then [] else [ field "deps" deps ] in
  let location_fields =
    match (target, alias) with
    | Some target, None -> [ field_atom "target" target ]
    | None, Some alias -> [ field_atom "alias" alias ]
    | Some target, Some alias ->
        [ field_atom "alias" alias; field_atom "target" target ]
    | None, None -> invalid_arg "rule requires a target or alias"
  in
  Dune_sexp.form "rule"
    (deps_field @ location_fields @ [ Dune_sexp.field "action" [ action ] ])

let copy_rule ~deps ~target ~prelude_lines =
  let open Dune_sexp in
  let prelude_actions =
    prelude_lines |> List.map (fun line -> form "echo" [ atom (line ^ "\n") ])
  in
  rule
    ~deps:[ atom deps ]
    ~target
    ~action:
      (form "with-stdout-to"
         [
           atom "%{target}";
           form "progn" (prelude_actions @ [ run "cat" [ "%{deps}" ] ]);
         ])
    ()

let write_file_rule ~target ~content =
  let open Dune_sexp in
  rule ~target ~action:(form "write-file" [ atom "%{target}"; atom content ]) ()

let copy_dependency_rule ~deps ~target =
  rule
    ~deps:[ Dune_sexp.atom deps ]
    ~target
    ~action:(run "cp" [ "%{deps}"; "%{target}" ])
    ()

let root_page_dependency relative_file =
  Printf.sprintf "../%s" (Filename.concat pages_directory relative_file)

let native_page_dependency relative_file =
  Printf.sprintf "../../%s" (Filename.concat pages_directory relative_file)

let root_shared_lib_dependency source_file =
  Printf.sprintf "../%s" (Filename.concat shared_lib_directory source_file)

let native_shared_lib_dependency source_file =
  Printf.sprintf "../../%s" (Filename.concat shared_lib_directory source_file)

let root_route_schema_dependency source_file =
  Printf.sprintf "../%s" source_file

let native_route_schema_dependency source_file =
  Printf.sprintf "../../%s" source_file

let source_project_root () =
  let project_path = Project.workspace_relative_project_path () in
  if project_path = "" then "%{workspace_root}"
  else Printf.sprintf "%%{workspace_root}/%s" project_path

let generate files route_entries =
  let open Dune_sexp in
  let pages_library_name = Project.generated_pages_library_name () in
  let shared_lib_files : Build_inputs.shared_lib_file list =
    Build_inputs.shared_lib_files_for_build ()
  in
  let route_schema_files : Build_inputs.route_schema_file list =
    Build_inputs.route_schema_files_for_build route_entries
  in
  let has_shared_lib = Build_inputs.has_shared_lib () in
  let code_files = Build_inputs.code_files_for_build files in
  let markdown_files = Build_inputs.markdown_files_for_build files in
  let melange_copy_rules =
    code_files
    |> List.map (fun (file : Build_inputs.compiled_code_file) ->
        let prelude_lines =
          let opens =
            [ open_statement file.extension "Melange_json.Primitives" ]
          in
          if has_shared_lib then
            opens @ [ open_statement file.extension generated_library_module ]
          else opens
        in
        copy_rule
          ~deps:(root_page_dependency file.relative_file)
          ~target:(Printf.sprintf "%s%s" file.base_name file.extension)
          ~prelude_lines)
  in
  let native_copy_rules =
    code_files
    |> List.map (fun (file : Build_inputs.compiled_code_file) ->
        let prelude_lines =
          let opens =
            [ open_statement file.extension "Melange_json.Primitives" ]
          in
          if has_shared_lib then
            opens @ [ open_statement file.extension generated_library_module ]
          else opens
        in
        copy_rule
          ~deps:(native_page_dependency file.relative_file)
          ~target:(Printf.sprintf "%s%s" file.base_name file.extension)
          ~prelude_lines)
  in
  let client_entry_rule =
    copy_dependency_rule
      ~deps:(Utopia_runtime.target_name Utopia_runtime.client_entry_source_file)
      ~target:client_entry_target
  in
  let react_server_dom_runtime_rule =
    copy_dependency_rule
      ~deps:
        "%{lib:server-reason-react.react-server-dom-esbuild:ReactServerDOMEsbuild.js}"
      ~target:"ReactServerDOMEsbuild.js"
  in
  let melange_shared_lib_copy_rules =
    if not has_shared_lib then []
    else
      shared_lib_files
      |> List.concat_map (fun (file : Build_inputs.shared_lib_file) ->
          [
            copy_rule
              ~deps:(root_shared_lib_dependency file.source_file)
              ~target:(Build_inputs.compiled_shared_lib_target file)
              ~prelude_lines:
                [
                  open_statement file.extension "Melange_json.Primitives";
                  open_statement file.extension generated_library_module;
                ];
            copy_rule
              ~deps:(root_shared_lib_dependency file.source_file)
              ~target:(Build_inputs.wrapped_shared_lib_target file)
              ~prelude_lines:
                [ open_statement file.extension "Melange_json.Primitives" ];
          ])
  in
  let native_shared_lib_copy_rules =
    if not has_shared_lib then []
    else
      shared_lib_files
      |> List.concat_map (fun (file : Build_inputs.shared_lib_file) ->
          [
            copy_rule
              ~deps:(native_shared_lib_dependency file.source_file)
              ~target:(Build_inputs.compiled_shared_lib_target file)
              ~prelude_lines:
                [
                  open_statement file.extension "Melange_json.Primitives";
                  open_statement file.extension generated_library_module;
                ];
            copy_rule
              ~deps:(native_shared_lib_dependency file.source_file)
              ~target:(Build_inputs.wrapped_shared_lib_target file)
              ~prelude_lines:
                [ open_statement file.extension "Melange_json.Primitives" ];
          ])
  in
  let melange_shared_lib_namespace_rule =
    if not has_shared_lib then []
    else
      let aliases =
        shared_lib_files
        |> List.map (fun (file : Build_inputs.shared_lib_file) ->
            Printf.sprintf "module %s = %s" file.module_name
              (Build_inputs.wrapped_shared_lib_module_name file))
        |> String.concat "\n"
      in
      [ write_file_rule ~target:"Lib.re" ~content:aliases ]
  in
  let native_shared_lib_namespace_rule =
    if not has_shared_lib then []
    else
      let aliases =
        shared_lib_files
        |> List.map (fun (file : Build_inputs.shared_lib_file) ->
            Printf.sprintf "module %s = %s" file.module_name
              (Build_inputs.wrapped_shared_lib_module_name file))
        |> String.concat "\n"
      in
      [ write_file_rule ~target:"Lib.re" ~content:aliases ]
  in
  let melange_route_schema_copy_rules =
    route_schema_files
    |> List.map (fun (file : Build_inputs.route_schema_file) ->
        copy_dependency_rule
          ~deps:(root_route_schema_dependency file.source_file)
          ~target:(Build_inputs.route_schema_target file))
  in
  let native_route_schema_copy_rules =
    route_schema_files
    |> List.map (fun (file : Build_inputs.route_schema_file) ->
        copy_dependency_rule
          ~deps:(native_route_schema_dependency file.source_file)
          ~target:(Build_inputs.route_schema_target file))
  in
  let generated_routes_native_copy_rule =
    copy_dependency_rule
      ~deps:("../" ^ generated_routes_source)
      ~target:generated_routes_source
  in
  let markdown_rules =
    markdown_files
    |> List.map (fun (file : Build_inputs.compiled_code_file) ->
        rule
          ~deps:[ atom (root_page_dependency file.relative_file) ]
          ~target:(Printf.sprintf "%s.html" file.base_name)
          ~action:
            (form "with-stdout-to"
               [
                 atom "%{target}";
                 form "with-stdin-from"
                   [ atom "%{deps}"; run "%{bin:utopia.markdown}" [] ];
               ])
          ())
  in
  let melange_rule =
    let page_modules =
      code_files
      |> List.map (fun (file : Build_inputs.compiled_code_file) ->
          file.base_name)
    in
    let route_schema_modules =
      route_schema_files
      |> List.map (fun (file : Build_inputs.route_schema_file) ->
          file.module_name)
    in
    let modules =
      if has_shared_lib then
        let lib_modules =
          shared_lib_files
          |> List.map (fun (file : Build_inputs.shared_lib_file) ->
              Build_inputs.compiled_shared_lib_module_name file)
        in
        let wrapped_modules =
          shared_lib_files
          |> List.map Build_inputs.wrapped_shared_lib_module_name
        in
        generated_library_module
        :: (wrapped_modules @ lib_modules @ route_schema_modules @ page_modules
           @ (generated_routes_module :: Utopia_runtime.melange_module_names)
           @ [ Utopia_runtime.client_entry_melange_module_name ])
      else
        page_modules @ route_schema_modules
        @ (generated_routes_module :: Utopia_runtime.melange_module_names)
        @ [ Utopia_runtime.client_entry_melange_module_name ]
    in
    form "melange.emit"
      [
        field_atom "target" melange_target_name;
        field_atoms "module_systems" [ "es6" ];
        field_atoms "modules" modules;
        field_atoms "libraries" melange_libraries;
        field "preprocess" [ field_atoms "pps" melange_preprocess_pps ];
      ]
  in
  let native_library_rule =
    let page_modules =
      code_files
      |> List.map (fun (file : Build_inputs.compiled_code_file) ->
          file.base_name)
    in
    let route_schema_modules =
      route_schema_files
      |> List.map (fun (file : Build_inputs.route_schema_file) ->
          file.module_name)
    in
    let modules =
      if has_shared_lib then
        let lib_modules =
          shared_lib_files
          |> List.map (fun (file : Build_inputs.shared_lib_file) ->
              Build_inputs.compiled_shared_lib_module_name file)
        in
        let wrapped_modules =
          shared_lib_files
          |> List.map Build_inputs.wrapped_shared_lib_module_name
        in
        Utopia_runtime.native_module_names
        @ generated_routes_module :: generated_library_module
          :: (wrapped_modules @ lib_modules @ route_schema_modules
            @ page_modules)
      else
        Utopia_runtime.native_module_names
        @ (generated_routes_module :: (route_schema_modules @ page_modules))
    in
    form "library"
      [
        field_atom "name" pages_library_name;
        field_atom "wrapped" "false";
        field_atoms "flags" native_library_flags;
        field_atoms "modules" modules;
        field_atoms "libraries" native_library_libraries;
        field "preprocess" [ field_atoms "pps" native_preprocess_pps ];
      ]
  in
  let native_subdir_rule =
    form "subdir"
      (atom native_subdir_name
      :: List.concat
           [
             native_copy_rules;
             native_shared_lib_copy_rules;
             native_route_schema_copy_rules;
             native_shared_lib_namespace_rule;
             [ generated_routes_native_copy_rule ];
             [ native_library_rule ];
           ])
  in
  let esbuild_rule =
    rule ~alias:"esbuild"
      ~deps:
        [
          form "alias" [ atom "melange" ];
          atom "esbuild.config.mjs";
          atom "paths.mjs";
          atom "../package.json";
        ]
      ~action:
        (form "chdir"
           [
             atom (source_project_root ());
             run "node" [ generated_esbuild_config_path () ];
           ])
      ()
  in
  let server_executable =
    form "executable"
      [
        field_atom "name" generated_server_name;
        field_atoms "modules" server_executable_modules;
        field_atoms "libraries" (server_executable_libraries pages_library_name);
      ]
  in
  render_many
    (List.concat
       [
         melange_copy_rules;
         [ client_entry_rule; react_server_dom_runtime_rule ];
         melange_shared_lib_copy_rules;
         melange_route_schema_copy_rules;
         melange_shared_lib_namespace_rule;
         [ melange_rule ];
         markdown_rules;
         [ native_subdir_rule; esbuild_rule; server_executable ];
       ])
