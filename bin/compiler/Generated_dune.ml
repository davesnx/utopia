let generated_library_module = "Lib"
let generated_app_local_module = "App_local"
let generated_subdir_name = "_utopia"
let client_entry_target = Utopia_runtime.client_entry_melange_target_name
let generated_server_name = "server_main"
let native_subdir_name = "native"
let melange_target_name = "target"
let shared_lib_directory = Fpath.to_string Utopia_path.shared_lib_directory_name
let generated_routes_source = "Routes.ml"
let generated_routes_module = "Routes"
let generated_routes_client_source = "Routes_client.ml"
let generated_routes_client_module = "Routes_client"
let generated_routes_server_module = "Routes_server"

let generated_esbuild_config_path () =
  Utopia_path.generated_esbuild_config (Project.project_paths ())
  |> Utopia_path.file_display

let melange_libraries =
  [
    (* Only melange-compatible libraries belong here. The full "utopia"
       library and the generated routes library are server-only — they
       transitively pull in dream, unix, lwt, cmarkit, etc. The
       utopia.client sub-library exposes only the melange-safe modules
       (Utopia_route, Utopia_router, Utopia_call_server, etc.). *)
    "reason-react";
    "melange-webapi";
    "melange-fetch";
    "server-reason-react.runtime";
    "server-reason-react.url_js";
    "melange-json";
  ]

let native_library_libraries generated_utopia_library_name =
  [
    generated_utopia_library_name;
    "utopia";
    "utopia.markdown_runtime";
    "server-reason-react.runtime";
    "server-reason-react.react";
    "server-reason-react.reactDom";
    "server-reason-react.fetch";
    "server-reason-react.url_native";
    "server-reason-react.webapi";
    "melange-json";
  ]

let native_api_library_libraries generated_utopia_library_name =
  [ generated_utopia_library_name; "utopia"; "dream"; "lwt"; "lwt.unix" ]

let native_library_flags = [ ":standard"; "-w"; "-26-27-39" ]
let server_executable_modules = [ generated_server_name ]

let server_executable_libraries ~pages_library_name:_ ~api_library_name:_
    ~generated_routes_server_library_name ~generated_utopia_library_name:_
    ~has_api_library:_ =
  [ generated_routes_server_library_name; "utopia" ]

let routes_server_libraries ~pages_library_name ~api_library_name
    ~generated_utopia_library_name ~has_api_library =
  let base = [ pages_library_name; generated_utopia_library_name; "utopia" ] in
  if has_api_library then base @ [ api_library_name ] else base

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

let open_flags modules_to_open =
  modules_to_open |> List.concat_map (fun m -> [ "-open"; m ])

let line_directive source_path =
  Printf.sprintf "# 1 \"%s\"" (String.escaped source_path)

let uses_source_relative_shared_folder_prefix extension = extension <> ".mlx"
let emits_line_directive extension = extension <> ".mlx"

let with_shared_folder_prefix shared_folder_prefix pps =
  pps
  |> List.map (fun value ->
      if String.starts_with ~prefix:"-shared-folder-prefix=" value then
        "-shared-folder-prefix=" ^ shared_folder_prefix
      else value)

let preprocess_pps ~shared_folder_prefix pps =
  Dune_sexp.field_atoms "pps"
    (with_shared_folder_prefix shared_folder_prefix pps)

let preprocess_field ~default_shared_folder_prefix
    ~mirrored_shared_folder_prefix ~pps ~modules ~mirrored_modules =
  let open Dune_sexp in
  let default_modules =
    modules
    |> List.filter (fun module_name ->
        not (List.mem module_name mirrored_modules))
  in
  let per_module_entry ~shared_folder_prefix modules =
    Dune_sexp.list
      (preprocess_pps ~shared_folder_prefix pps :: List.map atom modules)
  in
  match (mirrored_modules, default_modules) with
  | [], _ ->
      field "preprocess"
        [
          preprocess_pps ~shared_folder_prefix:default_shared_folder_prefix pps;
        ]
  | _, [] ->
      field "preprocess"
        [
          preprocess_pps ~shared_folder_prefix:mirrored_shared_folder_prefix pps;
        ]
  | _ ->
      field "preprocess"
        [
          form "per_module"
            [
              per_module_entry
                ~shared_folder_prefix:mirrored_shared_folder_prefix
                mirrored_modules;
              per_module_entry
                ~shared_folder_prefix:default_shared_folder_prefix
                default_modules;
            ];
        ]

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

let copy_rule ~deps ~target ~prelude_lines ~line_directive_path =
  let open Dune_sexp in
  let output_lines =
    match line_directive_path with
    | Some source_path ->
        let directive = line_directive source_path in
        if prelude_lines = [] then [ directive ]
        else directive :: (prelude_lines @ [ directive ])
    | None -> prelude_lines
  in
  let prelude_actions =
    output_lines |> List.map (fun line -> form "echo" [ atom (line ^ "\n") ])
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

let extraction_copy_rule ~deps ~target ~prelude_lines =
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
           form "progn"
             (prelude_actions
             @ [
                 run "%{bin:utopia.compiler}" [ "--extract-client"; "%{deps}" ];
               ]);
         ])
    ()

let copy_dependency_rule ~deps ~target =
  rule
    ~deps:[ Dune_sexp.atom deps ]
    ~target
    ~action:(run "cp" [ "%{deps}"; "%{target}" ])
    ()

let root_page_dependency ~source_root relative_file =
  Printf.sprintf "../%s" (Filename.concat source_root relative_file)

let native_page_dependency ~source_root relative_file =
  Printf.sprintf "../../%s" (Filename.concat source_root relative_file)

let native_api_dependency ~api_root relative_file =
  Printf.sprintf "../../%s" (Filename.concat api_root relative_file)

let root_shared_lib_dependency source_file =
  Printf.sprintf "../%s" (Filename.concat shared_lib_directory source_file)

let native_shared_lib_dependency source_file =
  Printf.sprintf "../../%s" (Filename.concat shared_lib_directory source_file)

let root_route_schema_dependency source_file =
  Printf.sprintf "../%s" source_file

let native_route_schema_dependency source_file =
  Printf.sprintf "../../%s" source_file

let app_reserved_basenames =
  [ "page"; "layout"; "route"; "_middleware"; "not-found" ]

let app_local_alias_lines ~source_root
    (code_files : Build_inputs.compiled_code_file list) =
  if source_root <> Names.app_directory then []
  else
    let aliases_by_module = Hashtbl.create 16 in
    code_files
    |> List.iter (fun (file : Build_inputs.compiled_code_file) ->
        let base_name =
          file.relative_file |> Filename.basename |> Filename.remove_extension
        in
        if not (List.mem base_name app_reserved_basenames) then
          let module_name = Names.sanitize_module_component base_name in
          let existing =
            Hashtbl.find_opt aliases_by_module module_name
            |> Option.value ~default:[]
          in
          Hashtbl.replace aliases_by_module module_name
            (file.base_name :: existing));
    aliases_by_module |> Hashtbl.to_seq |> List.of_seq
    |> List.sort (fun (left, _) (right, _) -> String.compare left right)
    |> List.filter_map (fun (module_name, targets) ->
        match targets |> List.sort_uniq String.compare with
        | [ target ] ->
            Some (Printf.sprintf "module %s = %s" module_name target)
        | _ -> None)

let source_project_root () =
  let project_path = Project.workspace_relative_project_path () in
  if project_path = "" then "%{workspace_root}"
  else Printf.sprintf "%%{workspace_root}/%s" project_path

let generate ?(dev_mode = false) ~source_root ~api_root
    ~is_client_component_page ~is_melange_lib_module files api_files
    route_entries _api_entries =
  let open Dune_sexp in
  (* Compute project-aware shared-folder-prefix for the PPX.  Dune sets
     pos_fname relative to _build/default/, so for a nested project like
     demo/notes the filename is demo/notes/_utopia/Foo.re.  The prefix
     must include the full project path to strip it cleanly. *)
  let project_path = Project.workspace_relative_project_path () in
  let melange_shared_folder_prefix =
    if project_path = "" then "_utopia/" else project_path ^ "/_utopia/"
  in
  let native_shared_folder_prefix =
    if project_path = "" then "_utopia/native/"
    else project_path ^ "/_utopia/native/"
  in
  let pages_library_name = Project.generated_pages_library_name () in
  let api_library_name = Project.generated_api_library_name () in
  let generated_utopia_library_name =
    Project.generated_utopia_library_name ()
  in
  let generated_routes_server_library_name =
    Project.generated_routes_server_library_name ()
  in
  let shared_lib_files : Build_inputs.shared_lib_file list =
    Build_inputs.shared_lib_files_for_build ()
  in
  let route_schema_files : Build_inputs.route_schema_file list =
    Build_inputs.route_schema_files_for_build route_entries
  in
  let has_shared_lib = Build_inputs.has_shared_lib () in
  let code_files = Build_inputs.code_files_for_build files in
  let api_code_files = Build_inputs.api_code_files_for_build api_files in
  let markdown_files = Build_inputs.markdown_files_for_build files in
  let app_local_aliases = app_local_alias_lines ~source_root code_files in
  let has_app_local_aliases = app_local_aliases <> [] in
  let shared_lib_open_modules =
    if has_shared_lib then [ generated_library_module ] else []
  in
  let page_open_modules =
    shared_lib_open_modules
    @ if has_app_local_aliases then [ generated_app_local_module ] else []
  in
  let is_route_or_layout_file relative_file =
    if source_root <> Names.app_directory then true
    else
      match relative_file |> Filename.basename |> Filename.remove_extension with
      | "page" | "layout" -> true
      | _ -> false
  in
  let melange_client_component_files =
    code_files
    |> List.filter (fun (file : Build_inputs.compiled_code_file) ->
        is_client_component_page file.relative_file)
  in
  (* Non-reserved app components (like date.mlx) are shared between
     server and client.  They're NOT pages/layouts but may be used by
     client components.  Plain-copy them into the melange stanza. *)
  let melange_shared_component_files =
    code_files
    |> List.filter (fun (file : Build_inputs.compiled_code_file) ->
        (not (is_client_component_page file.relative_file))
        && not (is_route_or_layout_file file.relative_file))
  in
  let melange_code_files =
    melange_client_component_files @ melange_shared_component_files
  in
  let melange_copy_rules =
    (melange_client_component_files
    |> List.map (fun (file : Build_inputs.compiled_code_file) ->
        let deps = root_page_dependency ~source_root file.relative_file in
        let prelude_lines =
          page_open_modules
          |> List.map (fun module_name ->
              open_statement file.extension module_name)
        in
        extraction_copy_rule ~deps
          ~target:(Printf.sprintf "%s%s" file.base_name file.extension)
          ~prelude_lines))
    @ (melange_shared_component_files
      |> List.map (fun (file : Build_inputs.compiled_code_file) ->
          let deps = root_page_dependency ~source_root file.relative_file in
          let prelude_lines =
            shared_lib_open_modules
            |> List.map (fun module_name ->
                open_statement file.extension module_name)
          in
          copy_rule ~deps
            ~target:(Printf.sprintf "%s%s" file.base_name file.extension)
            ~prelude_lines
            ~line_directive_path:
              (if emits_line_directive file.extension then Some deps else None))
      )
  in
  let melange_app_local_aliases =
    app_local_alias_lines ~source_root melange_code_files
  in
  let native_copy_rules =
    code_files
    |> List.map (fun (file : Build_inputs.compiled_code_file) ->
        let deps = native_page_dependency ~source_root file.relative_file in
        let modules_to_open =
          if is_route_or_layout_file file.relative_file then page_open_modules
          else shared_lib_open_modules
        in
        let prelude_lines =
          modules_to_open
          |> List.map (fun module_name ->
              open_statement file.extension module_name)
        in
        copy_rule ~deps
          ~target:(Printf.sprintf "%s%s" file.base_name file.extension)
          ~prelude_lines
          ~line_directive_path:
            (if emits_line_directive file.extension then Some deps else None))
  in
  let native_api_copy_rules =
    api_code_files
    |> List.map (fun (file : Build_inputs.compiled_code_file) ->
        let deps = native_api_dependency ~api_root file.relative_file in
        let prelude_lines =
          if has_shared_lib then
            [ open_statement file.extension generated_library_module ]
          else []
        in
        copy_rule ~deps
          ~target:(Printf.sprintf "%s%s" file.base_name file.extension)
          ~prelude_lines
          ~line_directive_path:
            (if emits_line_directive file.extension then Some deps else None))
  in
  let client_entry_rule =
    copy_dependency_rule
      ~deps:(Utopia_runtime.target_name Utopia_runtime.client_entry_source_file)
      ~target:client_entry_target
  in
  let dev_client_rule =
    if dev_mode then
      Some
        (copy_dependency_rule
           ~deps:"%{lib:utopia:utopia_dev_client/Utopia_dev_client.re}"
           ~target:"Utopia_dev_client.re")
    else None
  in
  (* Utopia client modules: copied from the installed utopia package into
     the melange.emit stanza as local modules.  These are the modules
     actually needed by client-side code (router, route types, call
     server, etc.).  They cannot be a library dependency because
     server-reason-react.react-server-dom-esbuild produces a
     pre-compiled JS that conflicts with melange.emit's output. *)
  let utopia_client_modules =
    [
      "Utopia_types.ml";
      "Utopia_route.ml";
      "Utopia_call_server.re";
      "Utopia_router.re";
      "Utopia_router_link.re";
      "Utopia_router_route.re";
      "Utopia.re";
      "ReactServerDOMEsbuild.re";
    ]
  in
  let utopia_client_copy_rules =
    utopia_client_modules
    |> List.map (fun filename ->
        copy_dependency_rule
          ~deps:(Printf.sprintf "%%{lib:utopia:utopia/%s}" filename)
          ~target:filename)
  in
  let utopia_client_module_names =
    utopia_client_modules |> List.map (fun f -> f |> Filename.remove_extension)
  in
  (* ReactServerDOMEsbuild is provided by the
     server-reason-react.react-server-dom-esbuild library in
     melange_libraries — no copy rule needed. *)
  (* Routes.ml and Routes_client.ml are owned by melange.emit (client side).
     For the native routes support library we copy them into the native/
     subdirectory so dune sees distinct source files in distinct directories. *)
  let native_routes_copy_rule =
    rule
      ~target:(generated_routes_module ^ ".ml")
      ~deps:[ atom ("../" ^ generated_routes_module ^ ".ml") ]
      ~action:(run "cp" [ "%{deps}"; "%{target}" ])
      ()
  in
  let native_routes_client_copy_rule =
    rule
      ~target:(generated_routes_client_module ^ ".ml")
      ~deps:[ atom ("../" ^ generated_routes_client_source) ]
      ~action:(run "cp" [ "%{deps}"; "%{target}" ])
      ()
  in
  let generated_utopia_library_rule =
    let route_schema_modules =
      route_schema_files
      |> List.map (fun (file : Build_inputs.route_schema_file) ->
          file.module_name)
    in
    form "library"
      [
        field_atom "name" generated_utopia_library_name;
        field_atom "wrapped" "false";
        field_atoms "modules"
          (generated_routes_module :: generated_routes_client_module
         :: route_schema_modules);
        field_atoms "libraries" [ "utopia" ];
      ]
  in
  let melange_shared_lib_copy_rules =
    if not has_shared_lib then []
    else
      shared_lib_files
      |> List.filter (fun (file : Build_inputs.shared_lib_file) ->
          is_melange_lib_module (Build_inputs.shared_lib_module_name file))
      |> List.map (fun (file : Build_inputs.shared_lib_file) ->
          let deps = root_shared_lib_dependency file.source_file in
          copy_rule ~deps
            ~target:(Build_inputs.shared_lib_target file)
            ~prelude_lines:[]
            ~line_directive_path:
              (if emits_line_directive file.extension then Some deps else None))
  in
  let native_shared_lib_copy_rules =
    if not has_shared_lib then []
    else
      shared_lib_files
      |> List.map (fun (file : Build_inputs.shared_lib_file) ->
          let deps = native_shared_lib_dependency file.source_file in
          copy_rule ~deps
            ~target:(Build_inputs.shared_lib_target file)
            ~prelude_lines:[]
            ~line_directive_path:
              (if emits_line_directive file.extension then Some deps else None))
  in
  let melange_shared_lib_namespace_rule =
    if not has_shared_lib then []
    else
      let reachable_lib_files =
        shared_lib_files
        |> List.filter (fun (file : Build_inputs.shared_lib_file) ->
            is_melange_lib_module (Build_inputs.shared_lib_module_name file))
      in
      let aliases =
        reachable_lib_files
        |> List.map (fun (file : Build_inputs.shared_lib_file) ->
            Printf.sprintf "module %s = %s" file.module_name
              (Build_inputs.shared_lib_module_name file))
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
              (Build_inputs.shared_lib_module_name file))
        |> String.concat "\n"
      in
      [ write_file_rule ~target:"Lib.re" ~content:aliases ]
  in
  let melange_app_local_namespace_rule =
    if not has_app_local_aliases then []
    else
      [
        write_file_rule ~target:"App_local.re"
          ~content:(String.concat "\n" melange_app_local_aliases);
      ]
  in
  let native_app_local_namespace_rule =
    if not has_app_local_aliases then []
    else
      [
        write_file_rule ~target:"App_local.re"
          ~content:(String.concat "\n" app_local_aliases);
      ]
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
  let markdown_rules =
    markdown_files
    |> List.map (fun (file : Build_inputs.compiled_code_file) ->
        rule
          ~deps:[ atom (root_page_dependency ~source_root file.relative_file) ]
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
      melange_code_files
      |> List.map (fun (file : Build_inputs.compiled_code_file) ->
          file.base_name)
    in
    let source_relative_page_modules =
      melange_code_files
      |> List.filter (fun (file : Build_inputs.compiled_code_file) ->
          uses_source_relative_shared_folder_prefix file.extension)
      |> List.map (fun (file : Build_inputs.compiled_code_file) ->
          file.base_name)
    in
    let melange_shared_lib_files =
      if has_shared_lib then
        shared_lib_files
        |> List.filter (fun (file : Build_inputs.shared_lib_file) ->
            is_melange_lib_module (Build_inputs.shared_lib_module_name file))
      else []
    in
    let lib_modules =
      melange_shared_lib_files
      |> List.map (fun (file : Build_inputs.shared_lib_file) ->
          Build_inputs.shared_lib_module_name file)
    in
    let source_relative_lib_modules =
      melange_shared_lib_files
      |> List.filter (fun (file : Build_inputs.shared_lib_file) ->
          uses_source_relative_shared_folder_prefix file.extension)
      |> List.map (fun (file : Build_inputs.shared_lib_file) ->
          Build_inputs.shared_lib_module_name file)
    in
    let mirrored_modules =
      source_relative_lib_modules @ source_relative_page_modules
    in
    let modules =
      let prelude_modules =
        (if has_shared_lib then [ generated_library_module ] else [])
        @ if has_app_local_aliases then [ generated_app_local_module ] else []
      in
      let dev_modules = if dev_mode then [ "Utopia_dev_client" ] else [] in
      prelude_modules @ lib_modules @ page_modules @ dev_modules
      @ utopia_client_module_names
      @ [
          generated_routes_module;
          generated_routes_client_module;
          Utopia_runtime.client_entry_melange_module_name;
        ]
    in
    let compile_flags =
      [ "-w"; "-26-27-32-39" ] @ open_flags [ "Melange_json.Primitives" ]
    in
    form "melange.emit"
      [
        field_atom "target" melange_target_name;
        field_atoms "module_systems" [ "es6" ];
        field_atoms "compile_flags" compile_flags;
        field_atoms "modules" modules;
        field_atoms "libraries" melange_libraries;
        preprocess_field
          ~default_shared_folder_prefix:melange_shared_folder_prefix
          ~mirrored_shared_folder_prefix:"../" ~pps:melange_preprocess_pps
          ~modules ~mirrored_modules;
      ]
  in
  let native_library_rule =
    let page_modules =
      code_files
      |> List.map (fun (file : Build_inputs.compiled_code_file) ->
          file.base_name)
    in
    let source_relative_page_modules =
      code_files
      |> List.filter (fun (file : Build_inputs.compiled_code_file) ->
          uses_source_relative_shared_folder_prefix file.extension)
      |> List.map (fun (file : Build_inputs.compiled_code_file) ->
          file.base_name)
    in
    let lib_modules =
      if has_shared_lib then
        shared_lib_files
        |> List.map (fun (file : Build_inputs.shared_lib_file) ->
            Build_inputs.shared_lib_module_name file)
      else []
    in
    let source_relative_lib_modules =
      if has_shared_lib then
        shared_lib_files
        |> List.filter (fun (file : Build_inputs.shared_lib_file) ->
            uses_source_relative_shared_folder_prefix file.extension)
        |> List.map (fun (file : Build_inputs.shared_lib_file) ->
            Build_inputs.shared_lib_module_name file)
      else []
    in
    let mirrored_modules =
      source_relative_lib_modules @ source_relative_page_modules
    in
    let modules =
      let prelude_modules =
        (if has_shared_lib then [ generated_library_module ] else [])
        @ if has_app_local_aliases then [ generated_app_local_module ] else []
      in
      prelude_modules @ lib_modules @ page_modules
    in
    let flags =
      native_library_flags @ open_flags [ "Melange_json.Primitives" ]
    in
    form "library"
      [
        field_atom "name" pages_library_name;
        field_atom "wrapped" "false";
        field_atoms "flags" flags;
        field_atoms "modules" modules;
        field_atoms "libraries"
          (native_library_libraries generated_utopia_library_name);
        preprocess_field
          ~default_shared_folder_prefix:native_shared_folder_prefix
          ~mirrored_shared_folder_prefix:"../../" ~pps:native_preprocess_pps
          ~modules ~mirrored_modules;
      ]
  in
  let native_api_library_rule =
    let api_modules =
      api_code_files
      |> List.map (fun (file : Build_inputs.compiled_code_file) ->
          file.base_name)
    in
    if api_modules = [] then None
    else
      let source_relative_api_modules =
        api_code_files
        |> List.filter (fun (file : Build_inputs.compiled_code_file) ->
            uses_source_relative_shared_folder_prefix file.extension)
        |> List.map (fun (file : Build_inputs.compiled_code_file) ->
            file.base_name)
      in
      let lib_modules =
        if has_shared_lib then
          shared_lib_files
          |> List.map (fun (file : Build_inputs.shared_lib_file) ->
              Build_inputs.shared_lib_module_name file)
        else []
      in
      let source_relative_lib_modules =
        if has_shared_lib then
          shared_lib_files
          |> List.filter (fun (file : Build_inputs.shared_lib_file) ->
              uses_source_relative_shared_folder_prefix file.extension)
          |> List.map (fun (file : Build_inputs.shared_lib_file) ->
              Build_inputs.shared_lib_module_name file)
        else []
      in
      let mirrored_modules =
        source_relative_lib_modules @ source_relative_api_modules
      in
      let modules =
        if has_shared_lib then
          generated_library_module :: (lib_modules @ api_modules)
        else api_modules
      in
      let flags =
        native_library_flags @ open_flags [ "Melange_json.Primitives" ]
      in
      Some
        (form "library"
           [
             field_atom "name" api_library_name;
             field_atom "wrapped" "false";
             field_atoms "flags" flags;
             field_atoms "modules" modules;
             field_atoms "libraries"
               (native_api_library_libraries generated_utopia_library_name);
             preprocess_field
               ~default_shared_folder_prefix:native_shared_folder_prefix
               ~mirrored_shared_folder_prefix:"../../"
               ~pps:native_preprocess_pps ~modules ~mirrored_modules;
           ])
  in
  let native_subdir_rule =
    let native_api_library_rules =
      match native_api_library_rule with None -> [] | Some rule -> [ rule ]
    in
    form "subdir"
      (atom native_subdir_name
      :: List.concat
           [
             native_copy_rules;
             native_api_copy_rules;
             native_shared_lib_copy_rules;
             native_route_schema_copy_rules;
             native_shared_lib_namespace_rule;
             native_app_local_namespace_rule;
             [
               native_routes_copy_rule;
               native_routes_client_copy_rule;
               generated_utopia_library_rule;
               native_library_rule;
             ]
             @ native_api_library_rules;
           ])
  in
  let esbuild_stamp = ".esbuild_stamp" in
  (* The esbuild rule is inside (subdir _utopia ...) where
     data_only_dirs shields it from (alias_rec all).  It uses a plain
     file target — not an alias — because dune does not expose aliases
     from data-only directories on the command line.  File targets in
     data-only subdirs are still addressable via their build path. *)
  let esbuild_rule =
    rule ~target:esbuild_stamp
      ~deps:
        [
          form "alias" [ atom "melange" ];
          atom "esbuild.config.mjs";
          atom "paths.mjs";
          atom "../package.json";
        ]
      ~action:
        (form "progn"
           [
             form "chdir"
               [
                 atom (source_project_root ());
                 run "node" [ generated_esbuild_config_path () ];
               ];
             form "write-file" [ atom "%{target}"; atom "" ];
           ])
      ()
  in
  let server_executable =
    form "executable"
      [
        field_atom "name" generated_server_name;
        field_atoms "modules" server_executable_modules;
        field_atoms "libraries"
          (server_executable_libraries ~pages_library_name ~api_library_name
             ~generated_routes_server_library_name
             ~generated_utopia_library_name
             ~has_api_library:(api_code_files <> []));
      ]
  in
  let generated_routes_server_library_rule =
    form "library"
      [
        field_atom "name" generated_routes_server_library_name;
        field_atom "wrapped" "false";
        field_atoms "flags" native_library_flags;
        field_atoms "modules" [ generated_routes_server_module ];
        field_atoms "libraries"
          (routes_server_libraries ~pages_library_name ~api_library_name
             ~generated_utopia_library_name
             ~has_api_library:(api_code_files <> []));
      ]
  in
  let ssg_rule =
    rule ~alias:"ssg"
      ~deps:[ form "alias" [ atom "all" ] ]
      ~action:(run "./server_main.exe" [ "--ssg" ])
      ()
  in
  let root_melange_alias = form "alias" [ field_atom "name" "melange" ] in
  let generated_subdir_rule =
    form "subdir"
      (atom generated_subdir_name
      :: List.concat
           [
             melange_copy_rules;
             [ client_entry_rule ];
             utopia_client_copy_rules;
             (match dev_client_rule with Some r -> [ r ] | None -> []);
             melange_shared_lib_copy_rules;
             melange_route_schema_copy_rules;
             melange_shared_lib_namespace_rule;
             melange_app_local_namespace_rule;
             [ melange_rule; generated_routes_server_library_rule ];
             markdown_rules;
             [ native_subdir_rule; esbuild_rule; server_executable; ssg_rule ];
           ])
  in
  render_many [ root_melange_alias; generated_subdir_rule ]
