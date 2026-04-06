open Dune_sexp

let native_library_flags = [ ":standard"; "-w"; "-26-27-39" ]

let source_lib_libraries generated_utopia_library_name =
  [ generated_utopia_library_name; "utopia"; "utopia.markdown_runtime" ]

let source_page_libraries generated_utopia_library_name ~has_source_lib =
  let base =
    [ generated_utopia_library_name; "utopia"; "utopia.markdown_runtime" ]
  in
  if has_source_lib then base @ [ Project.generated_source_lib_library_name () ]
  else base

let source_lib_preprocess_pps =
  [
    "server-reason-react.ppx";
    "-shared-folder-prefix=lib/";
    "server-reason-react.browser_ppx";
    "server-reason-react.melange_ppx";
    "melange-json-native.ppx";
  ]

let source_page_preprocess_pps =
  [
    "server-reason-react.ppx";
    "-shared-folder-prefix=";
    "server-reason-react.browser_ppx";
    "server-reason-react.melange_ppx";
    "melange-json-native.ppx";
  ]

let is_valid_source_module_name name =
  String.length name > 0
  &&
  let first = name.[0] in
  (match first with '0' .. '9' -> false | _ -> true)
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false)
       name

let page_directory relative_file =
  let dir = Filename.dirname relative_file in
  if dir = "." then "" else dir

let source_page_module_name_opt (file : Build_inputs.compiled_code_file) =
  let base_name =
    file.relative_file |> Filename.basename |> Filename.remove_extension
  in
  if is_valid_source_module_name base_name then
    Some (Names.sanitize_module_component base_name)
  else None

let source_page_library_name page_directory =
  let suffix =
    if page_directory = "" then "pages_root"
    else Filename.concat "pages" page_directory
  in
  "source_pages_"
  ^ Names.sanitize_library_component
      (Project.project_scope_identity () ^ "_" ^ suffix)

let source_page_directories code_files =
  let seen = Hashtbl.create 16 in
  code_files
  |> List.iter (fun (file : Build_inputs.compiled_code_file) ->
      match source_page_module_name_opt file with
      | None -> ()
      | Some module_name ->
          let dir = page_directory file.relative_file in
          let modules = Hashtbl.find_opt seen dir |> Option.value ~default:[] in
          if List.mem module_name modules then ()
          else Hashtbl.replace seen dir (modules @ [ module_name ]));
  seen |> Hashtbl.to_seq |> List.of_seq
  |> List.sort (fun (left, _) (right, _) -> String.compare left right)

let generate files _route_entries =
  let code_files = Build_inputs.code_files_for_build files in
  let has_shared_lib = Build_inputs.has_shared_lib () in
  let shared_lib_files : Build_inputs.shared_lib_file list =
    Build_inputs.shared_lib_files_for_build ()
  in
  let has_source_lib = has_shared_lib && shared_lib_files <> [] in
  let page_directories = source_page_directories code_files in
  if (not has_source_lib) && page_directories = [] then ""
  else
    let generated_utopia_library_name =
      Project.generated_utopia_library_name ()
    in
    let source_lib_stanzas =
      if not has_source_lib then []
      else
        let source_lib_library_name =
          Project.generated_source_lib_library_name ()
        in
        let shared_lib_modules =
          shared_lib_files
          |> List.map (fun (file : Build_inputs.shared_lib_file) ->
              file.module_name)
        in
        [
          form "subdir"
            [
              atom "lib";
              form "library"
                [
                  field_atom "name" source_lib_library_name;
                  field_atom "wrapped" "false";
                  field_atoms "modules" shared_lib_modules;
                  field_atoms "flags"
                    (native_library_flags
                    @ [ "-open"; "Melange_json.Primitives" ]);
                  field_atoms "libraries"
                    (source_lib_libraries generated_utopia_library_name);
                  field "preprocess"
                    [ field_atoms "pps" source_lib_preprocess_pps ];
                ];
            ];
        ]
    in
    let source_page_stanzas =
      page_directories
      |> List.map (fun (page_directory, modules) ->
          let subdir_path =
            if page_directory = "" then "pages"
            else Filename.concat "pages" page_directory
          in
          form "subdir"
            [
              atom subdir_path;
              form "library"
                [
                  field_atom "name" (source_page_library_name page_directory);
                  field_atom "wrapped" "false";
                  field_atoms "modules" modules;
                  field_atoms "flags"
                    (native_library_flags
                    @ [ "-open"; "Melange_json.Primitives" ]);
                  field_atoms "libraries"
                    (source_page_libraries generated_utopia_library_name
                       ~has_source_lib);
                  field "preprocess"
                    [ field_atoms "pps" source_page_preprocess_pps ];
                ];
            ])
    in
    render_many (source_lib_stanzas @ source_page_stanzas)
