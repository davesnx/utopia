let current_project = lazy (Utopia_path.current_project ())
let project_paths () = Lazy.force current_project

let project_scope_identity () =
  let project = project_paths () in
  match project.project_workspace_path with
  | Some path -> path |> Fpath.rem_empty_seg |> Fpath.to_string
  | None -> project.project_root |> Fpath.basename

let generated_pages_library_name () =
  "pages_" ^ Names.sanitize_library_component (project_scope_identity ())

let generated_utopia_library_name () =
  "utopia_" ^ Names.sanitize_library_component (project_scope_identity ())

let generated_source_lib_library_name () =
  "source_lib_" ^ Names.sanitize_library_component (project_scope_identity ())

let workspace_relative_project_path () =
  project_paths () |> Utopia_path.workspace_relative_project_path_string

let project_path_depth project_path =
  if project_path = "" then 0
  else List.length (String.split_on_char '/' project_path)
