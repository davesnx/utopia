let source_app_directory = "app"
let source_pages_directory = "pages"
let build_directory = Utopia_path.build_directory_name
let generated_directory = Utopia_path.generated_directory_name
let generated_directory_string = Fpath.to_string generated_directory
let generated_dist_directory = Filename.concat generated_directory_string "dist"

let generated_static_directory =
  Filename.concat generated_directory_string "static"

let current_project = lazy (Utopia_path.current_project ())
let project_paths () = Lazy.force current_project

let project_target_generated_directory () =
  let project = project_paths () in
  match project.project_workspace_path with
  | None -> Filename.concat "target" generated_directory_string
  | Some path ->
      Filename.concat "target"
        (Filename.concat (Fpath.to_string path) generated_directory_string)

let build_output_directories () =
  [
    generated_dist_directory;
    generated_static_directory;
    project_target_generated_directory ();
  ]

let workspace_root_string () =
  project_paths () |> fun project ->
  project.workspace_root |> Utopia_path.to_string

let project_root_string () =
  project_paths () |> fun project ->
  project.project_root |> Utopia_path.to_string

let dune_root_args () =
  [ "--root"; workspace_root_string (); "--no-print-directory" ]

let dune_build_args targets = [ "build" ] @ dune_root_args () @ targets
let dune_clean_args () = [ "clean" ] @ dune_root_args ()
let generated_dune_ref () = Utopia_path.generated_dune (project_paths ())

let generated_server_exe_ref () =
  Utopia_path.generated_server_exe (project_paths ())

let generated_server_build_target () =
  let project = project_paths () in
  Fpath.(Utopia_path.project_utopia_dir project / "server_main.exe")
  |> Fpath.to_string

let generated_build_targets () = [ generated_server_build_target () ]

let artifact_path artifact =
  artifact |> Utopia_path.file_path |> Utopia_path.to_string

let artifact_display artifact = Utopia_path.file_display artifact

let artifact_exists artifact =
  artifact |> Utopia_path.file_path |> Utopia_path.exists

let required_server_artifacts () =
  [ generated_dune_ref (); generated_server_exe_ref () ]

let missing_artifacts artifacts =
  List.filter (fun artifact -> not (artifact_exists artifact)) artifacts

let has_source_routes_directory () =
  Filesystem.is_directory source_app_directory
  || Filesystem.is_directory source_pages_directory
