let source_pages_directory = "pages"
let build_directory = Utopia_path.build_directory_name
let generated_directory = Utopia_path.generated_directory_name
let current_project = lazy (Utopia_path.current_project ())
let project_paths () = Lazy.force current_project

let workspace_root_string () =
  project_paths () |> fun project ->
  project.workspace_root |> Utopia_path.to_string

let dune_root_args () =
  [ "--root"; workspace_root_string (); "--no-print-directory" ]

let dune_build_args targets = [ "build" ] @ dune_root_args () @ targets
let dune_clean_args () = [ "clean" ] @ dune_root_args ()
let routes_manifest_ref () = Utopia_path.routes_manifest (project_paths ())
let generated_dune_ref () = Utopia_path.generated_dune (project_paths ())

let generated_server_exe_ref () =
  Utopia_path.generated_server_exe (project_paths ())

let artifact_path artifact =
  artifact |> Utopia_path.file_path |> Utopia_path.to_string

let artifact_display artifact = Utopia_path.file_display artifact

let artifact_exists artifact =
  artifact |> Utopia_path.file_path |> Utopia_path.exists

let required_server_artifacts () =
  [ routes_manifest_ref (); generated_dune_ref (); generated_server_exe_ref () ]

let missing_artifacts artifacts =
  List.filter (fun artifact -> not (artifact_exists artifact)) artifacts
