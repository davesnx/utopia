type project = {
  workspace_root : Fpath.t;
  project_root : Fpath.t;
  project_workspace_path : Fpath.t option;
}

type file_ref = { path : Fpath.t; display : string }
type binary = Cli | Compiler | Server

val of_string_exn : string -> Fpath.t
val to_string : Fpath.t -> string
val exists : Fpath.t -> bool
val is_directory : Fpath.t -> bool
val file_path : file_ref -> Fpath.t
val file_display : file_ref -> string
val current_project : unit -> project
val build_directory_name : Fpath.t
val generated_directory_name : Fpath.t
val generated_native_directory_name : Fpath.t
val shared_lib_directory_name : Fpath.t
val workspace_relative_project_path_string : project -> string
val root_relative_display : root:Fpath.t -> Fpath.t -> string
val build_root : project -> Fpath.t
val project_utopia_dir : project -> Fpath.t
val project_generated_directory : project -> Fpath.t
val project_generated_native_directory : project -> Fpath.t
val project_shared_lib_directory : project -> Fpath.t
val workspace_root_from_build_path : Fpath.t -> Fpath.t option
val routes_manifest : project -> file_ref
val generated_dune : project -> file_ref
val generated_esbuild_config : project -> file_ref
val generated_esbuild_paths : project -> file_ref
val generated_routes_source : project -> file_ref
val generated_server_source : project -> file_ref
val generated_server_exe : project -> file_ref
val built_binary : project -> binary -> file_ref
