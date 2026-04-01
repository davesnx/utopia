type project = {
  workspace_root : Fpath.t;
  project_root : Fpath.t;
  project_workspace_path : Fpath.t option;
}

type file_ref = { path : Fpath.t; display : string }
type binary = Cli | Compiler | Server

let of_string_exn value =
  match Fpath.of_string value with
  | Ok path -> path
  | Error (`Msg message) -> invalid_arg message

let to_string = Fpath.to_string
let exists path = Sys.file_exists (to_string path)
let is_directory path = exists path && Sys.is_directory (to_string path)
let file_path file_ref = file_ref.path
let file_display file_ref = file_ref.display
let build_directory_name = Fpath.v "_build"
let generated_directory_name = Fpath.v "_utopia"
let generated_native_directory_name = Fpath.v "native"
let shared_lib_directory_name = Fpath.v "lib"

let cwd_dir () =
  of_string_exn (Sys.getcwd ()) |> Fpath.to_dir_path |> Fpath.normalize

let rec workspace_root_from dir =
  if exists Fpath.(dir / "dune-project") then Some dir
  else
    let parent = Fpath.parent dir |> Fpath.normalize in
    if Fpath.equal parent dir || String.equal (Fpath.basename dir) "_build" then
      None
    else workspace_root_from parent

let current_project () =
  let project_root = cwd_dir () in
  let workspace_root =
    workspace_root_from project_root |> Option.value ~default:project_root
  in
  let project_workspace_path =
    match
      Fpath.relativize ~root:(Fpath.rem_empty_seg workspace_root) project_root
    with
    | Some relative when Fpath.is_current_dir relative -> None
    | relative -> relative
  in
  { workspace_root; project_root; project_workspace_path }

let workspace_relative_project_path_string project =
  match project.project_workspace_path with
  | None -> ""
  | Some path -> Fpath.rem_empty_seg path |> Fpath.to_string

let root_relative_display ~root path =
  match Fpath.relativize ~root:(Fpath.rem_empty_seg root) path with
  | Some relative -> Fpath.to_string relative
  | None -> Fpath.to_string path

let build_root project =
  Fpath.(project.workspace_root // build_directory_name / "default")

let project_utopia_dir project =
  match project.project_workspace_path with
  | None -> generated_directory_name
  | Some path -> Fpath.(path // generated_directory_name)

let project_generated_directory project =
  Fpath.(project.project_root // generated_directory_name)

let project_generated_native_directory project =
  Fpath.(project_generated_directory project // generated_native_directory_name)

let project_shared_lib_directory project =
  Fpath.(project.project_root // shared_lib_directory_name)

let workspace_root_from_build_path path =
  let value = Fpath.to_string path in
  let marker = Fpath.(build_directory_name / "default") |> Fpath.to_string in
  let marker_len = String.length marker in
  let value_len = String.length value in
  let rec find index =
    if index + marker_len > value_len then None
    else if String.sub value index marker_len = marker then Some index
    else find (index + 1)
  in
  match find 0 with
  | None -> None
  | Some index -> String.sub value 0 index |> of_string_exn |> Option.some

let project_root_ref project relative_path display =
  { path = Fpath.(project.project_root // relative_path); display }

let routes_manifest project =
  project_root_ref project
    (Fpath.v "_utopia/routes.manifest")
    "_utopia/routes.manifest"

let generated_dune project =
  project_root_ref project (Fpath.v "_utopia/dune") "_utopia/dune"

let generated_esbuild_config project =
  project_root_ref project
    (Fpath.v "_utopia/esbuild.config.mjs")
    "_utopia/esbuild.config.mjs"

let generated_esbuild_paths project =
  project_root_ref project (Fpath.v "_utopia/paths.mjs") "_utopia/paths.mjs"

let generated_routes_source project =
  project_root_ref project
    (Fpath.v "_utopia/Utopia_routes.ml")
    "_utopia/Utopia_routes.ml"

let generated_server_source project =
  project_root_ref project
    (Fpath.v "_utopia/server_main.ml")
    "_utopia/server_main.ml"

let generated_server_exe project =
  let path =
    Fpath.(build_root project // project_utopia_dir project / "server_main.exe")
  in
  { path; display = root_relative_display ~root:project.workspace_root path }

let build_relative_binary_path = function
  | Cli -> Fpath.v "bin/cli/cli.exe"
  | Compiler -> Fpath.v "bin/compiler/compiler.exe"
  | Server -> Fpath.v "bin/server/Server.exe"

let built_binary project binary =
  let path = Fpath.(build_root project // build_relative_binary_path binary) in
  { path; display = root_relative_display ~root:project.workspace_root path }
