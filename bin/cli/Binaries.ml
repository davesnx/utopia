let file_exists path = Sys.file_exists path && not (Sys.is_directory path)

let is_executable path =
  file_exists path
  &&
    try
      Unix.access path [ Unix.X_OK ];
      true
    with Unix.Unix_error _ -> false

let lookup_in_path executable =
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path_value ->
      path_value |> String.split_on_char ':'
      |> List.find_map (fun directory ->
          let candidate = Filename.concat directory executable in
          if file_exists candidate then Some candidate else None)

let resolve_executable_path () =
  let executable = Sys.executable_name in
  let candidate =
    if not (Filename.is_relative executable) then executable
    else if String.contains executable '/' then
      Filename.concat (Sys.getcwd ()) executable
    else lookup_in_path executable |> Option.value ~default:executable
  in
  try Unix.realpath candidate with Unix.Unix_error _ -> candidate

let binary_of_name = function
  | "utopia.compiler" -> Some Utopia_path.Compiler
  | "utopia" -> Some Utopia_path.Cli
  | "dune" -> None
  | _ -> None

let built_binary_path_for_project project binary =
  Utopia_path.built_binary project binary
  |> Utopia_path.file_path |> Utopia_path.to_string

let project_for_workspace_root workspace_root =
  {
    Utopia_path.workspace_root;
    project_root = workspace_root;
    project_workspace_path = None;
  }

let built_binary_path name =
  match binary_of_name name with
  | None -> None
  | Some binary -> (
      let project = Utopia_path.current_project () in
      let current_project_path = built_binary_path_for_project project binary in
      if file_exists current_project_path then Some current_project_path
      else
        let executable_workspace_path =
          resolve_executable_path () |> Utopia_path.of_string_exn
          |> Utopia_path.workspace_root_from_build_path
          |> Option.map (fun workspace_root ->
              let workspace_project =
                project_for_workspace_root workspace_root
              in
              built_binary_path_for_project workspace_project binary)
        in
        match executable_workspace_path with
        | Some path when file_exists path -> Some path
        | _ -> None)

let resolve_bin name =
  match built_binary_path name with
  | Some path -> path
  | None -> (
      let executable_dir = resolve_executable_path () |> Filename.dirname in
      let sibling = Filename.concat executable_dir name in
      if is_executable sibling then sibling
      else match lookup_in_path name with Some path -> path | None -> name)
