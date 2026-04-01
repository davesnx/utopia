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
  | "utopia.server" -> Some Utopia_path.Server
  | "utopia" -> Some Utopia_path.Cli
  | "dune" -> None
  | _ -> None

let built_binary_path name =
  match binary_of_name name with
  | None -> None
  | Some binary ->
      let project = Utopia_path.current_project () in
      let path =
        Utopia_path.built_binary project binary
        |> Utopia_path.file_path |> Utopia_path.to_string
      in
      if file_exists path then Some path else None

let resolve_bin name =
  match built_binary_path name with
  | Some path -> path
  | None -> (
      let executable_dir = resolve_executable_path () |> Filename.dirname in
      let sibling = Filename.concat executable_dir name in
      if is_executable sibling then sibling
      else match lookup_in_path name with Some path -> path | None -> name)
