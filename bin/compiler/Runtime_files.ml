let lookup_in_path executable =
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path_value ->
      path_value |> String.split_on_char ':'
      |> List.find_map (fun directory ->
          let candidate = Filename.concat directory executable in
          if Filesystem.file_exists candidate then Some candidate else None)

let resolve_executable_path () =
  let executable = Sys.executable_name in
  let candidate =
    if not (Filename.is_relative executable) then executable
    else if String.contains executable '/' then
      Filename.concat (Sys.getcwd ()) executable
    else lookup_in_path executable |> Option.value ~default:executable
  in
  try Unix.realpath candidate with Unix.Unix_error _ -> candidate

let workspace_root_from_build_path path =
  let marker = Filename.concat "_build" "default" ^ "/" in
  let marker_len = String.length marker in
  let path_len = String.length path in
  let rec find index =
    if index + marker_len > path_len then None
    else if String.sub path index marker_len = marker then Some index
    else find (index + 1)
  in
  match find 0 with
  | None -> None
  | Some index -> Some (String.sub path 0 index)

let runtime_source_candidates file =
  let workspace_candidates =
    [
      ( Project.project_paths () |> fun project ->
        project.workspace_root |> Fpath.to_string );
    ]
    |> List.map (fun root ->
        Filename.concat root (Utopia_runtime.repository_source_path file))
  in
  let executable_path = resolve_executable_path () in
  let build_workspace_candidates =
    match
      executable_path |> Utopia_path.of_string_exn
      |> Utopia_path.workspace_root_from_build_path
    with
    | None -> []
    | Some root ->
        [
          Filename.concat
            (Utopia_path.to_string root)
            (Utopia_runtime.repository_source_path file);
        ]
  in
  let executable_dir = executable_path |> Filename.dirname in
  let installed_candidates =
    [
      Filename.concat executable_dir "../lib/utopia";
      Filename.concat executable_dir "../lib";
    ]
    |> List.map (fun root ->
        Filename.concat root (Utopia_runtime.installed_source_path file))
  in
  workspace_candidates @ build_workspace_candidates @ installed_candidates

let resolve_project_support_source file =
  match
    List.find_opt Filesystem.file_exists (runtime_source_candidates file)
  with
  | Some path -> path
  | None ->
      failwith
        (Printf.sprintf "Missing project support file for %s"
           (Utopia_runtime.target_name file))

let copy_runtime_files () =
  let project = Project.project_paths () in
  let generated_directory =
    Utopia_path.project_generated_directory project |> Utopia_path.to_string
  in
  Filesystem.ensure_directory generated_directory;
  let copy_into directory file =
    let source_file = resolve_project_support_source file in
    let target_file =
      Filename.concat directory (Utopia_runtime.target_name file)
    in
    Filesystem.copy_file source_file target_file
  in
  List.iter (copy_into generated_directory) Utopia_runtime.root_files
