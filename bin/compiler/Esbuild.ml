let generate_paths () =
  let project_path = Project.workspace_relative_project_path () in
  Printf.sprintf "export const projectPath = %S;\n" project_path
