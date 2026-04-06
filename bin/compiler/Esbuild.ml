type build_mode = Development | Production

let development = Development
let production = Production

let build_mode_to_string = function
  | Development -> "development"
  | Production -> "production"

let generate_paths ~build_mode () =
  let project_path = Project.workspace_relative_project_path () in
  let build_mode = build_mode_to_string build_mode in
  Printf.sprintf
    "export const projectPath = %S;\n\
     export const buildMode = %S;\n\
     export const nodeEnv = %S;\n"
    project_path build_mode build_mode
