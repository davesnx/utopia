let required_packages =
  [
    "react";
    "react-dom";
    "esbuild";
    "server-reason-react-esbuild-plugin";
    "server-reason-react-server-dom-esbuild";
  ]

let package_json_name = "package.json"

type failure =
  | Missing_package_json
  | Missing_node
  | Missing_packages of string list

let package_json_exists project_root =
  let package_json = Filename.concat project_root package_json_name in
  Sys.file_exists package_json && not (Sys.is_directory package_json)

let node_available node =
  Process.run_command_capture node [ "--version" ] |> Option.is_some

let resolve_package ~node ~project_root package_name =
  let script =
    "const { createRequire } = require('module');"
    ^ "const path = require('path');" ^ "const projectRoot = process.argv[1];"
    ^ "const packageName = process.argv[2];" ^ "const requireFromProject ="
    ^ " createRequire(path.join(projectRoot, 'package.json'));"
    ^ "requireFromProject.resolve(packageName);"
  in
  Process.run_command_capture node [ "-e"; script; project_root; package_name ]
  |> Option.is_some

let validate project_root =
  if not (package_json_exists project_root) then Error Missing_package_json
  else
    let node = Binaries.resolve_bin "node" in
    if not (node_available node) then Error Missing_node
    else
      let missing_packages =
        List.filter
          (fun package_name ->
            not (resolve_package ~node ~project_root package_name))
          required_packages
      in
      if missing_packages = [] then Ok ()
      else Error (Missing_packages missing_packages)

let print_failure ~command_name = function
  | Missing_package_json ->
      Terminal.print_err
        (Printf.sprintf
           "Missing package.json in project root; `%s` requires npm \
            dependencies."
           command_name);
      Printf.eprintf "    remediation: npm install\n%!"
  | Missing_node ->
      Terminal.print_err
        (Printf.sprintf
           "Could not execute `node`; `%s` requires Node.js/npm dependencies."
           command_name);
      Printf.eprintf "    remediation: install Node.js and run npm install\n%!"
  | Missing_packages packages ->
      Terminal.print_err
        (Printf.sprintf "Missing required npm dependencies for `%s`."
           command_name);
      List.iter
        (fun package_name -> Printf.eprintf "    missing: %s\n%!" package_name)
        packages;
      Printf.eprintf "    remediation: npm install\n%!"

let ensure ~command_name () =
  Terminal.print_step "Checking npm dependencies";
  let project_root = Artifacts.project_root_string () in
  match validate project_root with
  | Ok () ->
      Terminal.print_done "Npm dependencies resolved";
      true
  | Error failure ->
      print_failure ~command_name failure;
      false
