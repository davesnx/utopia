let print_report () =
  let route_count = Manifest.route_count () in
  let routes_manifest = Artifacts.routes_manifest_ref () in
  let generated_dune = Artifacts.generated_dune_ref () in
  let generated_server = Artifacts.generated_server_exe_ref () in
  Printf.printf "\n%s\n" (Terminal.bold "  Build report");
  Printf.printf "  Routes:     %d\n" route_count;
  if Artifacts.artifact_exists generated_dune then
    Printf.printf "  Generated:  %s\n"
      (Artifacts.artifact_display generated_dune);
  if Artifacts.artifact_exists routes_manifest then
    Printf.printf "  Manifest:   %s\n"
      (Artifacts.artifact_display routes_manifest);
  if Artifacts.artifact_exists generated_server then
    Printf.printf "  Server:     %s\n"
      (Artifacts.artifact_display generated_server);
  Printf.printf "  Output:     %s\n%!"
    (Fpath.to_string Artifacts.build_directory);
  print_newline ()

let run _args =
  Printf.printf "\n%s\n\n" (Terminal.bold "utopia build");
  Terminal.print_step "Validating project structure";
  if not (Filesystem.is_directory Artifacts.source_pages_directory) then (
    Terminal.print_err
      "Missing 'pages' directory. Create it and add page files.";
    exit 1);
  Terminal.print_done "Project structure valid";

  Terminal.print_step "Generating route manifest and dune rules";
  let compiler = Binaries.resolve_bin "utopia.compiler" in
  let code = Process.run_command compiler [ "--mode"; "production" ] in
  if code <> 0 then (
    Terminal.print_err "Compiler failed (see errors above)";
    exit code);
  Terminal.print_done "Route manifest and dune rules generated";

  Terminal.print_step "Building server and client outputs";
  let dune = Binaries.resolve_bin "dune" in
  let code = Process.run_command dune (Artifacts.dune_build_args [ "." ]) in
  if code <> 0 then (
    Terminal.print_err "dune build failed";
    exit code);
  Terminal.print_done "Build complete";

  print_report ();
  0
