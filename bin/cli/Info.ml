let version_of program args =
  try
    match Process.run_command_capture program args with
    | Some output -> output
    | None -> Terminal.dim "unknown"
  with Unix.Unix_error (Unix.ENOENT, _, _) -> Terminal.dim "not found"

let run ~version _args =
  Printf.printf "\n%s\n\n" (Terminal.bold "utopia info");

  Printf.printf "  %s\n" (Terminal.bold "Versions");
  Printf.printf "    utopia:     %s\n" version;
  Printf.printf "    ocaml:      %s\n" (version_of "ocaml" [ "-vnum" ]);
  Printf.printf "    dune:       %s\n" (version_of "dune" [ "--version" ]);
  Printf.printf "    melange:    %s\n" (version_of "melange" [ "--version" ]);
  Printf.printf "    reason:     %s\n" (version_of "refmt" [ "--version" ]);

  Printf.printf "\n  %s\n" (Terminal.bold "Project");
  let cwd = Sys.getcwd () in
  Printf.printf "    root:       %s\n" cwd;
  Printf.printf "    pages:      %s\n"
    (if Filesystem.is_directory Artifacts.source_pages_directory then
       Terminal.green "found"
     else Terminal.red "missing");
  Printf.printf "    _utopia:    %s\n"
    (if Filesystem.is_directory Artifacts.generated_directory then
       Terminal.green "generated"
     else Terminal.dim "not generated");
  Printf.printf "    _build:     %s\n"
    (if Filesystem.is_directory Artifacts.build_directory then
       Terminal.green "present"
     else Terminal.dim "not present");

  let route_count = Manifest.route_count () in
  if route_count > 0 then Printf.printf "    routes:     %d\n" route_count;

  Printf.printf "\n  %s\n" (Terminal.bold "Commands");
  [
    ("build", true);
    ("prod", true);
    ("dev", true);
    ("clean", true);
    ("info", true);
  ]
  |> List.iter (fun (name, implemented) ->
      let status =
        if implemented then Terminal.green "implemented"
        else Terminal.yellow "scaffolded"
      in
      Printf.printf "    %-10s  %s\n" name status);

  Printf.printf "\n";
  0
