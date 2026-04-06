let remove_directories paths =
  let removed = ref [] in
  List.iter
    (fun path ->
      if Filesystem.remove_if_exists path then removed := path :: !removed)
    paths;
  List.rev !removed

let print_removed removed =
  if removed = [] then Terminal.print_done "Nothing to clean"
  else (
    List.iter
      (fun path -> Terminal.print_done (Printf.sprintf "Removed %s" path))
      removed;
    Terminal.print_done "Clean complete")

let run args =
  let config = Flags.parse_clean args in
  Printf.printf "\n%s\n\n" (Terminal.bold "utopia clean");
  let removed =
    match config.mode with
    | Flags.Build_outputs ->
        remove_directories (Artifacts.build_output_directories ())
    | Flags.Full ->
        let removed =
          remove_directories
            [
              Fpath.to_string Artifacts.build_directory;
              Fpath.to_string Artifacts.generated_directory;
              Artifacts.project_target_generated_directory ();
            ]
        in
        let dune = Binaries.resolve_bin "dune" in
        let code = Process.run_command dune (Artifacts.dune_clean_args ()) in
        if code <> 0 then
          Terminal.print_warn
            "dune clean returned non-zero (this is usually harmless)";
        removed
  in
  print_removed removed;
  Printf.printf "\n";
  0
