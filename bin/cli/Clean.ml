let run _args =
  Printf.printf "\n%s\n\n" (Terminal.bold "utopia clean");
  let removed = ref [] in

  if Filesystem.remove_if_exists (Fpath.to_string Artifacts.build_directory)
  then removed := Fpath.to_string Artifacts.build_directory :: !removed;

  if Filesystem.remove_if_exists (Fpath.to_string Artifacts.generated_directory)
  then removed := Fpath.to_string Artifacts.generated_directory :: !removed;

  let dune = Binaries.resolve_bin "dune" in
  let code = Process.run_command dune (Artifacts.dune_clean_args ()) in
  if code <> 0 then
    Terminal.print_warn
      "dune clean returned non-zero (this is usually harmless)";

  if !removed = [] then Terminal.print_done "Nothing to clean"
  else (
    List.iter
      (fun path -> Terminal.print_done (Printf.sprintf "Removed %s" path))
      (List.rev !removed);
    Terminal.print_done "Clean complete");
  Printf.printf "\n";
  0
