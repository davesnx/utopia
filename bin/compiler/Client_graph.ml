(** Inter-file module dependency graph for melange optimization.

    Given a set of seed modules (client component pages) and their module
    references, computes the transitive closure of lib modules that must be
    included in the melange build. *)

module StringSet = Client_component_scan.StringSet

type lib_module_info = {
  module_name : string;
  source_file : string;
  extension : string;
}

(** Scan a source file for module references (capitalized idents followed by
    ".") *)
let scan_module_references source =
  let tokens = Analysis.scan_code_tokens source in
  let tokens_array = Array.of_list tokens in
  let len = Array.length tokens_array in
  let modules = ref StringSet.empty in
  for i = 0 to len - 2 do
    let token = (tokens_array.(i) : Analysis.token) in
    if
      String.length token.text > 0
      && token.text.[0] >= 'A'
      && token.text.[0] <= 'Z'
      && (tokens_array.(i + 1) : Analysis.token).text = "."
    then modules := StringSet.add token.text !modules
  done;
  (* Also collect open/include targets *)
  for i = 0 to len - 1 do
    let token = (tokens_array.(i) : Analysis.token) in
    if token.text = "open" || token.text = "include" then
      let j =
        if i + 1 < len && (tokens_array.(i + 1) : Analysis.token).text = "!"
        then i + 2
        else i + 1
      in
      if
        j < len
        && String.length (tokens_array.(j) : Analysis.token).text > 0
        && (tokens_array.(j) : Analysis.token).text.[0] >= 'A'
        && (tokens_array.(j) : Analysis.token).text.[0] <= 'Z'
      then
        modules :=
          StringSet.add (tokens_array.(j) : Analysis.token).text !modules
  done;
  !modules

(** Build a mapping from public module names to lib module info.

    Lib modules are exposed under their public name (e.g. "Utils" for
    lib/utils.re which becomes Lib__Utils internally). The public name is the
    capitalized sanitized filename without prefix. *)
let build_lib_module_map (lib_files : Build_inputs.shared_lib_file list) =
  let map = Hashtbl.create 16 in
  List.iter
    (fun (file : Build_inputs.shared_lib_file) ->
      Hashtbl.replace map file.module_name
        {
          module_name = Build_inputs.shared_lib_module_name file;
          source_file = file.source_file;
          extension = file.extension;
        })
    lib_files;
  map

(** Compute the transitive closure of lib modules reachable from seed module
    references.

    [seed_refs] is the set of module names referenced by client component code.
    [lib_module_map] maps public module names to lib module info.
    [shared_lib_directory] is the path to the lib/ directory.

    Returns the set of internal lib module names (e.g. "Lib__Utils") that must
    be included in melange. *)
let compute_lib_closure ~seed_refs ~lib_module_map ~shared_lib_directory =
  let included = Hashtbl.create 16 in
  let worklist = Queue.create () in
  (* Seed the worklist with directly referenced lib modules *)
  StringSet.iter
    (fun module_ref ->
      match Hashtbl.find_opt lib_module_map module_ref with
      | Some info when not (Hashtbl.mem included info.module_name) ->
          Hashtbl.replace included info.module_name info;
          Queue.push info worklist
      | _ -> ())
    seed_refs;
  (* Expand transitively *)
  while not (Queue.is_empty worklist) do
    let info = Queue.pop worklist in
    let source_path = Filename.concat shared_lib_directory info.source_file in
    if Sys.file_exists source_path then
      let source =
        In_channel.with_open_bin source_path (fun ch -> In_channel.input_all ch)
      in
      let refs = scan_module_references source in
      StringSet.iter
        (fun module_ref ->
          match Hashtbl.find_opt lib_module_map module_ref with
          | Some dep_info when not (Hashtbl.mem included dep_info.module_name)
            ->
              Hashtbl.replace included dep_info.module_name dep_info;
              Queue.push dep_info worklist
          | _ -> ())
        refs
  done;
  Hashtbl.fold
    (fun name _ acc -> StringSet.add name acc)
    included StringSet.empty
