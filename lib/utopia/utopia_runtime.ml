type file = {
  target_name : string;
  module_name : string;
  repository_source_path : string;
  installed_source_path : string;
}

let make_custom_file ~target_name ~repository_source_path ~installed_source_path
    () =
  {
    target_name;
    module_name = Filename.remove_extension target_name;
    repository_source_path;
    installed_source_path;
  }

let make_file target_name =
  make_custom_file ~target_name
    ~repository_source_path:(Filename.concat "lib/utopia" target_name)
    ~installed_source_path:target_name ()

let esbuild_config = make_file "esbuild.config.mjs"
let client_entry_source_file = make_file "client_entry.re"
let dev_overlay_source_file = make_file "Utopia_dev_overlay.re"

let all_files =
  [ esbuild_config; client_entry_source_file; dev_overlay_source_file ]

let root_files = all_files
let native_files = []
let melange_module_names = []
let native_module_names = []
let target_name file = file.target_name
let module_name file = file.module_name
let repository_source_path file = file.repository_source_path
let installed_source_path file = file.installed_source_path
let client_entry_melange_target_name = "client_entry_melange.re"
let client_entry_melange_module_name = "client_entry_melange"
let dev_overlay_melange_target_name = "Utopia_dev_overlay.re"
let dev_overlay_melange_module_name = "Utopia_dev_overlay"
