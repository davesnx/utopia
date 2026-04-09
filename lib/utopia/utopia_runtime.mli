type file

val target_name : file -> string
val module_name : file -> string
val repository_source_path : file -> string
val installed_source_path : file -> string
val root_files : file list
val native_files : file list
val melange_module_names : string list
val native_module_names : string list
val client_entry_source_file : file
val client_entry_melange_target_name : string
val client_entry_melange_module_name : string
val dev_overlay_source_file : file
val dev_overlay_melange_target_name : string
val dev_overlay_melange_module_name : string
