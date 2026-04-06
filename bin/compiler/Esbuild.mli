type build_mode

val development : build_mode
val production : build_mode
val generate_paths : build_mode:build_mode -> unit -> string
