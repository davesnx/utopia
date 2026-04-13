(** Convention constants and name sanitization for the utopia compiler. *)

val app_directory : string
(** The canonical app directory name. *)

val app_api_directory : string
(** The canonical API subdirectory under app. *)

val app_reserved_basenames : string list
(** File basenames reserved by the framework (page, layout, route, etc.). *)

val sanitize_module_component : string -> string
(** Sanitize a path component into a valid OCaml module name component. Strips
    invalid characters, trims underscores, and capitalizes. *)

val sanitize_library_component : string -> string
(** Sanitize a path component into a valid dune library name component.
    Lowercases and replaces invalid characters with underscores. *)

val generated_module_base : string -> string
(** Derive a [Pages__Foo__Bar] module base from a relative file path. *)

val compiled_page_module_name : string -> string
(** Compute the full compiled page module name from a relative file. *)

val compiled_api_module_name : string -> string
(** Compute the full compiled API module name from a relative file. *)

val strip_directory_prefix : directory:string -> string -> string
(** Strip a directory prefix from a source file path. *)

val strip_pages_prefix : string -> string
(** Strip the app directory prefix from a source file. *)

val strip_api_prefix : string -> string
(** Strip the API directory prefix from a source file. *)

val route_constructor_name_of_source : string -> string
(** Derive a route constructor name from a source file. *)

val compiled_page_module_name_of_source : string -> string
(** Compute the compiled page module name from a full source file path. *)

val compiled_api_module_name_of_source : string -> string
(** Compute the compiled API module name from a full source file path. *)
