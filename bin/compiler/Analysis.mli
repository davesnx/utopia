(** Lexical analysis of OCaml/Reason source files.

    Scans source code while ignoring comments and string literals, then detects
    specific export patterns ([let before], [let paths],
    [react.client.component]). *)

type origin = { line : int; column : int }
(** Source location: line and column (1-indexed). *)

type token = { text : string; origin : origin; byte_offset : int }
(** A single token extracted from source code. *)

type summary = {
  before_export_origin : origin option;
  paths_origin : origin option;
  react_client_component_origins : origin list;
}
(** Analysis summary for a source file. *)

val scan_code_tokens : string -> token list
(** Check if a character can start an identifier. *)
val is_identifier_start : char -> bool

(** Check if a character can appear in an identifier. *)
val is_identifier_char : char -> bool

(** Tokenize source code, skipping comments and string literals. *)

val find_sequence_origin :
  ?origin_index:int -> token list -> string list -> origin option
(** Find the first occurrence of a token sequence and return the origin at the
    given [origin_index] within the match. *)

val find_sequence_origins :
  ?origin_index:int -> token list -> string list -> origin list
(** Find all occurrences of a token sequence and return their origins. *)

val analyze : string -> summary
(** Run the full analysis pipeline on source code. *)

val string_of_origin : origin -> string
(** Format an origin as ["line:column"]. *)
