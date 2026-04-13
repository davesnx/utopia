open Ppxlib

val evar : string -> expression
val pvar : string -> pattern
val pwild : pattern
val unit_pat : pattern
val unit : expression
val none : expression
val some : expression -> expression
val string : string -> expression
val bool : bool -> expression
val float : float -> expression
val construct : string -> expression option -> expression
val tuple : expression list -> expression
val ptuple : pattern list -> pattern
val pstring : string -> pattern

val pconstruct :
  string -> (string Location.loc list * pattern) option -> pattern

val value_param : pattern -> function_param
val labelled_param : string -> pattern -> function_param
val optional_param : string -> pattern -> function_param
val list : expression list -> expression
val list_pat : ?tail:pattern -> pattern list -> pattern
val record : (string * expression) list -> expression
val call : string -> (arg_label * expression) list -> expression
val infix : string -> expression -> expression -> expression
val case : pattern -> expression -> case
val match_ : expression -> case list -> expression
val if_ : expression -> expression -> expression -> expression
val let_in : pattern -> expression -> expression -> expression
val fun0 : expression -> expression
val fun1 : string -> expression -> expression
val typed_pat : string -> string -> pattern
val let_value : string -> expression -> structure_item

val let_function :
  ?result_type:core_type ->
  string ->
  pattern list ->
  expression ->
  structure_item

val let_function_params :
  ?result_type:core_type ->
  string ->
  function_param list ->
  expression ->
  structure_item

val include_module : string -> structure_item
val module_alias : string -> string -> structure_item
val type_record : string -> (string * core_type) list -> structure_item

val type_variant :
  string -> (string * (string * core_type) list) list -> structure_item

val core_type : string -> core_type
val core_type_apply : string -> core_type list -> core_type
val option_type : core_type -> core_type
val list_type : core_type -> core_type
val module_ : ?attrs:attribute list -> string -> structure -> structure_item
val native_platform_attr : attribute
val structure_to_string : structure -> string
