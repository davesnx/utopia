type t

val atom : string -> t
val list : t list -> t
val form : string -> t list -> t
val field : string -> t list -> t
val field_atom : string -> string -> t
val field_atoms : string -> string list -> t
val render_many : t list -> string
