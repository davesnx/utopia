type t = Sexplib0.Sexp.t

let atom value = Sexplib0.Sexp.Atom value
let list values = Sexplib0.Sexp.List values
let form name values = list (atom name :: values)
let field = form
let field_atom name value = field name [ atom value ]
let field_atoms name values = field name (List.map atom values)

let render_many sexps =
  match sexps with
  | [] -> ""
  | _ ->
      String.concat "\n\n" (List.map Sexplib0.Sexp.to_string_hum sexps) ^ "\n"
