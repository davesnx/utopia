open Ppxlib
open Ast_helper
open Asttypes
open Parsetree

let loc = Location.none
let txt value = Location.{ txt = value; loc }
let longident_of_string value = Longident.parse value
let lid value = txt (longident_of_string value)
let evar value = Exp.ident (lid value)
let pvar value = Pat.var (txt value)
let pwild = [%pat? _]
let unit_pat = [%pat? ()]
let unit = [%expr ()]
let none = [%expr None]
let some value = [%expr Some [%e value]]
let string value = Exp.constant (Const.string ~loc value)
let bool value = if value then [%expr true] else [%expr false]
let float value = Exp.constant (Const.float (Printf.sprintf "%f" value))
let construct name value = Exp.construct (lid name) value
let tuple values = Exp.tuple values
let ptuple values = Pat.tuple values
let pstring value = Pat.constant (Const.string ~loc value)
let pconstruct name value = Pat.construct (lid name) (Option.map snd value)

let value_param pattern =
  { pparam_loc = loc; pparam_desc = Pparam_val (Nolabel, None, pattern) }

let labelled_param label pattern =
  { pparam_loc = loc; pparam_desc = Pparam_val (Labelled label, None, pattern) }

let optional_param label pattern =
  { pparam_loc = loc; pparam_desc = Pparam_val (Optional label, None, pattern) }

let list values =
  List.fold_right
    (fun value acc -> [%expr [%e value] :: [%e acc]])
    values [%expr []]

let list_pat ?tail values =
  let tail = Option.value tail ~default:(pconstruct "[]" None) in
  List.fold_right (fun value acc -> [%pat? [%p value] :: [%p acc]]) values tail

let record fields =
  Exp.record (List.map (fun (name, value) -> (lid name, value)) fields) None

let app fn args =
  Exp.apply fn (List.map (fun (label, value) -> (label, value)) args)

let call value args = app (evar value) args
let infix op left right = app (evar op) [ (Nolabel, left); (Nolabel, right) ]
let case pat expr = Exp.case pat expr
let match_ expr cases = Exp.match_ expr cases
let if_ cond then_ else_ = [%expr if [%e cond] then [%e then_] else [%e else_]]

let let_in pat value body =
  [%expr
    let [%p pat] = [%e value] in
    [%e body]]

let fun0 body = [%expr fun () -> [%e body]]
let fun1 name body = [%expr fun [%p pvar name] -> [%e body]]

let typed_pat name type_name =
  Pat.constraint_ (pvar name) (Typ.constr (lid type_name) [])

let let_value name expr = [%stri let [%p pvar name] = [%e expr]]

let exp_fun params body =
  List.fold_right
    (fun param acc ->
      match param.pparam_desc with
      | Pparam_val (label, default, pattern) ->
          Exp.fun_ label default pattern acc
      | Pparam_newtype _ ->
          invalid_arg "Pparam_newtype not supported in Ocaml_gen")
    params body

let let_function ?result_type name params body =
  let body =
    match result_type with None -> body | Some typ -> Exp.constraint_ body typ
  in
  let expr = exp_fun (List.map value_param params) body in
  let_value name expr

let let_function_params ?result_type name params body =
  let body =
    match result_type with None -> body | Some typ -> Exp.constraint_ body typ
  in
  let expr = exp_fun params body in
  let_value name expr

let include_module module_name =
  [%stri include [%m Mod.ident (lid module_name)]]

let module_alias name target =
  Str.module_ (Mb.mk (txt (Some name)) (Mod.ident (lid target)))

let type_record name fields =
  let labels =
    fields
    |> List.map (fun (field_name, typ) -> Type.field (txt field_name) typ)
  in
  Str.type_ Nonrecursive [ Type.mk ~kind:(Ptype_record labels) (txt name) ]

let type_variant name constructors =
  let constructors =
    constructors
    |> List.map (fun (ctor_name, fields) ->
        match fields with
        | [] -> Type.constructor (txt ctor_name)
        | _ ->
            let labels =
              fields
              |> List.map (fun (field_name, typ) ->
                  Type.field (txt field_name) typ)
            in
            Type.constructor ~args:(Pcstr_record labels) (txt ctor_name))
  in
  Str.type_ Nonrecursive
    [ Type.mk ~kind:(Ptype_variant constructors) (txt name) ]

let core_type name = Typ.constr (lid name) []
let core_type_apply name args = Typ.constr (lid name) args
let option_type value = Typ.constr (lid "option") [ value ]
let list_type value = Typ.constr (lid "list") [ value ]

let module_ ?(attrs = []) name structure =
  let binding = Mb.mk (txt (Some name)) (Mod.structure structure) in
  let binding =
    { binding with pmb_attributes = binding.pmb_attributes @ attrs }
  in
  Str.module_ binding

let native_platform_attr =
  Attr.mk (txt "platform") (PStr [ Str.eval (evar "native") ])

let structure_to_string structure =
  let buffer = Buffer.create 1024 in
  let formatter = Format.formatter_of_buffer buffer in
  Pprintast.structure formatter structure;
  Format.pp_print_flush formatter ();
  Buffer.contents buffer
