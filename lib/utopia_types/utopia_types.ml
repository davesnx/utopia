type page_kind = Code_page | Markdown_page
type param_kind = Single | Catch_all | Optional_catch_all
type route_segment = Static of string | Param of string * param_kind

let kind_of_extension = function
  | ".ml" | ".mlx" | ".re" -> Some Code_page
  | ".md" -> Some Markdown_page
  | _ -> None

let string_of_kind = function
  | Code_page -> "code"
  | Markdown_page -> "markdown"

let parse_kind = function
  | "code" -> Some Code_page
  | "markdown" -> Some Markdown_page
  | _ -> None

let string_of_param_kind = function
  | Single -> "single"
  | Catch_all -> "catch_all"
  | Optional_catch_all -> "optional_catch_all"

let parse_param_kind = function
  | "single" -> Some Single
  | "catch_all" -> Some Catch_all
  | "optional_catch_all" -> Some Optional_catch_all
  | _ -> None
