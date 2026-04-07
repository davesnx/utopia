type frontmatter_value = Utopia_markdown.frontmatter_value =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | List of frontmatter_value list
  | Object of (string * frontmatter_value) list

type frontmatter_object = Utopia_markdown.frontmatter_object

let frontmatter ~path =
  [%platform
    match () with
    | Server -> Utopia_server.markdown_frontmatter ~path
    | Client ->
        ignore path;
        None]
