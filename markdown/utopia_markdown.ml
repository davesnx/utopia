type frontmatter_value = Frontmatter.frontmatter_value =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | List of frontmatter_value list
  | Object of (string * frontmatter_value) list

type frontmatter_object = Frontmatter.frontmatter_object

type extraction = Frontmatter.extraction = {
  frontmatter : frontmatter_object option;
  markdown_body : string;
  title : string option;
  description : string option;
  warning : string option;
}

let extract_frontmatter ?source_file markdown =
  Frontmatter.extract ?source_file markdown

let frontmatter_object_of_list (values : (string * frontmatter_value) list) :
    frontmatter_object =
  values

let doc_of_string input =
  let extraction = extract_frontmatter input in
  Cmarkit.Doc.of_string ~layout:true ~strict:false extraction.markdown_body

let element_of_doc ?(components = Components.make ()) doc =
  Render.of_doc ~safety:Render.State.Unsafe ~components doc

let render_doc_to_html doc = ReactDOM.renderToStaticMarkup (element_of_doc doc)
let render_string_to_html input = input |> doc_of_string |> render_doc_to_html
