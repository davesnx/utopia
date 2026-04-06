let doc_of_string input = Cmarkit.Doc.of_string ~layout:true ~strict:false input

let element_of_doc ?(components = Components.make ()) doc =
  Render.of_doc ~safety:Render.State.Unsafe ~components doc

let render_doc_to_html doc = ReactDOM.renderToStaticMarkup (element_of_doc doc)
let render_string_to_html input = input |> doc_of_string |> render_doc_to_html
