let () =
  let rec loop acc =
    match Some (read_line ()) with
    | Some line -> loop (line :: acc)
    | None | (exception End_of_file) -> List.rev acc
  in
  let input = String.concat "\n" (loop []) in
  let doc = Cmarkit.Doc.of_string ~layout:true ~strict:false input in
  let element =
    Render.of_doc ~safe:false ~components:(Components.make ()) doc
  in
  ReactDOM.renderToStaticMarkup element |> print_endline
