let process_cmark : strict:bool -> string -> React.element =
 fun ~strict md ->
  let doc = Cmarkit.Doc.of_string ~layout:true ~strict md in
  Render_to_element.of_doc ~safe:false doc

let maybe_read_line () = try Some (read_line ()) with End_of_file -> None

let rec loop acc =
  match maybe_read_line () with
  | Some line -> loop (line :: acc)
  | None -> List.rev acc

let () =
  let input = String.concat "\n" (loop []) in
  let element = process_cmark ~strict:false input in
  (* let path = "TODO" in
     let (module Page) = Utopia.Router.make path (fun () -> element) in
     Utopia.Router.register (module Page) *)
  ReactDOM.renderToStaticMarkup element |> print_endline
