let process_cmark ~strict md =
  let doc = Cmarkit.Doc.of_string ~layout:true ~strict md in
  Render.of_doc ~safe:false ~components:(Render.Custom_components.make ()) doc

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
