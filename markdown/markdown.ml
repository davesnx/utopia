let process_cmark : strict:bool -> string -> string =
 fun ~strict md ->
  let doc = Cmarkit.Doc.of_string ~layout:true ~strict md in
  Render.of_doc ~safe:false doc

let maybe_read_line () = try Some (read_line ()) with End_of_file -> None

let rec loop acc =
  match maybe_read_line () with
  | Some line -> loop (line :: acc)
  | None -> List.rev acc

let input = String.concat "\n" (loop [])

let () =
  print_endline
    (Printf.sprintf
       {|module Page = {
      let path = "TODO";
    
      [@react.component]
      let make = () => {
       <> %s; </>
      };
    };
    
    Utopia.Router.register((module Page));|}
       (process_cmark ~strict:false input))
