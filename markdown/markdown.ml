let () =
  let rec loop acc =
    match Some (read_line ()) with
    | Some line -> loop (line :: acc)
    | None | (exception End_of_file) -> List.rev acc
  in
  let input = String.concat "\n" (loop []) in
  Utopia_markdown.render_string_to_html input |> print_endline
