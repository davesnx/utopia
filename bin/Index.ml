let write_file file s =
  Out_channel.with_open_bin file (fun oc -> Out_channel.output_string oc s)

let rec empty_folder path =
  match Sys.is_directory path with
  | true ->
      (Sys.readdir path) |>
        (Array.iter (fun name -> empty_folder (Filename.concat path name)))
  | false -> Sys.remove path

let () =
  empty_folder "_utopia";

  Pages.list_of_pages |>
    (List.iter
       (fun (module Page: Pages.Page) ->
          let file = "_utopia/" ^ (Page.path ^ ".html") in
          write_file file (ReactDOM.renderToStaticMarkup (Page.make ~key:"" ()))))
