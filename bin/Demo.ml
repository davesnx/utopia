[@@@warning "-32"]

let write_file file s =
  Out_channel.with_open_bin file (fun oc -> Out_channel.output_string oc s)

let rec empty_folder path =
  match Sys.is_directory path with
  | true ->
      (Sys.readdir path) |>
        (Array.iter (fun name -> empty_folder (Filename.concat path name)))
  | false -> Sys.remove path


let load_plug fname =
  let fname = Dynlink.adapt_filename fname in
  if Sys.file_exists fname then
    try
      Dynlink.loadfile fname
    with
    | (Dynlink.Error err) as e -> print_endline ("ERROR loading plugin: " ^ (Dynlink.error_message err) ); raise e
    | _ -> failwith "Unknow error while loading plugin"
  else
    failwith "Plugin file does not exist"

let () =
  empty_folder "_utopia";

  load_plug "_build/default/pages/pages.cmo";

  let list_of_pages = Utopia.Loader.get_plugin () in

  list_of_pages |>
    (List.iter
       (fun (module Page: Utopia.Loader.PLUG) ->
          let file = "_utopia/" ^ (Page.path ^ ".html") in
          write_file file (ReactDOM.renderToStaticMarkup (Page.make ~key:"" ()))));
