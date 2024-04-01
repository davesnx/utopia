let write_file file s =
  Out_channel.with_open_bin file (fun oc -> Out_channel.output_string oc s)

let rec empty_folder path =
  match Sys.file_exists path with
  | false -> (
      try Unix.mkdir path 0o777
      with Unix.Unix_error (err, _, _) ->
        Printf.eprintf "Error creating directory '%s': %s\n" path
          (Unix.error_message err))
  | true -> (
      match Sys.is_directory path with
      | true ->
          Sys.readdir path
          |> Array.iter (fun name -> empty_folder (Filename.concat path name))
      | false -> Sys.remove path)

let load_pages fname =
  let fname = Dynlink.adapt_filename fname in
  if Sys.file_exists fname then
    try Dynlink.loadfile fname with
    | Dynlink.Error err as e ->
        print_endline
        @@ Printf.sprintf "ERROR loading page: %s\n%s" fname
             (Dynlink.error_message err);
        raise e
    | _ -> failwith "Unknow error while loading plugin"
  else failwith "Plugin file does not exist"

let render_html_page ~title content =
  let component =
    Html.make ~key:"html" ~title
      ~scripts:
        [
          React.createElement "script"
            [ React.JSX.string "src" "https://cdn.tailwindcss.com" ]
            [];
        ]
      ~body:content ()
  in
  let output = ReactDOM.renderToStaticMarkup component in
  Printf.sprintf "<!DOCTYPE html>%s" output

let () =
  empty_folder "_utopia";

  load_pages "_build/default/pages/pages.cmo";

  Utopia.get_pages ()
  |> List.iter (fun (module Page : Utopia.Loader_page) ->
         let file = "_utopia/" ^ Page.path ^ ".html" in
         let data = Page.loader () in
         let content = render_html_page ~title:Page.path (Page.make data) in
         write_file file content)
