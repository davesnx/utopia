(*

let write_to_file path content =
  Eio.Path.with_open_out ~create:(`Exclusive 0o600) path @@ fun flow ->
  Eio.Flow.copy_string content flow

let rec empty_folder path =
  match Eio.Path.rmtree ~missing_ok:true path with
  | () -> Eio.Path.mkdir ~perm:0o777 path
  | exception Eio.Io _ -> ()

(* match Sys.file_exists path with
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
       | false -> Sys.remove path) *)

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

(* There must be a way to point to a module type like
   type layout = Utopia.Loader_page.layout *)
type layout =
  ?key:string ->
  title:string ->
  scripts:React.element list ->
  children:React.element ->
  unit ->
  React.element

let render_html_page ~title ~(layout : layout) children =
  let component : React.element =
    layout ~key:"html" ~title ~scripts:[] ~children ()
  in
  let output = ReactDOM.renderToStaticMarkup component in
  Printf.sprintf "<!DOCTYPE html>%s" output

let split_at n lst =
  let rec aux n lst acc =
    if n <= 0 then (List.rev acc, lst)
    else
      match lst with
      | [] -> (List.rev acc, [])
      | head :: tail -> aux (n - 1) tail (head :: acc)
  in
  aux n lst []

let split_list_into_max_size_lists lst max_size =
  let rec aux lst acc =
    match lst with
    | [] -> List.rev acc
    | _ ->
        let chunk, rest = split_at max_size lst in
        aux rest (chunk :: acc)
  in
  aux lst []

let bootstrap () : unit =
  let ( / ) = Eio.Path.( / ) in
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.Src.set_level Cohttp_eio.src (Some Debug);

  Utopia.page ~path:"index" (fun () ->
      (div ~children:[ React.string "Static page" ] () [@JSX]));

  Utopia.register ~path:"home"
    ~loader:(fun () -> "home")
    (fun data -> (div ~children:[ React.string ("Hello " ^ data) ] () [@JSX]));

  Utopia.register ~path:"users"
    ~loader:(fun () -> ())
    (fun _ -> (div ~children:[ React.string "This page is slow!" ] () [@JSX]));

  Array.make 5_000 "mock_page"
  |> Array.iteri (fun index fixture ->
         Utopia.register
           ~path:(fixture ^ Int.to_string index)
           ~loader:(fun () -> fixture)
           (fun data ->
             (div
                ~children:
                  [
                    React.string data;
                    (h1 ~children:[ React.int index ] () [@JSX]);
                  ]
                () [@JSX])));

  let pages = Utopia.get_pages () in
  Eio_main.run @@ fun env ->
  let cwd = Eio.Stdenv.fs env in
  let utopia_artifacts_folder = cwd / "_utopia" in
  empty_folder utopia_artifacts_folder;

  Eio.Switch.run @@ fun sw ->
  (* let clock = Eio.Stdenv.clock env in *)
  Eio.traceln "Number of pages: %d" (Seq.length pages);

  let generate_page (module Page : Utopia.Loader_page) =
    let file = utopia_artifacts_folder / (Page.path ^ ".html") in
    Eio.traceln "Rendering page: %s" Page.path;
    let data = Page.loader () in
    let content =
      render_html_page ~layout:Page.layout ~title:Page.path (Page.make data)
    in
    write_to_file file content
  in

  let treshold = 1024 in

  (* let fibers = pages |> Seq.map (fun p () -> generate_page p) in *)
  let list_of_pages = List.of_seq pages in
  let fibers = split_list_into_max_size_lists list_of_pages treshold in
  (* let fibers = List.fold_left (fun acc p ->
         if List.length acc >= treshold then
           acc @
     ) [] list_of_pages in *)
  (* Eio.Fiber.all new_fibers *)
  List.iter (fun p -> Eio.Fiber.List.iter (fun p -> generate_page p) p) fibers
*)

let read_files path =
  match Sys.file_exists path with
  | false -> Error (`Page_directory_doesnt_exist path)
  | true ->
      let pages = Sys.readdir path in
      (* |> Array.iter (fun name -> empty_folder (Filename.concat path name)) *)
      Ok pages

let write_to_file file content =
  Out_channel.with_open_bin file (fun channel -> output_string channel content)

(* TODO: Probably we want to use sexp expresions or any abstraction on top *)
let generate_dune_rules files =
  let pages =
    List.map
      (fun page -> (Filename.remove_extension page, Filename.extension page))
      files
  in
  let ml_pages, md_pages =
    pages
    |> List.partition (fun (_, extension) ->
           extension = ".ml" || extension = ".re")
  in
  let custom_rules =
    ml_pages
    |> List.map (fun (file, extension) ->
           Printf.sprintf
             "(rule\n\
             \ (deps ../pages/%s%s)\n\
             \ (targets %s_melange%s %s_native%s)\n\
             \ (action\n\
             \  (progn\n\
             \   (run cp %%{deps} %s_melange%s)\n\
             \   (run cp %%{deps} %s_native%s))))\n\n"
             file extension file extension file extension file extension file
             extension)
    |> String.concat ""
  in
  let markdown_rules =
    md_pages
    |> List.map (fun (file, extension) ->
           Printf.sprintf
             {|(rule
 (deps ../pages/%s%s)
 (target %s.html)
 (action
  (with-stdout-to %%{target}
  (with-stdin-from %%{deps}
   (run %%{bin:utopia.markdown})))))

|}
             file extension file)
    |> String.concat ""
  in
  let melange_rule =
    let modules =
      ml_pages
      |> List.map (fun (page, _extension) -> Printf.sprintf "%s_melange" page)
      |> String.concat " "
    in
    Printf.sprintf
      "(melange.emit\n\
      \ (target %s)\n\
      \ (modules %s)\n\
      \ (libraries reason-react)\n\
      \ (preprocess\n\
      \  (pps reason-react-ppx)))\n\n"
      "target" modules
  in
  let library_rules =
    let modules =
      ml_pages
      |> List.map (fun (page, _extension) -> Printf.sprintf "%s_native" page)
      |> String.concat " "
    in

    Printf.sprintf
      "(library\n\
      \ (name pages)\n\
      \ (modules %s)\n\
      \ (public_name utopia)\n\
      \ (libraries server-reason-react.react server-reason-react.reactDom)\n\
      \ (preprocess\n\
      \  (pps server-reason-react.ppx)))\n\n"
      modules
  in
  Printf.sprintf "%s%s%s%s" custom_rules melange_rule markdown_rules
    library_rules

let () =
  let utopia_dune_file = "_utopia/dune" in
  print_endline "\n\nUtopia compiler";
  Sys.remove utopia_dune_file;
  match read_files "pages" with
  | Error (`Page_directory_doesnt_exist path) ->
      Printf.eprintf "  Error reading the '%s' directory\n" path
  | Ok pages ->
      Printf.printf "  Pages: %s\n" (String.concat ", " (Array.to_list pages));
      print_endline "\n  Generating rules\n";
      let dune_rules = generate_dune_rules (Array.to_list pages) in
      print_endline dune_rules;
      write_to_file utopia_dune_file dune_rules
