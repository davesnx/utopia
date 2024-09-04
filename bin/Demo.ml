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

(* module Pages = struct
     open Ppx_deriving_router_runtime

     type t =
       | Home [@GET "/"]
       | About
       | Hello of { name : string; repeat : int option } [@GET "/hello/:name"]
     [@@deriving router]
   end *)

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
(* let text = " lol" in
   let handler _socket request _body =
     match Http.Request.resource request with
     | "/" -> Cohttp_eio.Server.respond_string ~status:`OK ~body:text ()
     | "/html" ->
         (* Use a plain flow to test chunked encoding *)
         let body = Eio.Flow.string_source text in
         Cohttp_eio.Server.respond () ~status:`OK
           ~headers:(Http.Header.of_list [ ("content-type", "text/html") ])
           ~body
     | _ -> Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"" ()
   in

   let port = 8080 in
   let socket =
     Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
       (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
   and server = Cohttp_eio.Server.make ~callback:handler () in
   Cohttp_eio.Server.run socket server ~on_error:(fun ex ->
       Logs.warn (fun f -> f "%a" Eio.Exn.pp ex)) *)

let () = bootstrap ()
