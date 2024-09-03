let write_to_file path content =
  Eio.Path.with_open_out ~create:(`Exclusive 0o600) path @@ fun flow ->
  Eio.Flow.copy_string content flow

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

module Pages = struct
  open Ppx_deriving_router_runtime

  type t =
    | Home [@GET "/"]
    | About
    | Hello of { name : string; repeat : int option } [@GET "/hello/:name"]
  [@@deriving router]
end

let () =
  empty_folder "_utopia";

  Eio_main.run @@ fun env ->
  (* let clock = Eio.Stdenv.clock env in *)
  let ( / ) = Eio.Path.( / ) in
  let cwd = Eio.Stdenv.fs env in
  Utopia.page ~path:"index" (fun () ->
      (div ~children:[ React.string "Static page" ] () [@JSX]));

  Utopia.register ~path:"home"
    ~loader:(fun () -> "home")
    (fun data -> (div ~children:[ React.string ("Hello " ^ data) ] () [@JSX]));

  Utopia.register ~path:"users"
    ~loader:(fun () -> ())
    (fun _ -> (div ~children:[ React.string "This page is slow!" ] () [@JSX]));

  Array.make 500 "mock_page"
  |> Array.iteri (fun index fixture ->
         Eio.traceln "Register page: %d" index;
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

  Eio.traceln "Number of pages: %d" (Seq.length pages);

  let utopia_artifacts_folder = cwd / "_utopia" in

  let generate_page (module Page : Utopia.Loader_page) =
    let file = utopia_artifacts_folder / (Page.path ^ ".html") in
    Eio.traceln "Rendering page: %s" Page.path;
    let data = Page.loader () in
    let content = render_html_page ~title:Page.path (Page.make data) in
    write_to_file file content
  in

  let treshold = 1024 in

  let split_at n lst =
    let rec aux n lst acc =
      if n <= 0 then (List.rev acc, lst)
      else
        match lst with
        | [] -> (List.rev acc, [])
        | head :: tail -> aux (n - 1) tail (head :: acc)
    in
    aux n lst []
  in

  let split_list_into_max_size_lists lst max_size =
    let rec aux lst acc =
      match lst with
      | [] -> List.rev acc
      | _ ->
          let chunk, rest = split_at max_size lst in
          aux rest (chunk :: acc)
    in
    aux lst []
  in

  (* let fibers = pages |> Seq.map (fun p () -> generate_page p) in *)
  let list_of_pages = List.of_seq pages in
  let fibers = split_list_into_max_size_lists list_of_pages treshold in
  (* let fibers = List.fold_left (fun acc p ->
         if List.length acc >= treshold then
           acc @
     ) [] list_of_pages in *)
  (* Eio.Fiber.all new_fibers *)
  List.iter (fun p -> Eio.Fiber.List.iter (fun p -> generate_page p) p) fibers
