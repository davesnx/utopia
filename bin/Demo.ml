open Lwt.Syntax
module Body = Cohttp_lwt.Body
module Server = Cohttp_lwt_unix.Server

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

let bootstrap =
  Printexc.record_backtrace true;
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());

  let callback (_connection : Server.conn) request body =
    Router.handle
      (fun route req ->
        match route with
        | Router.Home ->
            let body =
              render_html_page ~title:"Home" ~layout:Html.make
                (div ~children:[] ~key:"html" () [@JSX])
            in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
        | Router.About ->
            let body =
              render_html_page ~title:"About" ~layout:Html.make
                (div ~children:[] ~key:"html" () [@JSX])
            in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
        | Router.Hello { name } ->
            let body =
              render_html_page ~title:"Home" ~layout:Html.make
                (div ~children:[] ~key:"html" () [@JSX])
            in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ())
      (request, body)
  in
  let port = 8080 in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () = Lwt_main.run bootstrap
