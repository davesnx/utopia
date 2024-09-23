open Lwt.Syntax
module Body = Cohttp_lwt.Body
module Server = Cohttp_lwt_unix.Server

(* Used the same layout as Utopia.Loader_page.layout *)
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

module Router = struct
  open Ppx_deriving_router_runtime.Primitives

  type t =
    | Home [@GET "/"]
    | About [@GET "/about"]
    | Blog [@GET "/blog"]
    | Blog_post of { slug : string; wat : string } [@GET "/blog/:slug"]
    | Work [@GET "/work"]
    | Talks [@GET "/talks"]
  [@@deriving router]
end

module Pages = struct
  let home () =
    let layout = Html.make in
    let component = (div ~children:[] ~key:"html" () [@JSX]) in
    render_html_page ~title:"Home" ~layout component

  let about () =
    let layout = Html.make in
    let component = (div ~children:[] ~key:"html" () [@JSX]) in
    render_html_page ~title:"About" ~layout component

  let blog () =
    let layout = Html.make in
    let component = (div ~children:[] () [@JSX]) in
    render_html_page ~title:"Blog" ~layout component

  let blog_post ~slug () =
    let layout = Html.make in
    let component = (div ~children:[] () [@JSX]) in
    render_html_page ~title:"Blog" ~layout component

  let work () =
    let layout = Html.make in
    let component = (div ~children:[] () [@JSX]) in
    render_html_page ~title:"Work" ~layout component

  let talks () =
    let layout = Html.make in
    let component = (div ~children:[] () [@JSX]) in
    render_html_page ~title:"Talks" ~layout component
end

let bootstrap =
  Printexc.record_backtrace true;
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());

  let callback (_connection : Server.conn) request body =
    Router.handle
      (fun route _request ->
        match route with
        | Router.Home ->
            let body = Pages.home () in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
        | Router.About ->
            let body = Pages.about () in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
        | Router.Blog ->
            let body = Pages.blog () in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
        | Router.Blog_post { slug } ->
            let body = Pages.blog_post ~slug () in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
        | Router.Work ->
            let body = Pages.work () in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()
        | Router.Talks ->
            let body = Pages.talks () in
            Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ())
      (request, body)
  in
  let port = 8080 in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () = Lwt_main.run bootstrap
