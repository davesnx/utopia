(* Used the same layout as Utopia.Loader_page.layout *)
(* type layout =
   ?key:string ->
   title:string ->
   scripts:React.element list ->
   children:React.element ->
   unit ->
   React.element *)

(* let render_html_page ~title ?(layout : layout = Html.make) children =
   let component : React.element =
     layout ~key:"html" ~title ~scripts:[] ~children ()
   in
   let output = ReactDOM.renderToStaticMarkup component in
   Printf.sprintf "<!DOCTYPE html>%s" output *)

(* render_html_page ~title:"Home" ~layout component *)

let () =
  Printexc.record_backtrace true;
  Logs.set_level (Some Info);
  Logs.set_reporter (Logs_fmt.reporter ());
  let router = [] in
  Dream.run @@ Dream.logger @@ Dream.router router
