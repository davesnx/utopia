  $ mkdir -p pages/about _utopia
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ printf "let page = ()\n" > pages/about/Team.re
  $ printf "# hello\n" > pages/Guide.md
  $ utopia.compiler > /dev/null
  $ grep -E "let about__team_router_tree|Utopia\.Router\.Boundary\.make ~path:\"/about\"|Utopia\.Router\.Boundary\.make ~path:\"/about/team\"|Utopia\.make ~initialPath:location|\| \"/\" -> Some \(Utopia\.Router\.Boundary\.make ~path:\"/about\"|\| \"/about\" -> Some \(Utopia\.Router\.Boundary\.make ~path:\"/about/team\"|render_markdown_body \"pages/Guide\.md\"|\| \"/\" -> Some \(Utopia\.Router\.Boundary\.make ~path:\"/guide\"|~router_shell:about__team_router_shell|~router_tree:guide_router_tree|lookup_server_function:FunctionReferences.get" _utopia/server_main.ml
  let about__team_router_tree () =
    Utopia.Router.Boundary.make ~path:"/" ~layout:(Utopia.PassThroughLayout.make ~children:(Utopia.Router.Boundary.PageConsumer.make ()) ()) ~pageconsumer:(Some (Utopia.Router.Boundary.make ~path:"/about" ~layout:(Utopia.PassThroughLayout.make ~children:(Utopia.Router.Boundary.PageConsumer.make ()) ()) ~pageconsumer:(Some (Utopia.Router.Boundary.make ~path:"/about/team" ~layout:(Utopia_server.wrap_raw_inner_html_element (About__Team.make ())) ~pageconsumer:None ())) ())) ()
    Utopia.make ~initialPath:location ~children:(about__team_router_tree ()) ()
    | "/" -> Some (Utopia.Router.Boundary.make ~path:"/about" ~layout:(Utopia.PassThroughLayout.make ~children:(Utopia.Router.Boundary.PageConsumer.make ()) ()) ~pageconsumer:(Some (Utopia.Router.Boundary.make ~path:"/about/team" ~layout:(Utopia_server.wrap_raw_inner_html_element (About__Team.make ())) ~pageconsumer:None ())) ())
    | "/about" -> Some (Utopia.Router.Boundary.make ~path:"/about/team" ~layout:(Utopia_server.wrap_raw_inner_html_element (About__Team.make ())) ~pageconsumer:None ())
    Utopia.Router.Boundary.make ~path:"/" ~layout:(Utopia.PassThroughLayout.make ~children:(Utopia.Router.Boundary.PageConsumer.make ()) ()) ~pageconsumer:(Some (Utopia.Router.Boundary.make ~path:"/guide" ~layout:(Utopia_server.render_markdown_body "pages/Guide.md") ~pageconsumer:None ())) ()
    Utopia.make ~initialPath:location ~children:(guide_router_tree ()) ()
    | "/" -> Some (Utopia.Router.Boundary.make ~path:"/guide" ~layout:(Utopia_server.render_markdown_body "pages/Guide.md") ~pageconsumer:None ())
    Utopia.make ~initialPath:location ~children:(home_router_tree ()) ()
    Utopia_server.Generated_route.code ~route:"about/team" ~matcher:"about/team" ~params:[] ~source_file:"pages/about/Team.re" ~layouts:[] ~render:(fun () -> Utopia_server.wrap_raw_inner_html_element (About__Team.make ())) ~layout_renderers:[] ~router_shell:about__team_router_shell ~router_tree:about__team_router_tree ~router_subtree:about__team_router_subtree;
    Utopia_server.Generated_route.markdown ~route:"guide" ~matcher:"guide" ~params:[] ~source_file:"pages/Guide.md" ~layouts:[]  ~layout_renderers:[] ~router_shell:guide_router_shell ~router_tree:guide_router_tree ~router_subtree:guide_router_subtree;
  let () = Utopia_server.start_generated generated_routes ~lookup_server_function:FunctionReferences.get
