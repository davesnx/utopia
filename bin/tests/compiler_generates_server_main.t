  $ mkdir -p pages/about _utopia
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ printf "let page = ()\n" > pages/about/Team.re
  $ printf "# hello\n" > pages/Guide.md
  $ utopia.compiler > /dev/null
  $ grep -E "let about__team_make_page|let about__team_router|Utopia_route_builder\.build_router ~matcher:\"about/team\"|render_markdown_body \"pages/Guide\.md\"|Utopia_route_builder\.build_router ~matcher:\"guide\"|~router_shell:about__team_router\.Utopia_route_builder\.shell|~router_tree:guide_router\.Utopia_route_builder\.tree|lookup_server_function:FunctionReferences.get" _utopia/server_main.ml
  let about__team_make_page =
  let about__team_router =
    Utopia_route_builder.build_router ~matcher:"about/team" ~make_page:about__team_make_page ~layouts:about__team_layouts
    fun () -> Utopia_server.render_markdown_body "pages/Guide.md"
    Utopia_route_builder.build_router ~matcher:"guide" ~make_page:guide_make_page ~layouts:guide_layouts
      ~router_shell:about__team_router.Utopia_route_builder.shell
      ~router_tree:guide_router.Utopia_route_builder.tree
  let () = Utopia_server.start_generated generated_routes ~lookup_server_function:FunctionReferences.get
