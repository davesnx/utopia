  $ mkdir -p pages/about _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ printf "let page = ()\n" > pages/about/Team.re
  $ printf "# hello\n" > pages/Guide.md
  $ utopia.compiler > /dev/null
  $ grep -E "let page_meta = Routes.get_all \(\)|let markdown_meta = Routes.Markdown.get_all \(\)|let api_meta = Routes.Api.get_all \(\)|let pages = Route_modules.resolve_pages markdown_meta page_meta|let api_routes = Route_modules.resolve_api api_meta|let argv = Array.to_list Sys.argv|if List.mem \"--ssg\" argv then Utopia_server.ssg_generated pages|let dev_mode = List.mem \"--dev\" argv|Utopia_server.start_generated ~pages ~api_routes|~lookup_server_function:FunctionReferences.get ~dev_mode \(\)" _utopia/server_main.ml
  let page_meta = Routes.get_all ()
  let markdown_meta = Routes.Markdown.get_all ()
  let api_meta = Routes.Api.get_all ()
  let pages = Route_modules.resolve_pages markdown_meta page_meta
  let api_routes = Route_modules.resolve_api api_meta
    let argv = Array.to_list Sys.argv in
    if List.mem "--ssg" argv then Utopia_server.ssg_generated pages
      let dev_mode = List.mem "--dev" argv in
      Utopia_server.start_generated ~pages ~api_routes
        ~lookup_server_function:FunctionReferences.get ~dev_mode ()
