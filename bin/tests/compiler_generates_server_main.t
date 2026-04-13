  $ mkdir -p app/about app/guide _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/page.re
  $ printf "let page = ()\n" > app/about/page.re
  $ printf "# hello\n" > app/guide/page.md
  $ utopia.compiler > /dev/null
  $ grep -qF 'run_generated_routes_server_cli' _utopia/server_main.ml
  $ grep -qF '(module Routes_server)' _utopia/server_main.ml
  $ grep -qF '~lookup_server_function:FunctionReferences.get ()' _utopia/server_main.ml
  $ grep -qF 'include Routes' _utopia/Routes_server.ml
  $ grep -qF 'let page_renders =' _utopia/Routes_server.ml
  $ grep -qF 'let page_metadata =' _utopia/Routes_server.ml
  $ grep -qF 'let page_paths =' _utopia/Routes_server.ml
  $ grep -qF 'let api_handlers =' _utopia/Routes_server.ml
  $ grep -qF 'let api_middlewares =' _utopia/Routes_server.ml
  $ grep -qF 'let not_found_page = None' _utopia/Routes_server.ml
  $ grep -qF 'include Routes_client' _utopia/Routes.ml
  $ ! grep -qF 'resolve_generated_pages' _utopia/Routes_server.ml
  $ ! grep -qF 'resolve_generated_api' _utopia/Routes_server.ml
  $ ! test -f _utopia/Generated_server_registry.ml
