  $ mkdir -p app/about app/api/users/[id] _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let x = 1\n" > app/button.re
  $ printf "let page = ()\n" > app/page.re
  $ printf "let page = ()\n" > app/about/page.re
  $ printf "let middleware next request = next request\n" > app/api/_middleware.ml
  $ printf "let middleware next request = next request\n" > app/api/users/_middleware.ml
  $ printf "let handler (_request : Dream.request) = Utopia_server.respond \"{\\\"ok\\\":true}\"\n" > app/api/users/[id]/route.ml
  $ utopia.compiler > /dev/null
  $ grep -qF 'source_file = "app/page.re";' _utopia/Routes.ml
  $ grep -qF 'source_file = "app/about/page.re";' _utopia/Routes.ml
  $ grep -F 'route = "api/users/[id]"; matcher = "api/users/:id"; conflict_key = "api/users/:";' _utopia/Routes.ml
      ({ route = "api/users/[id]"; matcher = "api/users/:id"; conflict_key = "api/users/:"; params = [("id", Utopia_types.Single)]; middlewares = ["app/api/_middleware.ml"; "app/api/users/_middleware.ml"]; source_file = "app/api/users/[id]/route.ml"; module_name = "Api__Users__Id__Route" } : Utopia_types.api_route_meta);
  $ ! grep -qF 'source_file = "app/button.re";' _utopia/Routes.ml
  $ grep -qF '(deps ../app/page.re)' _utopia/dune
  $ grep -qF '(deps ../app/button.re)' _utopia/dune
  $ grep -qF 'Pages__Button' _utopia/dune
  $ grep -qF '(deps ../../app/api/users/[id]/route.ml)' _utopia/dune
