  $ mkdir -p app/api/users/[id] _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/page.re
  $ printf "let middleware next request = next request\n" > app/api/_middleware.ml
  $ printf "let middleware next request = next request\n" > app/api/users/_middleware.ml
  $ printf "let handler (_request : Dream.request) = Utopia_server.respond \"{\\\"ok\\\":true}\"\n" > app/api/users/[id]/route.ml
  $ utopia.compiler > /dev/null
  $ grep -qF 'let get_all ()' _utopia/Routes.ml
  $ grep -qF 'module Api =' _utopia/Routes.ml
  $ grep -qF 'route = "api/users/[id]"' _utopia/Routes.ml
  $ grep -qF 'let id (request : Dream.request) =' _utopia/Routes.ml
  $ grep -qF 'app/api/users/[id]/route.ml' _utopia/dune
  $ grep -qF 'Api__Users__Id__Route.ml' _utopia/dune
  $ grep -qE '\(name api_' _utopia/dune
  $ ! test -f _utopia/routes.manifest
  $ ! test -f _utopia/api.manifest
