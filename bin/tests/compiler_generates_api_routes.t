  $ mkdir -p pages api/users _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ printf "let middleware next request = next request\n" > api/_middleware.ml
  $ printf "let middleware next request = next request\n" > api/users/_middleware.ml
  $ printf "let handler (_request : Dream.request) = Utopia_server.respond \"{\\\"ok\\\":true}\"\n" > api/users/[id].ml
  $ utopia.compiler > /dev/null
  $ grep -qF 'let get_all ()' _utopia/Routes.ml
  $ grep -qF 'module Api =' _utopia/Routes.ml
  $ grep -qF 'route = "api/users/[id]"' _utopia/Routes.ml
  $ grep -qF 'let id (request : Dream.request) =' _utopia/Routes.ml
  $ grep -qF '(rule (deps ../../api/users/[id].ml) (target Api__Users__Id.ml)' _utopia/dune
  $ grep -qE '\(name api_' _utopia/dune
  $ ! test -f _utopia/routes.manifest
  $ ! test -f _utopia/api.manifest
