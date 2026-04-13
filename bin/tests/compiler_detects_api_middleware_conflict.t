  $ mkdir -p app/api/health _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/page.re
  $ printf "let middleware next request = next request\n" > app/api/_middleware.ml
  $ printf "let middleware next request = next request\n" > app/api/_middleware.re
  $ printf "let handler _request = Utopia_server.respond \"{}\"\n" > app/api/health/route.ml
  $ utopia.compiler > compiler.log 2>&1 || true
  $ rg -qF 'Invalid API declarations:' compiler.log
  $ rg -qF 'Middleware conflict in app/api/: both app/api/_middleware.ml and app/api/_middleware.re define _middleware' compiler.log
