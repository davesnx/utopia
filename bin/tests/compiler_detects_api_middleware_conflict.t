  $ mkdir -p pages api _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ printf "let middleware next request = next request\n" > api/_middleware.ml
  $ printf "let middleware next request = next request\n" > api/_middleware.re
  $ printf "let handler _request = Utopia_server.respond \"{}\"\n" > api/health.ml
  $ utopia.compiler 2>&1 | rg 'Invalid API declarations|Middleware conflict in api/'
    Invalid API declarations:
      - Middleware conflict in api/: both api/_middleware.ml and api/_middleware.re define _middleware
