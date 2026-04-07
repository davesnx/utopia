  $ mkdir -p app/health _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/page.re
  $ printf "let handler _request = Utopia_server.respond \"{}\"\n" > app/health/route.ml
  $ utopia.compiler 2>&1 | rg -F 'Invalid app route declaration: app/health/route.ml is a route file outside app/api/**'
      - Invalid app route declaration: app/health/route.ml is a route file outside app/api/**
