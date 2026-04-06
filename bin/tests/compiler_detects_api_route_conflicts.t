  $ mkdir -p pages api/users _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ printf "let handler _request = Utopia_server.respond \"{}\"\n" > api/users.ml
  $ printf "let handler _request = Utopia_server.respond \"{}\"\n" > api/users/index.ml
  $ utopia.compiler 2>&1 | rg 'API route conflicts detected|/api/users|api/users.ml|api/users/index.ml'
    API route conflicts detected:
      - /api/users has 2 competing API files:
          * api/users.ml
          * api/users/index.ml
