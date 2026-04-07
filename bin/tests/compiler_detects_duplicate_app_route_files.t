  $ mkdir -p app app/api/users _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/page.re
  $ printf "let handler _request = Utopia_server.respond \"{}\"\n" > app/api/users/route.re
  $ printf "let handler _request = Utopia_server.respond \"{}\"\n" > app/api/users/route.ml
  $ utopia.compiler 2>&1 | rg -F 'Duplicate route files in app/api/users: app/api/users/route.ml, app/api/users/route.re'
      - Duplicate route files in app/api/users: app/api/users/route.ml, app/api/users/route.re
