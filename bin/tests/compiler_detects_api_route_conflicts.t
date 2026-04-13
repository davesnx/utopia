  $ mkdir -p app/api/users/[id] app/api/users/[slug] _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/page.re
  $ printf "let handler _request = Utopia_server.respond \"{}\"\n" > app/api/users/[id]/route.ml
  $ printf "let handler _request = Utopia_server.respond \"{}\"\n" > app/api/users/[slug]/route.ml
  $ utopia.compiler > compiler.log 2>&1 || true
  $ rg -qF 'API route conflicts detected:' compiler.log
  $ rg -qF '/api/users/[id] has 2 competing API files:' compiler.log
  $ rg -qF 'app/api/users/[id]/route.ml' compiler.log
  $ rg -qF 'app/api/users/[slug]/route.ml' compiler.log
