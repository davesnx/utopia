  $ mkdir -p app pages api _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/page.re
  $ printf "let page = ()\n" > pages/Home.re
  $ printf "let handler _request = Utopia_server.respond \"{}\"\n" > api/health.ml
  $ utopia.compiler > /tmp/compiler.out 2> /tmp/compiler.err
  $ cat /tmp/compiler.out /tmp/compiler.err | rg -F 'Warning: app/ detected; ignoring legacy route roots: pages, api'
    Warning: app/ detected; ignoring legacy route roots: pages, api
  $ rg -qF 'source_file = "app/page.re"' _utopia/Routes.ml
  $ ! rg -F 'pages/Home.re' _utopia/Routes.ml
  $ ! rg -F 'api/health.ml' _utopia/Routes.ml
