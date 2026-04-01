  $ mkdir -p pages/about/boo _utopia
  $ touch _utopia/dune
  $ printf "let layout = ()\n" > pages/layout.re
  $ printf "let page = ()\n" > pages/about/Team.re
  $ printf "let page = ()\n" > pages/about/boo/index.re
  $ utopia.compiler > /dev/null
  $ grep -qF 'target Utopia_page__About__Team.re' _utopia/dune
  $ grep -qF 'target Utopia_page__About__Boo__Index.re' _utopia/dune
  $ grep -qF 'target Utopia_page__Layout.re' _utopia/dune
  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -qF 'modules Utopia_page__About__Team Utopia_page__About__Boo__Index Utopia_page__Layout Utopia_routes Utopia Utopia_route Utopia_types ReactServerDOMEsbuild Utopia_router Utopia_router_route Utopia_router_link client_entry_melange'
  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -qF 'modules FunctionReferences Utopia Utopia_route Utopia_types Utopia_router Utopia_router_route Utopia_router_link Utopia_route_builder Utopia_routes Utopia_page__About__Team Utopia_page__About__Boo__Index Utopia_page__Layout'
