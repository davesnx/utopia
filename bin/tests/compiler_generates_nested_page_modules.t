  $ mkdir -p pages/about/boo _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let layout = ()\n" > pages/layout.re
  $ printf "let page = ()\n" > pages/about/Team.re
  $ printf "let page = ()\n" > pages/about/boo/index.re
  $ utopia.compiler > /dev/null
  $ grep -qF 'target Pages__About__Team.re' _utopia/dune
  $ grep -qF 'target Pages__About__Boo__Index.re' _utopia/dune
  $ grep -qF 'target Pages__Layout.re' _utopia/dune
  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -qF 'modules Pages__About__Team Pages__About__Boo__Index Pages__Layout client_entry_melange'
  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -qF '(library (name utopia_'
  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -qF 'modules Routes'
