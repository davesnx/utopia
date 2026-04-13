  $ mkdir -p app/about/boo _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let layout = ()\n" > app/layout.re
  $ printf "let page = ()\n" > app/about/page.re
  $ printf "let page = ()\n" > app/about/boo/page.re
  $ utopia.compiler > /dev/null
  $ grep -qF 'target Pages__About__Page.re' _utopia/dune
  $ grep -qF 'target Pages__About__Boo__Page.re' _utopia/dune
  $ grep -qF 'target Pages__Layout.re' _utopia/dune
  $ grep -qF '(library (name pages_' _utopia/dune
  $ grep -qF '(modules Routes Routes_client)' _utopia/dune
