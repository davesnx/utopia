  $ mkdir -p app/about _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/about/page.re
  $ printf "let page = ()\n" > app/about/page.ml
  $ utopia.compiler 2>&1 | rg -F 'Duplicate page files in app/about: app/about/page.ml, app/about/page.re'
      - Duplicate page files in app/about: app/about/page.ml, app/about/page.re
