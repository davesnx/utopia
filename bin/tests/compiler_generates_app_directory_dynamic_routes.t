  $ mkdir -p app/notes/[tag] "app/docs/[...slug]" "app/help/[[...path]]" _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/page.re
  $ printf "let before request = request\nlet page = ()\n" > app/notes/[tag]/page.re
  $ printf "let before request = request\nlet page = ()\n" > "app/docs/[...slug]/page.re"
  $ printf "let before request = request\nlet page = ()\n" > "app/help/[[...path]]/page.re"
  $ utopia.compiler > /dev/null
  $ grep -qF 'source_file = "app/notes/[tag]/page.re"' _utopia/Routes.ml
  $ grep -qF 'source_file = "app/docs/[...slug]/page.re"' _utopia/Routes.ml
  $ grep -qF 'source_file = "app/help/[[...path]]/page.re"' _utopia/Routes.ml
