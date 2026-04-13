  $ mkdir -p app/blog _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/blog/page.md <<'EOF'
  > ---
  > title: Hello
  > description: App markdown page
  > ---
  > 
  > # Body
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -qF 'source_file = "app/blog/page.md"' _utopia/Routes.ml
  $ grep -qF 'frontmatter_object_of_list' _utopia/Routes.ml
