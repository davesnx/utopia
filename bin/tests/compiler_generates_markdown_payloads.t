  $ mkdir -p app/post _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/post/page.md <<'EOF'
  > ---
  > title: Hello
  > description: Demo post
  > author: Ada
  > ---
  > 
  > # Body
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -qF 'module Markdown =' _utopia/Routes.ml
  $ grep -qF 'frontmatter:' _utopia/Routes.ml
  $ grep -qF 'source_file = "app/post/page.md"' _utopia/Routes.ml
  $ grep -qF 'frontmatter_object_of_list' _utopia/Routes.ml
  $ ! grep -qF 'Generated_markdown_entry.make' _utopia/Routes_server.ml
  $ ! grep -qF 'resolve_generated_pages registry' _utopia/Routes_server.ml
