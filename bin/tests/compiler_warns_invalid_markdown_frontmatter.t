  $ mkdir -p app/bad _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/bad/page.md <<'EOF'
  > ---
  > title: "broken
  > ---
  > 
  > # Keep me
  > EOF
  $ utopia.compiler > /tmp/compiler.out 2> /tmp/compiler.err
  $ cat /tmp/compiler.out /tmp/compiler.err | rg -qF 'invalid YAML frontmatter'
  $ grep -qF 'source_file = "app/bad/page.md"' _utopia/Routes.ml
