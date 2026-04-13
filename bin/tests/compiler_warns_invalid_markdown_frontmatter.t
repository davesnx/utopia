  $ mkdir pages _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Bad.md <<'EOF'
  > ---
  > title: "broken
  > ---
  > 
  > # Keep me
  > EOF
  $ utopia.compiler > /tmp/compiler.out 2> /tmp/compiler.err
  $ cat /tmp/compiler.out /tmp/compiler.err | rg -qF 'invalid YAML frontmatter'
  $ grep -qF 'source_file = "pages/Bad.md"' _utopia/Routes.ml
