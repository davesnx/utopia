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
  $ rg -F 'invalid YAML frontmatter' /tmp/compiler.err
    Warning: markdown frontmatter warning (pages/Bad.md): invalid YAML frontmatter (error calling parser: found unexpected end of stream character 0 position 0 returned: 0); falling back to full markdown body
  $ rg -F 'source_file = "pages/Bad.md"; body = "---\ntitle: \"broken\n---\n\n# Keep me\n"' _utopia/Routes.ml
      ({ route = "bad"; matcher = "bad"; source_file = "pages/Bad.md"; body = "---\ntitle: \"broken\n---\n\n# Keep me\n"; frontmatter = None; title = None; description = None } : entry);
