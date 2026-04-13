  $ mkdir -p app/dynamic _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/page.ml <<'EOF'
  > let makeProps () = ()
  > let make () = React.string "static by default"
  > EOF
  $ cat > app/dynamic/page.ml <<'EOF'
  > let before request = request
  > let makeProps () = ()
  > let make () = React.string "dynamic via before"
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -A 12 -F 'source_file = "app/page.ml"' _utopia/Routes.ml | grep -qF 'static = true;'
  $ grep -A 12 -F 'source_file = "app/dynamic/page.ml"' _utopia/Routes.ml | grep -qF 'static = false;'
