  $ mkdir pages _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Static.ml <<'EOF'
  > let makeProps () = ()
  > let make () = React.string "static by default"
  > EOF
  $ cat > pages/Dynamic.ml <<'EOF'
  > let before request = request
  > let makeProps () = ()
  > let make () = React.string "dynamic via before"
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -F 'source_file = "pages/Static.ml"' _utopia/Routes.ml | rg -o 'static = true'
  static = true
  $ grep -F 'source_file = "pages/Dynamic.ml"' _utopia/Routes.ml | rg -o 'static = false'
  static = false
