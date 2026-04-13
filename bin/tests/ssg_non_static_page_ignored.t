  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.ml <<'EOF'
  > let before request = request
  > let makeProps () = ()
  > let make () = React.string "non-static"
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -A 12 -F 'source_file = "pages/Home.ml"' _utopia/Routes.ml | grep -qF 'static = false;'
  $ grep -A 12 -F 'source_file = "pages/Home.ml"' _utopia/Routes.ml | grep -qF 'has_paths = false'
  $ dune build _utopia/server_main.exe > /dev/null
  $ _build/default/_utopia/server_main.exe --ssg > /dev/null
  $ test ! -f _utopia/static/home.html && echo no-static-output
  no-static-output
