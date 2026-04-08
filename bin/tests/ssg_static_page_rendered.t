  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/About.ml <<'EOF'
  > let makeProps () = ()
  > let make () = React.string "about static"
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
  $ _build/default/_utopia/server_main.exe --ssg > /dev/null
  $ test -f _utopia/static/about.html && echo rendered
  rendered
  $ rg -m1 -o 'about static' _utopia/static/about.html
  about static
