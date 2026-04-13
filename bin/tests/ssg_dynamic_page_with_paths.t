  $ mkdir -p app/blog/[slug] _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > 'app/blog/[slug]/page.ml' <<'EOF'
  > let paths () =
  >   [ [ ("slug", "hello-world") ]; [ ("slug", "second-post") ] ]
  > let makeProps () = ()
  > let make () = React.string "blog post"
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
  $ _build/default/_utopia/server_main.exe --ssg > /dev/null
  $ test -f _utopia/static/blog/hello-world.html && echo hello-rendered
  hello-rendered
  $ test -f _utopia/static/blog/second-post.html && echo second-rendered
  second-rendered
