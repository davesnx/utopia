  $ mkdir -p pages/post _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > 'pages/post/[id].ml' <<'EOF'
  > let paths () = List.init 500 (fun i -> [("id", string_of_int i)])
  > let makeProps () = ()
  > let make () =
  >   React.createElement "div" []
  >     [ React.createElement "h1" [] [React.string "Post title"];
  >       React.createElement "p" [] [React.string "content"] ]
  > EOF
  $ cat > pages/About.ml <<'EOF'
  > let makeProps () = ()
  > let make () = React.string "about page"
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
Parallel SSG renders all pages correctly:
  $ _build/default/_utopia/server_main.exe --ssg > /dev/null
  $ find _utopia/static -name '*.html' | wc -l
  501
  $ test -f _utopia/static/post/0.html && echo first-ok
  first-ok
  $ test -f _utopia/static/post/499.html && echo last-ok
  last-ok
  $ test -f _utopia/static/about.html && echo about-ok
  about-ok
Verify content is correct (not corrupted by parallel writes):
  $ grep -q 'Post title' _utopia/static/post/0.html && echo content-ok
  content-ok
  $ grep -q 'Post title' _utopia/static/post/250.html && echo content-ok
  content-ok
  $ grep -q 'about page' _utopia/static/about.html && echo content-ok
  content-ok
