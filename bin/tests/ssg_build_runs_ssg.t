  $ mkdir -p app/about _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ cat > package.json <<'EOF'
  > {"name":"utopia-test","private":true}
  > EOF
  $ touch _utopia/dune
  $ cat > app/about/page.ml <<'EOF'
  > let makeProps () = ()
  > let make () = React.string "built static"
  > EOF
  $ utopia export > /dev/null
  $ test -f _utopia/static/about.html && echo built-with-ssg
  built-with-ssg
