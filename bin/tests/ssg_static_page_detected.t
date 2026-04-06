  $ mkdir pages _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > let static = true
  > let metadata _params = Utopia_types.empty_metadata
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ printf "let page = ()\n" > pages/About.re
  $ utopia.compiler > /dev/null
  $ cat _utopia/routes.manifest
  about	code	pages/About.re	about			false	false
  home	code	pages/Home.re	home			true	true
