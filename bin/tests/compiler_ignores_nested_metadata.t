  $ mkdir pages _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > module Inner = {
  >   let metadata _params =
  >     { Utopia_types.title = Some "Inner";
  >       description = None };
  > };
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ cat _utopia/routes.manifest
  home	code	pages/Home.re	home			false	false
