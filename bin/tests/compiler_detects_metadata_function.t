  $ mkdir pages _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > let metadata _params =
  >   { Utopia_types.title = Some "My Home";
  >     description = Some "Welcome to the site." };
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ printf "let page = ()\n" > pages/About.re
  $ utopia.compiler > /dev/null
  $ grep -qF 'source_file = "pages/About.re"' _utopia/Routes.ml
  $ grep -qF 'module_name = "Pages__About"' _utopia/Routes.ml
  $ grep -qF 'has_metadata = false;' _utopia/Routes.ml
  $ grep -qF 'source_file = "pages/Home.re"' _utopia/Routes.ml
  $ grep -qF 'module_name = "Pages__Home"' _utopia/Routes.ml
  $ grep -qF 'has_metadata = true;' _utopia/Routes.ml
  $ grep -qF '("pages/Home.re", Pages__Home.metadata)' _utopia/Routes_server.ml
