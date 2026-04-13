  $ mkdir -p app/about _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/page.re <<'EOF'
  > let metadata _params =
  >   { Utopia_types.title = Some "My Home";
  >     description = Some "Welcome to the site." };
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ printf "let page = ()\n" > app/about/page.re
  $ utopia.compiler > /dev/null
  $ grep -qF 'source_file = "app/about/page.re"' _utopia/Routes.ml
  $ grep -qF 'module_name = "Pages__About__Page"' _utopia/Routes.ml
  $ grep -qF 'has_metadata = false;' _utopia/Routes.ml
  $ grep -qF 'source_file = "app/page.re"' _utopia/Routes.ml
  $ grep -qF 'module_name = "Pages__Page"' _utopia/Routes.ml
  $ grep -qF 'has_metadata = true;' _utopia/Routes.ml
  $ grep -qF '("app/page.re", Pages__Page.metadata)' _utopia/Routes_server.ml
