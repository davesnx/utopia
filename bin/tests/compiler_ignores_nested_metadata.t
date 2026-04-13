  $ mkdir app _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/page.re <<'EOF'
  > module Inner = {
  >   let metadata _params =
  >     { Utopia_types.title = Some "Inner";
  >       description = None };
  > };
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -qF 'source_file = "app/page.re"' _utopia/Routes.ml
  $ grep -qF 'module_name = "Pages__Page"' _utopia/Routes.ml
  $ grep -qF 'has_metadata = false;' _utopia/Routes.ml
