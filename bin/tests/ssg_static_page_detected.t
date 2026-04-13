  $ mkdir pages _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > let metadata _params = Utopia_types.empty_metadata
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ cat > pages/Dynamic.re <<'EOF'
  > let before = request => request
  > [@react.component]
  > let make = () => <div> {React.string("dynamic")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -A 12 -F 'source_file = "pages/Home.re"' _utopia/Routes.ml | grep -qF 'module_name = "Pages__Home"'
  $ grep -A 12 -F 'source_file = "pages/Home.re"' _utopia/Routes.ml | grep -qF 'has_metadata = true;'
  $ grep -A 12 -F 'source_file = "pages/Home.re"' _utopia/Routes.ml | grep -qF 'static = true;'
  $ grep -A 12 -F 'source_file = "pages/Dynamic.re"' _utopia/Routes.ml | grep -qF 'module_name = "Pages__Dynamic"'
  $ grep -A 12 -F 'source_file = "pages/Dynamic.re"' _utopia/Routes.ml | grep -qF 'has_metadata = false;'
  $ grep -A 12 -F 'source_file = "pages/Dynamic.re"' _utopia/Routes.ml | grep -qF 'static = false;'
