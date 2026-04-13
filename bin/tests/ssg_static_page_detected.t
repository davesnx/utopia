  $ mkdir -p app/home app/dynamic _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/home/page.re <<'EOF'
  > let metadata _params = Utopia_types.empty_metadata
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ cat > app/dynamic/page.re <<'EOF'
  > let before = request => request
  > [@react.component]
  > let make = () => <div> {React.string("dynamic")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -A 12 -F 'source_file = "app/home/page.re"' _utopia/Routes.ml | grep -qF 'module_name = "Pages__Home__Page"'
  $ grep -A 12 -F 'source_file = "app/home/page.re"' _utopia/Routes.ml | grep -qF 'has_metadata = true;'
  $ grep -A 12 -F 'source_file = "app/home/page.re"' _utopia/Routes.ml | grep -qF 'static = true;'
  $ grep -A 12 -F 'source_file = "app/dynamic/page.re"' _utopia/Routes.ml | grep -qF 'module_name = "Pages__Dynamic__Page"'
  $ grep -A 12 -F 'source_file = "app/dynamic/page.re"' _utopia/Routes.ml | grep -qF 'has_metadata = false;'
  $ grep -A 12 -F 'source_file = "app/dynamic/page.re"' _utopia/Routes.ml | grep -qF 'static = false;'
