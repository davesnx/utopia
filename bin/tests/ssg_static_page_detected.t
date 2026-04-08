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
  $ grep -F 'source_file = "pages/Home.re"; module_name = "Pages__Home"; has_metadata = true; static = true; has_paths = false' _utopia/Routes.ml
    ({ route = "home"; matcher = "home"; conflict_key = "home"; params = []; layouts = []; kind = Utopia_types.Code_page; source_file = "pages/Home.re"; module_name = "Pages__Home"; has_metadata = true; static = true; has_paths = false } : Utopia_types.page_route_meta);
  $ grep -F 'source_file = "pages/Dynamic.re"; module_name = "Pages__Dynamic"; has_metadata = false; static = false; has_paths = false' _utopia/Routes.ml
    ({ route = "dynamic"; matcher = "dynamic"; conflict_key = "dynamic"; params = []; layouts = []; kind = Utopia_types.Code_page; source_file = "pages/Dynamic.re"; module_name = "Pages__Dynamic"; has_metadata = false; static = false; has_paths = false } : Utopia_types.page_route_meta);
