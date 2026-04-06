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
  $ grep -F 'source_file = "pages/Home.re"; module_name = "Pages__Home"; has_metadata = false;' _utopia/Routes.ml
    ({ route = "home"; matcher = "home"; conflict_key = "home"; params = []; layouts = []; kind = Utopia_types.Code_page; source_file = "pages/Home.re"; module_name = "Pages__Home"; has_metadata = false; static = false; has_static_paths = false } : Utopia_types.page_route_meta);
