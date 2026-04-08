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
  $ grep -F 'source_file = "pages/About.re"; module_name = "Pages__About"; has_metadata = false;' _utopia/Routes.ml
    ({ route = "about"; matcher = "about"; conflict_key = "about"; params = []; layouts = []; kind = Utopia_types.Code_page; source_file = "pages/About.re"; module_name = "Pages__About"; has_metadata = false; static = true; has_paths = false } : Utopia_types.page_route_meta);
  $ grep -F 'source_file = "pages/Home.re"; module_name = "Pages__Home"; has_metadata = true;' _utopia/Routes.ml
    ({ route = "home"; matcher = "home"; conflict_key = "home"; params = []; layouts = []; kind = Utopia_types.Code_page; source_file = "pages/Home.re"; module_name = "Pages__Home"; has_metadata = true; static = true; has_paths = false } : Utopia_types.page_route_meta);
  $ grep -F '| "pages/Home.re" -> Some Pages__Home.metadata' _utopia/server_main.ml
      | "pages/Home.re" -> Some Pages__Home.metadata
