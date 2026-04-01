  $ mkdir pages _utopia
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
  $ cat _utopia/routes.manifest
  about	code	pages/About.re	about			false
  home	code	pages/Home.re	home			true
  $ grep "metadata" _utopia/server_main.ml
    Utopia_server.Generated_route.code ~route:"about" ~matcher:"about" ~params:[] ~source_file:"pages/About.re" ~layouts:[] ~render:(fun () -> Utopia_server.wrap_raw_inner_html_element (Utopia_page__About.make ())) ~metadata:None ~layout_renderers:[] ~router_shell:about_router_shell ~router_tree:about_router_tree ~router_subtree:about_router_subtree;
    Utopia_server.Generated_route.code ~route:"home" ~matcher:"home" ~params:[] ~source_file:"pages/Home.re" ~layouts:[] ~render:(fun () -> Utopia_server.wrap_raw_inner_html_element (Utopia_page__Home.make ())) ~metadata:(Some Utopia_page__Home.metadata) ~layout_renderers:[] ~router_shell:home_router_shell ~router_tree:home_router_tree ~router_subtree:home_router_subtree;
