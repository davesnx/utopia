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
  $ cat _utopia/routes.manifest
  about	code	pages/About.re	about			false	false
  home	code	pages/Home.re	home			true	false
  $ grep "metadata" _utopia/server_main.ml
      ~layouts:[] ~render:about_make_page ~metadata:None
      ~layouts:[] ~render:home_make_page ~metadata:(Some Pages__Home.metadata)
