  $ mkdir pages _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > let doc = "let before = request => request" ++ " and let paths () = []"
  > let char_sample = 'x'
  > // let before = request => request
  > // let paths () = []
  > (* let before = request => request *)
  > (* outer (* let before = request => request *) end *)
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -F 'source_file = "pages/Home.re"' _utopia/Routes.ml | rg -o 'static = true; has_paths = false'
  static = true; has_paths = false
