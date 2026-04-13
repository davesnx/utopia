  $ mkdir app _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/page.re <<'EOF'
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
  $ grep -A 12 -F 'source_file = "app/page.re"' _utopia/Routes.ml | grep -qF 'static = true;'
  $ grep -A 12 -F 'source_file = "app/page.re"' _utopia/Routes.ml | grep -qF 'has_paths = false'
