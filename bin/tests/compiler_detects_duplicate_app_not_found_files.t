  $ mkdir -p app _utopia
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ printf "let page = ()\n" > app/page.re
  $ cat > app/not-found.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("not found re")} </div>;
  > EOF
  $ cat > app/not-found.ml <<'EOF'
  > let make () = React.null
  > EOF
  $ utopia.compiler 2>&1 | rg -F 'Duplicate not-found files'
      - Duplicate not-found files in app/: both app/not-found.ml and app/not-found.re define not-found
