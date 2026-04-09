Projects with zero client components still have melange.emit (for runtime):

  $ mkdir -p pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ utopia.compiler > /dev/null

melange.emit is present with just the client entry runtime:

  $ grep -qF 'melange.emit' _utopia/dune
  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'melange\.emit.*?modules \K[^)]+' | tr ' ' '\n' | sort
  client_entry_melange

No melange copy rules for pages (only native has the page):

  $ grep -c '(target Pages__Home.re)' _utopia/dune
  1
