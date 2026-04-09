Server-only pages (no [@react.client.component]) are excluded from melange:

  $ mkdir -p pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ cat > pages/About.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("about")} </div>;
  > EOF
  $ cat > pages/Settings.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("settings")} </div>;
  > EOF
  $ utopia.compiler > /dev/null

Melange modules should NOT include any page modules (no client components):

  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'melange\.emit.*?modules \K[^)]+' | tr ' ' '\n' | sort
  client_entry_melange

No melange copy rules for pages:

  $ ! grep -qF '(deps ../pages/Home.re) (target Pages__Home.re)' _utopia/dune
  $ ! grep -qF '(deps ../pages/About.re) (target Pages__About.re)' _utopia/dune

Native library still includes all pages:

  $ grep -qF 'Pages__Home' _utopia/dune
  $ grep -qF 'Pages__About' _utopia/dune
  $ grep -qF 'Pages__Settings' _utopia/dune
