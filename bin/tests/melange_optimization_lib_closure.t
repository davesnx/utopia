Only lib modules reachable from client components are included in melange:

  $ mkdir -p pages lib _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > lib/Utils.re <<'EOF'
  > let format_label = (x) => "Label: " ++ x;
  > EOF
  $ cat > lib/Server_only.re <<'EOF'
  > let fetch_data = () => "db result";
  > EOF
  $ cat > pages/Home.re <<'EOF'
  > module Widget = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~label: string) => {
  >     <div> {React.string(Utils.format_label(label))} </div>;
  >   };
  > };
  > let data = Server_only.fetch_data();
  > [@react.component]
  > let make = () => <Widget label=data />;
  > EOF
  $ utopia.compiler > /dev/null

Melange includes Utils (used by client component) but not Server_only:

  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'melange\.emit.*?modules \K[^)]+' | tr ' ' '\n' | sort
  Lib
  Lib__Utils
  Pages__Home
  client_entry_melange

Melange Lib.re namespace includes only Utils (not Server_only).
First write-file for Lib.re is the melange context:

  $ grep -c 'module Server_only' _utopia/dune
  1
  $ grep -c 'module Utils = Lib__Utils' _utopia/dune
  2
