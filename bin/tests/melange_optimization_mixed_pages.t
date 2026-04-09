Mixed project: some pages with client components, some without.
Only client-component pages go to melange:

  $ mkdir -p pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > module Counter = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~initial: int) => {
  >     let (count, setCount) = React.useState(() => initial);
  >     <button onClick={_ => setCount(c => c + 1)}>
  >       {React.string(string_of_int(count))}
  >     </button>;
  >   };
  > };
  > [@react.component]
  > let make = () => <Counter initial=0 />;
  > EOF
  $ cat > pages/About.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("about")} </div>;
  > EOF
  $ cat > pages/Contact.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("contact")} </div>;
  > EOF
  $ cat > pages/Dashboard.re <<'EOF'
  > module Toggle = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~on: bool) => {
  >     <span> {React.string(on ? "ON" : "OFF")} </span>;
  >   };
  > };
  > [@react.component]
  > let make = () => <Toggle on=true />;
  > EOF
  $ utopia.compiler > /dev/null

Only Home and Dashboard (with client components) are in melange:

  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'melange\.emit.*?modules \K[^)]+' | tr ' ' '\n' | sort
  Pages__Dashboard
  Pages__Home
  client_entry_melange

About and Contact only appear in native subdir (not melange copy rules).
Native has all 4, melange has only 2 extraction rules:

  $ grep -c 'extract-client' _utopia/dune
  2

But all pages are in native library:

  $ cat _utopia/dune | tr -s ' \n' ' ' | grep -oP 'library.*?name pages_.*?modules \K[^)]+' | tr ' ' '\n' | sort
  Pages__About
  Pages__Contact
  Pages__Dashboard
  Pages__Home

Extraction rules use --extract-client:

  $ grep -c 'extract-client' _utopia/dune
  2
