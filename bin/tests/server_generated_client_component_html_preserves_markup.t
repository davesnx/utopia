  $ mkdir -p app/home _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/home/page.re <<'EOF'
  > module Checklist = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~label: string) => {
  >     let (_count, _setCount) = React.useState(() => 0);
  >     <section>
  >       <p> {React.string(label)} </p>
  >       <div> {React.string("Ready")} </div>
  >     </section>;
  >   };
  > };
  > 
  > [@react.component]
  > let make = () =>
  >   <main>
  >     <Checklist label="Checklist" />
  >   </main>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build @melange _utopia/server_main.exe > /dev/null
  $ PORT=8109 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8109/home | rg -o '<section><p>Checklist</p><div>Ready</div></section>|<section><div><p>Checklist</p></div><div>Ready</div></section>'
  <section><p>Checklist</p><div>Ready</div></section>
  $ kill $server_pid 2>/dev/null || true
  $ wait $server_pid 2>/dev/null || true
