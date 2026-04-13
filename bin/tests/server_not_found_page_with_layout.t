  $ mkdir -p app/about _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/layout.re <<'EOF'
  > [@react.component]
  > let make = (~children) => <div className="shell"> children </div>;
  > EOF
  $ cat > app/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ cat > app/not-found.re <<'EOF'
  > [@react.component]
  > let make = () => <h1> {React.string("Page not found")} </h1>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
  $ PORT=8122 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!

The not-found page should be wrapped by the root layout:
  $ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8122/nonexistent | rg -qF '<div class="shell"><h1>Page not found</h1></div>'

  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
