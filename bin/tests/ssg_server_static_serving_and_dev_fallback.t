  $ mkdir -p app/about _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/about/page.ml <<'EOF'
  > let makeProps () = ()
  > let make () = React.string "initial static html"
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
  $ _build/default/_utopia/server_main.exe --ssg > /dev/null
  $ PORT=8120 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -s --retry 10 --retry-connrefused --retry-delay 1 http://127.0.0.1:8120/about | rg -m1 -o 'initial static html'
  initial static html
  $ kill $server_pid 2>/dev/null || true
  $ wait $server_pid 2>/dev/null || true
  $ cat > app/about/page.ml <<'EOF'
  > let makeProps () = ()
  > let make () = React.string "updated server render"
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
Prod mode still serves stale static HTML (pre-rendered file unchanged):
  $ PORT=8120 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -s --retry 10 --retry-connrefused --retry-delay 1 http://127.0.0.1:8120/about | rg -m1 -o 'initial static html'
  initial static html
  $ kill $server_pid 2>/dev/null || true
  $ wait $server_pid 2>/dev/null || true
Dev mode bypasses static HTML and always server-renders (static file still present):
  $ PORT=8120 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe --dev > server.log 2>&1 &
  $ server_pid=$!
  $ curl -s --retry 10 --retry-connrefused --retry-delay 1 http://127.0.0.1:8120/about | rg -m1 -o 'updated server render'
  updated server render
  $ kill $server_pid 2>/dev/null || true
  $ wait $server_pid 2>/dev/null || true
Prod mode falls back to SSR when static HTML is missing:
  $ rm _utopia/static/about.html
  $ PORT=8120 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -s --retry 10 --retry-connrefused --retry-delay 1 http://127.0.0.1:8120/about | rg -m1 -o 'updated server render'
  updated server render
  $ kill $server_pid 2>/dev/null || true
  $ wait $server_pid 2>/dev/null || true
