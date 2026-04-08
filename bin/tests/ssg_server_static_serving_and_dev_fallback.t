  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/About.ml <<'EOF'
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
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
  $ cat > pages/About.ml <<'EOF'
  > let makeProps () = ()
  > let make () = React.string "updated server render"
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
  $ PORT=8120 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -s --retry 10 --retry-connrefused --retry-delay 1 http://127.0.0.1:8120/about | rg -m1 -o 'initial static html'
  initial static html
  $ rm _utopia/static/about.html
  $ curl -s --retry 10 --retry-connrefused --retry-delay 1 http://127.0.0.1:8120/about | rg -m1 -o 'updated server render'
  updated server render
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
  $ PORT=8120 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe --dev > server.log 2>&1 &
  $ server_pid=$!
  $ curl -s --retry 10 --retry-connrefused --retry-delay 1 http://127.0.0.1:8120/about | rg -m1 -o 'updated server render'
  updated server render
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
