  $ mkdir -p pages/about _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/about/Team.re <<'EOF'
  > [@react.component]
  > let make = () => <div> <h1> {React.string("Team")} </h1> </div>;
  > EOF
  $ cat > pages/about/History.re <<'EOF'
  > [@react.component]
  > let make = () => <div> <h1> {React.string("History")} </h1> </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build @melange _utopia/server_main.exe > /dev/null
  $ PORT=8108 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 -H 'Accept: application/react.component' -H 'X-Utopia-Current-Path: /about/team' http://127.0.0.1:8108/about/history > response.txt
  $ rg -o 'HTTP/1.1 200 OK|Content-Type: application/react.component|X-Location: /about/history|\["diff","/about"|Utopia_router_route' response.txt
  HTTP/1.1 200 OK
  Content-Type: application/react.component
  X-Location: /about/history
  Utopia_router_route
  ["diff","/about"
  $ ! rg -q 'Async exception|InnerHtml does not exist in RSC' server.log
  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
