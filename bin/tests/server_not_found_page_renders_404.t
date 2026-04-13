  $ mkdir -p app/about _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("home")} </div>;
  > EOF
  $ cat > app/about/page.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("about")} </div>;
  > EOF
  $ cat > app/not-found.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("custom 404 page")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
  $ PORT=8121 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!

Existing pages should still work with 200:
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8121/ | rg "HTTP/1.1 200 OK"
  HTTP/1.1 200 OK
  $ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8121/ | rg -o '<div>home</div>'
  <div>home</div>

Unmatched routes should return 404 with the custom not-found page:
  $ curl -i -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8121/nonexistent | rg "HTTP/1.1 404 Not Found"
  HTTP/1.1 404 Not Found
  $ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8121/nonexistent | rg -o '<div>custom 404 page</div>'
  <div>custom 404 page</div>

The not-found page should include proper HTML structure:
  $ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8121/nonexistent | rg -o '<title>Not Found</title>'
  <title>Not Found</title>

  $ kill $server_pid
  $ wait $server_pid || true
  Terminated
