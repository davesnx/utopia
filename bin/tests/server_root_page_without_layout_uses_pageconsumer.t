$ mkdir -p app/about _utopia
$ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
$ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
$ touch _utopia/dune
$ cat > app/page.re <<'EOF'
> let before = _request => ();
> [@react.component]
> let make = () => <div> {React.string("home")} </div>;
> EOF
$ cat > app/about/page.re <<'EOF'
> [@react.component]
> let make = () => <div> {React.string("about")} </div>;
> EOF
$ utopia.compiler > /dev/null
$ dune build _utopia/server_main.exe > /dev/null
$ PORT=8123 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
$ server_pid=$!
$ curl -s --retry 5 --retry-connrefused --retry-delay 1 http://127.0.0.1:8123 > response.html
$ rg -o '"path":"/"' response.html
"path":"/"
$ rg -o '"pageconsumer":\["\$","div",null,\{"children":"home"' response.html
"pageconsumer":["$","div",null,{"children":"home"
$ ! rg -q '"layout":\["\$","div",null,\{"children":"home"' response.html
$ kill $server_pid
$ wait $server_pid || true
Terminated
