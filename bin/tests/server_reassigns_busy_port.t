  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("hello")} </div>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build _utopia/server_main.exe > /dev/null
  $ nc -l -k 127.0.0.1 48115 > /dev/null 2>&1 &
  $ blocker_pid=$!
  $ for i in $(seq 30); do nc -z 127.0.0.1 48115 2>/dev/null && break; sleep 0.1; done
  $ PORT=48115 HOST=127.0.0.1 NO_LOG=1 _build/default/_utopia/server_main.exe > server.log 2>&1 &
  $ server_pid=$!
  $ curl -s --retry 10 --retry-connrefused --retry-delay 1 http://127.0.0.1:48116/home | rg -o -m1 'hello'
  hello
  $ rg 'Port 48115 is already in use on 127.0.0.1; retrying with 48116' server.log
  Port 48115 is already in use on 127.0.0.1; retrying with 48116
  $ kill $server_pid $blocker_pid
  $ wait $server_pid 2>/dev/null || true
  $ wait $blocker_pid 2>/dev/null || true
