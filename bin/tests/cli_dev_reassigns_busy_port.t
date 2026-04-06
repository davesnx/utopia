  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("home on fallback port")} </div>;
  > EOF
  $ nc -l -k 127.0.0.1 48113 > /dev/null 2>&1 &
  $ blocker_pid=$!
  $ for i in $(seq 30); do nc -z 127.0.0.1 48113 2>/dev/null && break; sleep 0.1; done
  $ PORT=48113 HOST=127.0.0.1 NO_LOG=1 utopia dev --no-watch > dev.log 2>&1 &
  $ dev_pid=$!
  $ curl -s --retry 10 --retry-connrefused --retry-delay 1 http://127.0.0.1:48114/home | rg -o '<div>home on fallback port</div>'
  <div>home on fallback port</div>
  $ rg 'Port 48113 is already in use on 127.0.0.1; using 48114 instead|Ready at|http://127.0.0.1:48114' dev.log
    ⚠ Port 48113 is already in use on 127.0.0.1; using 48114 instead
    Ready at http://127.0.0.1:48114
  $ kill $dev_pid $blocker_pid
  $ wait $dev_pid 2>/dev/null || true
  $ wait $blocker_pid 2>/dev/null || true
