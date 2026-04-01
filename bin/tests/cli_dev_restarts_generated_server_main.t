  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard _utopia)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("one")} </div>;
  > EOF
  $ PORT=8112 HOST=127.0.0.1 NO_LOG=1 utopia dev > dev.log 2>&1 &
  $ dev_pid=$!
  $ for i in $(seq 30); do curl -sf http://127.0.0.1:8112/home 2>/dev/null | grep -qF 'one' && break; sleep 1; done && echo ready
  ready
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("two")} </div>;
  > EOF
  $ for i in $(seq 30); do curl -sf http://127.0.0.1:8112/home 2>/dev/null | grep -qF 'two' && break; sleep 1; done && echo updated
  updated
  $ kill $dev_pid
  $ wait $dev_pid || true
