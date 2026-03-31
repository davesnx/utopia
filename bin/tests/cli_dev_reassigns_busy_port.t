  $ mkdir pages _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard _utopia)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("home on fallback port")} </div>;
  > EOF
  $ python3 - <<'PY' > /dev/null 2>&1 &
  > import socket
  > import time
  > sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  > sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
  > sock.bind(("127.0.0.1", 48113))
  > sock.listen()
  > time.sleep(120)
  > PY
  $ blocker_pid=$!
  $ python3 - <<'PY'
  > import socket
  > import sys
  > import time
  > for _ in range(30):
  >     try:
  >         sock = socket.create_connection(("127.0.0.1", 48113), timeout=0.2)
  >         sock.close()
  >         print("blocked")
  >         sys.exit(0)
  >     except OSError:
  >         time.sleep(0.1)
  > sys.exit(1)
  > PY
  blocked
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
