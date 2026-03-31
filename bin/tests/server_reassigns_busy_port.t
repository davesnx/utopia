  $ mkdir pages _utopia
  $ touch _utopia/dune
  $ printf "let page = ()\n" > pages/Home.re
  $ utopia.compiler > /dev/null
  $ python3 - <<'PY' > /dev/null 2>&1 &
  > import socket
  > import time
  > sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  > sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
  > sock.bind(("127.0.0.1", 48115))
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
  >         sock = socket.create_connection(("127.0.0.1", 48115), timeout=0.2)
  >         sock.close()
  >         print("blocked")
  >         sys.exit(0)
  >     except OSError:
  >         time.sleep(0.1)
  > sys.exit(1)
  > PY
  blocked
  $ PORT=48115 HOST=127.0.0.1 NO_LOG=1 utopia.server > server.log 2>&1 &
  $ server_pid=$!
  $ curl -s --retry 10 --retry-connrefused --retry-delay 1 http://127.0.0.1:48116/home | rg -o 'Code page from <code>pages/Home\.re</code>'
  Code page from <code>pages/Home.re</code>
  $ rg 'Port 48115 is already in use on 127.0.0.1; retrying with 48116' server.log
  Port 48115 is already in use on 127.0.0.1; retrying with 48116
  $ kill $server_pid $blocker_pid
  $ wait $server_pid 2>/dev/null || true
  $ wait $blocker_pid 2>/dev/null || true
