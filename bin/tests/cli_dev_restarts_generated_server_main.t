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
  $ python3 - <<'PY'
  > import sys, time, urllib.request
  > url = 'http://127.0.0.1:8112/home'
  > for _ in range(30):
  >     try:
  >         body = urllib.request.urlopen(url).read().decode()
  >         if 'one' in body:
  >             print('ready')
  >             sys.exit(0)
  >     except Exception:
  >         pass
  >     time.sleep(1)
  > sys.exit(1)
  > PY
  ready
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string("two")} </div>;
  > EOF
  $ python3 - <<'PY'
  > import sys, time, urllib.request
  > url = 'http://127.0.0.1:8112/home'
  > for _ in range(30):
  >     try:
  >         body = urllib.request.urlopen(url).read().decode()
  >         if 'two' in body:
  >             print('updated')
  >             sys.exit(0)
  >     except Exception:
  >         pass
  >     time.sleep(1)
  > sys.exit(1)
  > PY
  updated
  $ kill $dev_pid
  $ wait $dev_pid || true
