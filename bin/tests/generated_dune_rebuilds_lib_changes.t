  $ mkdir -p pages lib _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard _utopia)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string(Message.value)} </div>;
  > EOF
  $ cat > lib/Message.re <<'EOF'
  > let value = "one";
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build . > /dev/null
  $ rg 'let value = "one";' _build/default/_utopia/Message.re
  let value = "one";
  $ server_mtime=$(stat -c %Y _build/default/_utopia/server_main.exe)
  $ cat > lib/Message.re <<'EOF'
  > let value = "two";
  > EOF
  $ dune build . > /dev/null
  $ SERVER_MTIME=$server_mtime python3 - <<'PY'
  > import os, pathlib, sys
  > baseline = float(os.environ['SERVER_MTIME'])
  > generated = pathlib.Path('_build/default/_utopia/Message.re')
  > server = pathlib.Path('_build/default/_utopia/server_main.exe')
  > if 'two' in generated.read_text() and server.stat().st_mtime > baseline:
  >     print('updated')
  > else:
  >     sys.exit(1)
  > PY
  updated
