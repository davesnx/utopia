  $ mkdir -p pages lib _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
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
  $ cat > lib/Message.re <<'EOF'
  > let value = "two";
  > EOF
  $ dune build . > /dev/null
  $ grep -qF '"two"' _build/default/_utopia/native/Lib__Message.re && echo content_updated
  content_updated
