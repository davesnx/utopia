  $ mkdir -p pages lib _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Utopia.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string(Lib.value)} </div>;
  > EOF
  $ printf "let value = \"collision-free\"\n" > lib/Lib.re
  $ utopia.compiler > /dev/null
  $ dune build @melange _utopia/server_main.exe _utopia/Lib.re > /dev/null
  $ grep -qF 'target Pages__Utopia.re' _utopia/dune
  $ grep -qF 'target Lib__Lib.re' _utopia/dune
  $ grep -qF 'target Lib.re' _utopia/dune
  $ cat _build/default/_utopia/Lib.re
  module Lib = Lib__Lib
