  $ mkdir -p pages lib _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard _utopia)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string(string_of_int(Utils.value))} </div>;
  > EOF
  $ printf "let value = 1\n" > lib/Utils.re
  $ utopia.compiler > /dev/null
  $ dune build . > /dev/null
  $ dune build @melange _utopia/server_main.exe _utopia/Utopia_page__Home.re _utopia/Utopia_lib__Utils.re _utopia/Lib.re _utopia/native/Utopia_page__Home.re _utopia/native/Utopia_lib__Utils.re _utopia/native/Lib.re > /dev/null
  $ for f in _build/default/_utopia/Utopia_page__Home.re _build/default/_utopia/Utopia_lib__Utils.re _build/default/_utopia/Lib.re _build/default/_utopia/native/Utopia_page__Home.re _build/default/_utopia/native/Utopia_lib__Utils.re _build/default/_utopia/native/Lib.re; do echo "$f"; sed -n '1,4p' "$f"; done
  _build/default/_utopia/Utopia_page__Home.re
  open! Melange_json.Primitives;
  open! Lib;
  [@react.component]
  let make = () => <div> {React.string(string_of_int(Utils.value))} </div>;
  _build/default/_utopia/Utopia_lib__Utils.re
  open! Melange_json.Primitives;
  open! Lib;
  let value = 1
  _build/default/_utopia/Lib.re
  module Utils = Lib__Utils_build/default/_utopia/native/Utopia_page__Home.re
  open! Melange_json.Primitives;
  open! Lib;
  [@react.component]
  let make = () => <div> {React.string(string_of_int(Utils.value))} </div>;
  _build/default/_utopia/native/Utopia_lib__Utils.re
  open! Melange_json.Primitives;
  open! Lib;
  let value = 1
  _build/default/_utopia/native/Lib.re
  module Utils = Lib__Utils
  $ cat _utopia/routes.manifest
  home	code	pages/Home.re	home			false
