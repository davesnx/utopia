  $ mkdir -p pages lib _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard _utopia)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Utopia.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string(Lib.value)} </div>;
  > EOF
  $ printf "let value = \"collision-free\"\n" > lib/Lib.re
  $ utopia.compiler > /dev/null
  $ dune build @melange _utopia/server_main.exe > /dev/null
  $ python3 - <<'PY'
  > from pathlib import Path
  > generated = Path("_utopia/dune").read_text()
  > assert "target Utopia_page__Utopia.re" in generated
  > assert "target Utopia_lib__Lib.re" in generated
  > assert "target Lib.re" in generated
  > alias_source = Path("_build/default/_utopia/Lib.re").read_text().strip()
  > assert alias_source == "module Lib = Lib__Lib", alias_source
  > print(alias_source)
  > PY
  module Lib = Lib__Lib
