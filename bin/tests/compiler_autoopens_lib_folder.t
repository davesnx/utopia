  $ mkdir -p pages lib _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => {
  >   let label = switch%platform () {
  >   | Client => "client"
  >   | Server => "server"
  >   };
  >   <div> {React.string(label ++ string_of_int(Utils.value))} </div>;
  > };
  > EOF
  $ printf "let value = 1\n" > lib/Utils.re
  $ printf "let answer = 42\n" > lib/Data.ml
  $ utopia.compiler > /dev/null
Check compile_flags are generated with -open Melange_json.Primitives:
  $ grep -F 'compile_flags' _utopia/dune
   (compile_flags -open Melange_json.Primitives)
  $ grep -F 'flags :standard' _utopia/dune
    (flags :standard -w -26-27-39 -open Melange_json.Primitives)
Check page copy rules have open! Lib prelude:
  $ grep -A5 'target Pages__Home.re' _utopia/dune | head -6
  (rule (deps ../pages/Home.re) (target Pages__Home.re)
   (action
    (with-stdout-to %{target}
     (progn (echo "# 1 \"../pages/Home.re\"\n") (echo "open! Lib;\n")
      (echo "# 1 \"../pages/Home.re\"\n") (run cat %{deps})))))
  
Check lib copy rules do NOT have open! Lib (just line directive):
  $ grep -A4 'target Lib__Utils.re' _utopia/dune | head -5
  (rule (deps ../lib/Utils.re) (target Lib__Utils.re)
   (action
    (with-stdout-to %{target}
     (progn (echo "# 1 \"../lib/Utils.re\"\n") (run cat %{deps})))))
  
Check Lib.re namespace module (appears twice - for melange and native):
  $ grep 'Lib.re' _utopia/dune
  (rule (target Lib.re)
   (rule (target Lib.re)
  $ cat _utopia/routes.manifest
  home	code	pages/Home.re	home			false	false
