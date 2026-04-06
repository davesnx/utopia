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
  $ grep -qF '(compile_flags -open Melange_json.Primitives)' _utopia/dune
  $ grep -qF '(flags :standard -w -26-27-39 -open Melange_json.Primitives)' _utopia/dune
Check page copy rules have open! Lib prelude:
  $ grep -qF '(echo "open! Lib;\n")' _utopia/dune
Check lib copy rules do NOT have open! Lib (just line directive):
  $ grep -qF '(rule (deps ../lib/Utils.re) (target Lib__Utils.re)' _utopia/dune
Check Lib.re namespace module (appears twice - for melange and native):
  $ test "$(grep -c '(rule (target Lib.re)' _utopia/dune)" -eq 2
  $ grep -F 'source_file = "pages/Home.re"; module_name = "Pages__Home"; has_metadata = false;' _utopia/Routes.ml
    ({ route = "home"; matcher = "home"; conflict_key = "home"; params = []; layouts = []; kind = Utopia_types.Code_page; source_file = "pages/Home.re"; module_name = "Pages__Home"; has_metadata = false; static = false; has_static_paths = false } : Utopia_types.page_route_meta);
