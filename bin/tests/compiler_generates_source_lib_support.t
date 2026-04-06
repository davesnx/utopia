  $ mkdir pages lib _utopia
  $ cat > dune-project <<'EOF'
  > (lang dune 3.9)
  > (using melange 0.1)
  > 
  > (dialect
  >  (name mlx)
  >  (implementation
  >   (extension mlx)
  >   (preprocess
  >    (run mlx-pp %{input-file}))))
  > EOF
  $ cat > dune <<'EOF'
  > (data_only_dirs _utopia)
  > (include _utopia/dune)
  > EOF
  $ touch _utopia/dune
  $ cat > pages/Home.re <<'EOF'
  > [@react.component]
  > let make = () => <div> {React.string(string_of_int(Data.answer))} </div>;
  > EOF
  $ printf "let answer = 42\n" > lib/Data.ml
  $ cat > lib/Widget.mlx <<'EOF'
  > module Badge = struct
  >   let[@react.component] make ~(label : string) () =
  >     <div> (React.string label) </div>
  > end
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -qF '(library (name utopia_' _utopia/dune
  $ grep -qF '(subdir lib' _utopia/dune
  $ grep -qF '(modules Data Widget)' _utopia/dune
  $ grep -qF '(libraries utopia)' _utopia/dune
  $ export UTOPIA_ROOT="$(dirname "$OPAM_SWITCH_PREFIX")"
  $ export OCAMLPATH="$UTOPIA_ROOT/_build/install/default/lib"
  $ dune build --root . @all _utopia/server_main.exe > /dev/null
  $ ls _build/default/lib/.source_lib_*.objs/byte/data.cmi | grep -q 'data.cmi'
  $ ls _build/default/lib/.source_lib_*.objs/byte/widget.cmi | grep -q 'widget.cmi'
