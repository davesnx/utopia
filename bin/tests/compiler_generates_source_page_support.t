  $ mkdir -p pages pages/blog/[slug] _utopia
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
  $ cat > pages/index.mlx <<'EOF'
  > let[@react.component] make () = <div> (React.string "home") </div>
  > EOF
  $ cat > pages/layout.mlx <<'EOF'
  > let[@react.component] make ~children () = children
  > EOF
  $ cat > pages/blog/index.mlx <<'EOF'
  > let[@react.server.function] action () : string Js.Promise.t =
  >   Js.Promise.resolve "ok"
  > 
  > let[@react.component] make () = <div> (React.string "blog") </div>
  > EOF
  $ cat > pages/blog/[slug]/index.mlx <<'EOF'
  > let before _request = ()
  > let[@react.component] make () = <div> (React.string "slug") </div>
  > EOF
  $ utopia.compiler > /dev/null
  $ grep -qF '(subdir pages' _utopia/dune
  $ grep -qF '(subdir pages/blog' _utopia/dune
  $ grep -qF '(subdir pages/blog/[slug]' _utopia/dune
  $ grep -qF '(modules Index Layout)' _utopia/dune
  $ grep -qF '(modules Index)' _utopia/dune
  $ export UTOPIA_ROOT="$(dirname "$OPAM_SWITCH_PREFIX")"
  $ export OCAMLPATH="$UTOPIA_ROOT/_build/install/default/lib"
  $ dune build --root . @pages/all @pages/blog/all '@pages/blog/[slug]/all' > /dev/null
  $ ls _build/default/pages/.source_pages_*.objs/byte/index.cmi | grep -q 'index.cmi'
  $ ls _build/default/pages/.source_pages_*.objs/byte/layout.cmi | grep -q 'layout.cmi'
  $ ls _build/default/pages/blog/.source_pages_*.objs/byte/index.cmi | grep -q 'index.cmi'
  $ ls "_build/default/pages/blog/[slug]"/.source_pages_*.objs/byte/index.cmi | grep -q 'index.cmi'
