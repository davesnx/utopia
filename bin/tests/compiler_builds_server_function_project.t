  $ mkdir app _utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
  $ touch _utopia/dune
  $ cat > app/page.re <<'EOF'
  > [@react.server.function]
  > let increment = (~count: int): Js.Promise.t(int) =>
  >   Js.Promise.resolve(count + 1);
  > 
  > [@react.component]
  > let make = () =>
  >   <form
  >     action={
  >       switch%platform () {
  >       | Server => `Function(increment)
  >       | Client => ""
  >       }
  >     }>
  >     <button> {React.string("Go")} </button>
  >   </form>;
  > EOF
  $ utopia.compiler > /dev/null
  $ dune build @melange _utopia/server_main.exe
