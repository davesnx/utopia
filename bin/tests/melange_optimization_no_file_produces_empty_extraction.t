Files without client components produce empty extraction output:

  $ cat > input.re <<'EOF'
  > let before = _request => ();
  > [@react.server.function]
  > let action = (): Js.Promise.t(string) => Js.Promise.resolve("ok");
  > [@react.component]
  > let make = () => <div> {React.string("page")} </div>;
  > EOF
  $ utopia.compiler --extract-client input.re > output.re

Output is empty (no client components):

  $ test ! -s output.re
