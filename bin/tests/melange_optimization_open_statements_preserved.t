Open statements are always preserved in extraction (needed for name resolution):

  $ cat > input.re <<'EOF'
  > open React;
  > open Js;
  > 
  > let server_fn = () => "server";
  > 
  > module Widget = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = () => {
  >     <div> {string("hello")} </div>;
  >   };
  > };
  > [@react.component]
  > let make = () => <Widget />;
  > EOF
  $ utopia.compiler --extract-client input.re > output.re

Both open statements are preserved:

  $ grep -c '^open' output.re
  2
  $ grep -qF 'open React' output.re
  $ grep -qF 'open Js' output.re

Server-only code is excluded:

  $ ! grep -qF 'server_fn' output.re
