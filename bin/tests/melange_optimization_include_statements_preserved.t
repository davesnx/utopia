Include statements are preserved in extraction alongside open statements:

  $ cat > input.re <<'EOF'
  > open React;
  > include Melange_json.Primitives;
  > 
  > let server_fn = () => "server";
  > 
  > module Widget = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = () => <div />;
  > };
  > [@react.component]
  > let make = () => <Widget />;
  > EOF
  $ utopia.compiler --extract-client input.re > output.re

Open and include are both preserved:

  $ grep -qF 'open React' output.re
  $ grep -qF 'include Melange_json' output.re

Server code excluded:

  $ ! grep -qF 'server_fn' output.re
