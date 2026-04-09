The --extract-client subcommand extracts client component modules and their dependencies:

  $ cat > input.re <<'EOF'
  > let server_helper = () => "server only";
  > let shared_helper = (x) => x ++ " world";
  > module Widget = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~label: string) => {
  >     <div> {React.string(shared_helper(label))} </div>;
  >   };
  > };
  > [@react.component]
  > let make = () => <Widget label="hello" />;
  > EOF
  $ utopia.compiler --extract-client input.re > output.re

Extracted code includes the client component module:

  $ grep -qF 'module Widget' output.re

Extracted code includes the shared helper (used by Widget):

  $ grep -qF 'shared_helper' output.re

Extracted code does NOT include the server-only helper:

  $ ! grep -qF 'server_helper' output.re

Extracted code does NOT include the server-side make:

  $ ! grep -F '[@react.component]' output.re | grep -v 'client.component' | grep -qF 'let make'
