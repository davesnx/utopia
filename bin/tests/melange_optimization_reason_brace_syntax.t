Extraction works with Reason { } module syntax:

  $ cat > input.re <<'EOF'
  > let server_helper = () => "server";
  > let shared_helper = (x) => x ++ " world";
  > 
  > module Widget = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~label: string) => {
  >     <div> {React.string(shared_helper(label))} </div>;
  >   };
  > };
  > 
  > [@react.component]
  > let make = () => {
  >   let data = server_helper();
  >   <div> <Widget label=data /> </div>;
  > };
  > EOF
  $ utopia.compiler --extract-client input.re > output.re

Client component module is extracted:

  $ grep -qF 'module Widget' output.re
  $ grep -qF 'react.client.component' output.re

Shared helper is included:

  $ grep -qF 'shared_helper' output.re

Server code is excluded:

  $ ! grep -qF 'server_helper' output.re
