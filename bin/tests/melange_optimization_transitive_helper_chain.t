Helpers used by client code are included transitively within the same file.
If Widget uses helper_a, and helper_a uses helper_b, both are included:

  $ cat > input.re <<'EOF'
  > let helper_b = (x) => x ++ "!";
  > let helper_a = (x) => helper_b("Hello " ++ x);
  > let server_only = () => "db query";
  > module Widget = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~name: string) => {
  >     <div> {React.string(helper_a(name))} </div>;
  >   };
  > };
  > [@react.component]
  > let make = () => {
  >   let data = server_only();
  >   <Widget name=data />;
  > };
  > EOF
  $ utopia.compiler --extract-client input.re > output.re

Both helpers in the chain are included:

  $ grep -qF 'helper_a' output.re
  $ grep -qF 'helper_b' output.re

Server-only code is excluded:

  $ ! grep -qF 'server_only' output.re
