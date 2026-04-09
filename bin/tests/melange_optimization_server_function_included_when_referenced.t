Server functions referenced by client components are included (PPX creates client proxy):

  $ cat > input.re <<'EOF'
  > [@react.server.function]
  > let saveData = (~name: string): Js.Promise.t(string) =>
  >   Js.Promise.resolve("saved " ++ name);
  > 
  > [@react.server.function]
  > let unusedAction = (): Js.Promise.t(string) =>
  >   Js.Promise.resolve("unused");
  > 
  > module Form = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = () => {
  >     let _ = saveData.call(~name="test");
  >     <div> {React.string("form")} </div>;
  >   };
  > };
  > [@react.component]
  > let make = () => <Form />;
  > EOF
  $ utopia.compiler --extract-client input.re > output.re

Referenced server function is included (PPX will transform to client proxy):

  $ grep -qF 'let saveData' output.re

Unreferenced server function is excluded:

  $ ! grep -qF 'unusedAction' output.re

Client component module is included:

  $ grep -qF 'module Form' output.re

Server make is excluded:

  $ test "$(grep -c 'let make' output.re)" -eq 1
