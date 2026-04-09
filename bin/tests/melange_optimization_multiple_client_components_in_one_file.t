Multiple client components in the same file are all extracted with shared helpers:

  $ cat > input.re <<'EOF'
  > let format = (x) => "(" ++ x ++ ")";
  > let server_data = () => "db";
  > module Header = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~title: string) => {
  >     <h1> {React.string(format(title))} </h1>;
  >   };
  > };
  > module Body = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~content: string) => {
  >     <div> {React.string(format(content))} </div>;
  >   };
  > };
  > module Footer = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = () => {
  >     <footer> {React.string("footer")} </footer>;
  >   };
  > };
  > [@react.component]
  > let make = () => {
  >   let data = server_data();
  >   <div> <Header title=data /> <Body content=data /> <Footer /> </div>;
  > };
  > EOF
  $ utopia.compiler --extract-client input.re > output.re

All three client component modules are extracted:

  $ grep -c 'module Header' output.re
  1
  $ grep -c 'module Body' output.re
  1
  $ grep -c 'module Footer' output.re
  1

Shared helper used by Header and Body is included:

  $ grep -qF 'let format' output.re

Server-only code is excluded:

  $ ! grep -qF 'server_data' output.re

Server make and server_data are excluded:

  $ grep 'server_data' output.re || echo "not found"
  not found
  $ grep 'let data =' output.re || echo "not found"
  not found
