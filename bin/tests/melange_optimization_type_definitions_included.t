Type definitions used by client components are included in extraction:

  $ cat > input.re <<'EOF'
  > type item = {
  >   id: int,
  >   text: string,
  >   done_: bool,
  > };
  > type server_config = {host: string, port: int};
  > let encode_item = (item: item) => item.text;
  > module Checklist = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~items: list(item)) => {
  >     <ul>
  >       {React.array(Array.of_list(
  >         List.map(item => <li key={string_of_int(item.id)}>
  >           {React.string(encode_item(item))}
  >         </li>, items)))}
  >     </ul>;
  >   };
  > };
  > [@react.component]
  > let make = () => <Checklist items=[] />;
  > EOF
  $ utopia.compiler --extract-client input.re > output.re

Type used by client component is included:

  $ grep -qF 'type item' output.re

Helper using that type is included:

  $ grep -qF 'encode_item' output.re

Unused server type is excluded:

  $ ! grep -qF 'server_config' output.re
