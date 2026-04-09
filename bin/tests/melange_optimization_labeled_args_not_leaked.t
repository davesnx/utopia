Labeled arguments in client component make functions do not leak into
the dependency closure. A top-level binding with the same name as a
labeled arg should NOT be pulled in:

  $ cat > input.re <<'EOF'
  > let items = [1, 2, 3];
  > let config = "server config";
  > 
  > module List_view = {
  >   [@react.client.component]
  >   [@react.component]
  >   let make = (~items: list(string), ~config: string) => {
  >     <ul>
  >       {React.array(Array.of_list(
  >         List.map(item => <li> {React.string(item)} </li>, items)))}
  >     </ul>;
  >   };
  > };
  > [@react.component]
  > let make = () => <List_view items={List.map(string_of_int, items)} config />;
  > EOF
  $ utopia.compiler --extract-client input.re > output.re

Client component module is extracted:

  $ grep -qF 'module List_view' output.re

Top-level 'items' and 'config' are NOT included (they share names with
labeled args but are server-only bindings):

  $ grep '^let items' output.re || echo "not found"
  not found
  $ grep '^let config' output.re || echo "not found"
  not found
