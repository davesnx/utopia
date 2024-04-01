[@react.component]
let make = (~title, ~scripts, ~body) => {
  <html>
    <head>
      <meta charSet="utf-8" />
      <title> {React.string(title)} </title>
      <meta name="viewport" content="width=device-width, initial-scale=1" />
      {scripts |> List.map(script => script) |> React.list}
    </head>
    <body> body </body>
  </html>;
};
