[@react.component]
let make = (~title, ~scripts, ~children) => {
  <html>
    <head>
      <meta charSet="utf-8" />
      <title> {React.string(title)} </title>
      <meta name="viewport" content="width=device-width, initial-scale=1" />
      {scripts |> List.map(script => script) |> React.list}
    </head>
    <body> children </body>
  </html>;
};
