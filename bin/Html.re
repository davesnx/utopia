[@react.component]
let make = (~title, ~body) => {
  <html>
    <head>
      <meta charSet="utf-8" />
      <title> {React.string(title)} </title>
      <meta name="viewport" content="width=device-width, initial-scale=1" />
    </head>
    <body> body </body>
  </html>;
};
