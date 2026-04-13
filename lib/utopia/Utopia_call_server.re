open! Melange_json.Primitives;

let reportDevError = (operation, message, stack, context) =>
  switch%platform () {
  | Client =>
    ignore(
      [%mel.raw
        {|
      (function() {
        if (typeof window !== 'undefined' && window.__utopia_dev_report_error) {
          window.__utopia_dev_report_error({
            operation: operation,
            message: message,
            stack: stack || null,
            context: context || null
          });
        }
      })()
    |}
      ],
    )
  | Server => ()
  };

let reportServerActionError = (actionId, phase, message) =>
  reportDevError(
    "server_action",
    message,
    None,
    Some("action=" ++ actionId ++ " phase=" ++ phase),
  );

let callServer = (id, args) =>
  switch%platform () {
  | Client =>
    React_server_dom_esbuild.encodeReply(args)
    |> Js.Promise.catch(err => {
         let msg = [%mel.raw
           {| String(err && err.message ? err.message : err) |}
         ];
         reportServerActionError(id, "encodeReply", msg);
         Js.Promise.reject(
           [%mel.raw
             {| err instanceof Error ? err : new Error(String(err)) |}
           ],
         );
       })
    |> Js.Promise.then_(body => {
          let isFormData = React_server_dom_esbuild.encodedReplyIsFormData(body);
          let commonHeaders = [|
            ("Accept", "application/react.action"),
            ("ACTION_ID", id),
            ("X-Action-ID", id),
          |];
          let headers =
            Fetch.HeadersInit.makeWithArray(
              if (isFormData) {
                commonHeaders;
              } else {
                Array.append(commonHeaders, [|
                  ("Content-Type", "text/plain;charset=utf-8"),
                |]);
              },
            );
          let init =
            Fetch.RequestInit.make(
              ~method_=Post,
              ~headers,
              /* FormData replies need the browser-generated multipart boundary. */
              ~body=React_server_dom_esbuild.toBodyInit(body),
              (),
            );
         Fetch.fetchWithInit("", init)
         |> Js.Promise.catch(err => {
              let msg = [%mel.raw
                {| String(err && err.message ? err.message : err) |}
              ];
              reportServerActionError(id, "fetch", msg);
              Js.Promise.reject(
                [%mel.raw
                  {| err instanceof Error ? err : new Error(String(err)) |}
                ],
              );
            })
         |> Js.Promise.then_(response =>
               React_server_dom_esbuild.createFromReadableStream(
                Fetch.Response.body(response),
              )
              |> Js.Promise.catch(err => {
                   let msg = [%mel.raw
                     {| String(err && err.message ? err.message : err) |}
                   ];
                   reportServerActionError(id, "decode", msg);
                   Js.Promise.reject(
                     [%mel.raw
                       {| err instanceof Error ? err : new Error(String(err)) |}
                     ],
                   );
                 })
            );
       })
  | Server => failwith("callServer isn't supported on the server")
  };
