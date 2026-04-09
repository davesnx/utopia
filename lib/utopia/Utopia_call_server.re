open! Melange_json.Primitives;

let reportServerActionError = (actionId, phase, message) =>
  switch%platform () {
  | Client =>
    ignore(
      [%mel.raw
        {|
      (function() {
        if (typeof window !== 'undefined' && window.__utopia_dev_report_error) {
          window.__utopia_dev_report_error({
            operation: 'server_action',
            message: message,
            stack: null,
            context: 'action=' + actionId + ' phase=' + phase
          });
        }
      })()
    |}
      ],
    )
  | Server => ()
  };

let callServer = (id, args) =>
  switch%platform () {
  | Client =>
    ReactServerDOMEsbuild.encodeReply(args)
    |> Js.Promise.catch(err => {
         let msg = [%mel.raw {| String(err && err.message ? err.message : err) |}];
         reportServerActionError(id, "encodeReply", msg);
         Js.Promise.reject([%mel.raw {| err instanceof Error ? err : new Error(String(err)) |}]);
       })
    |> Js.Promise.then_(body => {
         let isFormData = ReactServerDOMEsbuild.encodedReplyIsFormData(body);
         let headers =
           if (isFormData) {
             Fetch.HeadersInit.makeWithArray([|
               ("Accept", "application/react.action"),
               ("ACTION_ID", id),
               ("X-Action-ID", id),
             |]);
           } else {
             Fetch.HeadersInit.makeWithArray([|
               ("Accept", "application/react.action"),
               ("Content-Type", "text/plain;charset=utf-8"),
               ("ACTION_ID", id),
               ("X-Action-ID", id),
             |]);
           };
         let init =
           Fetch.RequestInit.make(
             ~method_=Post,
             ~headers,
             /* FormData replies need the browser-generated multipart boundary. */
             ~body=Obj.magic(body),
             (),
           );
         Fetch.fetchWithInit("", init)
         |> Js.Promise.catch(err => {
              let msg = [%mel.raw {| String(err && err.message ? err.message : err) |}];
              reportServerActionError(id, "fetch", msg);
              Js.Promise.reject([%mel.raw {| err instanceof Error ? err : new Error(String(err)) |}]);
            })
         |> Js.Promise.then_(response =>
              ReactServerDOMEsbuild.createFromReadableStream(
                Fetch.Response.body(response),
              )
              |> Js.Promise.catch(err => {
                   let msg = [%mel.raw {| String(err && err.message ? err.message : err) |}];
                   reportServerActionError(id, "decode", msg);
                   Js.Promise.reject([%mel.raw {| err instanceof Error ? err : new Error(String(err)) |}]);
                 })
            );
       })
  | Server => failwith("callServer isn't supported on the server")
  };
