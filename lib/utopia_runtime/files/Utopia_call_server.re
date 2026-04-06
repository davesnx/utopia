open! Melange_json.Primitives;

let callServer = (id, args) =>
  switch%platform () {
  | Client =>
    ReactServerDOMEsbuild.encodeReply(args)
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
         |> Js.Promise.then_(response =>
              ReactServerDOMEsbuild.createFromReadableStream(
                Fetch.Response.body(response),
              )
            );
       })
  | Server => failwith("callServer isn't supported on the server")
  };
