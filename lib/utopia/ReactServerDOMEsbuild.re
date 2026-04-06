type arg;
type encodedReply;
type callServer = (string, list(arg)) => Js.Promise.t(React.element);
type options = {callServer};

[@mel.module "server-reason-react-server-dom-esbuild"]
external createFromReadableStreamImpl:
  (Webapi.ReadableStream.t, ~options: options=?, unit) => Js.Promise.t('a) =
  "createFromReadableStream";

[@mel.module "server-reason-react-server-dom-esbuild"]
external createFromFetchImpl:
  (Js.Promise.t(Fetch.response), ~options: options=?, unit) =>
  Js.Promise.t('a) =
  "createFromFetch";

[@mel.module "server-reason-react-server-dom-esbuild"]
external createServerReferenceImpl:
  (
    string,
    callServer,
    option('encodeFormActionCallback),
    option('findSourceMapURLCallback),
    option(string)
  ) =>
  'action =
  "createServerReference";

[@mel.module "server-reason-react-server-dom-esbuild"]
external encodeReply: list('arg) => Js.Promise.t(encodedReply) =
  "encodeReply";

[@warning "-27"]
let encodedReplyIsFormData = (reply: encodedReply): bool => [%mel.raw
  {js|
    (function(reply) {
      return typeof FormData !== "undefined" && reply instanceof FormData;
    })(reply)
  |js}
];

let callServerRef: ref(option(callServer)) = ref(None);
let setCallServer = callServer => {
  callServerRef := Some(callServer);
};
let getCallServer = () => callServerRef^;

let createFromReadableStream = (~callServer=?, stream): Js.Promise.t('a) =>
  switch (callServer) {
  | Some(callServer) =>
    setCallServer(callServer);
    createFromReadableStreamImpl(
      stream,
      ~options={ callServer: callServer },
      (),
    );
  | None => createFromReadableStreamImpl(stream, ())
  };

let createFromFetch = (~callServer=?, promise) =>
  switch (callServer) {
  | Some(callServer) =>
    setCallServer(callServer);
    createFromFetchImpl(promise, ~options={ callServer: callServer }, ());
  | None => createFromFetchImpl(promise, ())
  };

let createServerReference = serverReferenceId => {
  let callServer =
    switch (getCallServer()) {
    | Some(callServer) => callServer
    | None =>
      raise(
        Invalid_argument(
          "No callServer has been set, you are trying to create a server function without passing callServer to createFromFetch or createFromReadableStream",
        ),
      )
    };
  createServerReferenceImpl(serverReferenceId, callServer, None, None, None);
};
