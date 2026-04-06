type srrStream = {. "readable_stream": Webapi.ReadableStream.t };

[@mel.scope "window"] external srrStream: srrStream = "srr_stream";

[@mel.module "react-dom/client"]
external hydrateDocumentRoot:
  (Dom.document, React.element) => ReactDOM.Client.root =
  "hydrateRoot";

let browserDocument: Dom.document = Webapi.Dom.document;

module App = {
  let initialModel =
    ReactServerDOMEsbuild.createFromReadableStream(
      ~callServer=Utopia_call_server.callServer,
      srrStream##readable_stream,
    );

  [@react.component]
  let make = () => React.Experimental.usePromise(initialModel);
};

let () =
  React.startTransition(() => {
    ignore(hydrateDocumentRoot(browserDocument, <App />));
    ();
  });
