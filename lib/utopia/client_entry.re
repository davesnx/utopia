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

let reportDevError: (string, string, option(string)) => unit =
    (operation, message, stack) => {
  ignore(
    [%mel.raw
      {|
      (function() {
        if (typeof window !== 'undefined' && window.__utopia_dev_report_error) {
          window.__utopia_dev_report_error({
            operation: operation,
            message: message,
            stack: stack || null,
            context: null
          });
        }
      })()
    |}
    ],
  );
};

let () =
  try(
    React.startTransition(() => {
      ignore(hydrateDocumentRoot(browserDocument, <App />));
      ();
    })
  ) {
  | exn =>
    let message = Printexc.to_string(exn);
    let stack = Printexc.get_backtrace();
    let stackOpt = String.length(stack) > 0 ? Some(stack) : None;
    reportDevError("hydration", message, stackOpt);
    Js.Console.error2("Hydration failed:", message);
  };
