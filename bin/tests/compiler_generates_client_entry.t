$ mkdir pages _utopia
 $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
 $ touch _utopia/dune
$ printf "let page = ()\n" > pages/Home.re
$ utopia.compiler > /dev/null
$ grep -qF 'external srrStream: srrStream = "srr_stream";' _utopia/client_entry.re
$ grep -qF 'external hydrateDocumentRoot: (Dom.document, React.element) => ReactDOM.Client.root = "hydrateRoot";' _utopia/client_entry.re
$ grep -qF 'let browserDocument: Dom.document = Webapi.Dom.document;' _utopia/client_entry.re
$ grep -qF 'let initialModel =' _utopia/client_entry.re
$ grep -qF 'React_server_dom_esbuild.createFromReadableStream(' _utopia/client_entry.re
$ grep -qF '~callServer=Utopia.callServer,' _utopia/client_entry.re
$ grep -qF 'React.Experimental.usePromise(initialModel)' _utopia/client_entry.re
$ grep -qF 'React.startTransition(() => {' _utopia/client_entry.re
$ grep -qF 'ignore(hydrateDocumentRoot(browserDocument, <App />));' _utopia/client_entry.re
