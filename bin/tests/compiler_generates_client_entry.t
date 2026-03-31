$ mkdir pages _utopia
$ touch _utopia/dune
$ printf "let page = ()\n" > pages/Home.re
$ utopia.compiler > /dev/null
$ python3 - <<'PY'
> from pathlib import Path
> source = Path("_utopia/client_entry.re").read_text()
> needles = [
>     'external srrStream: srrStream = "srr_stream";',
>     'external hydrateDocumentRoot: (Dom.document, React.element) => ReactDOM.Client.root = "hydrateRoot";',
>     'let browserDocument: Dom.document = Webapi.Dom.document;',
>     'let initialModel =',
>     'ReactServerDOMEsbuild.createFromReadableStream(',
>     '~callServer=Utopia.callServer,',
>     'React.Experimental.usePromise(initialModel)',
>     'React.startTransition(() => {',
>     'ignore(hydrateDocumentRoot(browserDocument, <App />));',
> ]
> for needle in needles:
>     assert needle in source, needle
> PY
