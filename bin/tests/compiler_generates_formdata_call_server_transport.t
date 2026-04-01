$ mkdir pages _utopia
$ touch _utopia/dune
$ printf "let page = ()\n" > pages/Home.re
$ utopia.compiler > /dev/null
$ grep -qF 'let isFormData = ReactServerDOMEsbuild.encodedReplyIsFormData(body);' _utopia/Utopia_router.re
$ grep -qF 'if (isFormData) {' _utopia/Utopia_router.re
$ grep -qF '("Accept", "application/react.action"),' _utopia/Utopia_router.re
$ grep -qF '("Content-Type", "text/plain;charset=utf-8"),' _utopia/Utopia_router.re
$ grep -qF '("ACTION_ID", id),' _utopia/Utopia_router.re
$ grep -qF '("X-Action-ID", id),' _utopia/Utopia_router.re
$ grep -qF '~body=ReactServerDOMEsbuild.toBodyInit(body),' _utopia/Utopia_router.re
$ grep -qF 'let toBodyInit = (reply: encodedReply): Fetch.BodyInit.t =>' _utopia/ReactServerDOMEsbuild.re
$ grep -qF 'type encodedReply;' _utopia/ReactServerDOMEsbuild.re
$ grep -qF "external encodeReply: list('arg) => Js.Promise.t(encodedReply) = \"encodeReply\";" _utopia/ReactServerDOMEsbuild.re
$ grep -qF 'let encodedReplyIsFormData = (reply: encodedReply): bool =>' _utopia/ReactServerDOMEsbuild.re
