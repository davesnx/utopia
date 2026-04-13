$ mkdir pages _utopia
 $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > dune
 $ touch _utopia/dune
$ printf "let page = ()\n" > pages/Home.re
$ utopia.compiler > /dev/null
$ grep -qF 'let isFormData = React_server_dom_esbuild.encodedReplyIsFormData(body);' _utopia/Utopia_call_server.re
$ grep -qF 'if (isFormData) {' _utopia/Utopia_call_server.re
$ grep -qF '("Accept", "application/react.action"),' _utopia/Utopia_call_server.re
$ grep -qF '("Content-Type", "text/plain;charset=utf-8"),' _utopia/Utopia_call_server.re
$ grep -qF '("ACTION_ID", id),' _utopia/Utopia_call_server.re
$ grep -qF '("X-Action-ID", id),' _utopia/Utopia_call_server.re
$ grep -qF '~body=React_server_dom_esbuild.toBodyInit(body),' _utopia/Utopia_call_server.re
$ grep -qF 'let toBodyInit = (reply: encodedReply): Fetch.BodyInit.t =>' _utopia/React_server_dom_esbuild.re
$ grep -qF 'type encodedReply;' _utopia/React_server_dom_esbuild.re
$ grep -qF "external encodeReply: list('arg) => Js.Promise.t(encodedReply) = \"encodeReply\";" _utopia/React_server_dom_esbuild.re
$ grep -qF 'let encodedReplyIsFormData = (reply: encodedReply): bool =>' _utopia/React_server_dom_esbuild.re
