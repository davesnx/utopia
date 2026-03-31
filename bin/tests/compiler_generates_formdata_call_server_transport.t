$ mkdir pages _utopia
$ touch _utopia/dune
$ printf "let page = ()\n" > pages/Home.re
$ utopia.compiler > /dev/null
$ python3 - <<'PY'
> from pathlib import Path
> router_source = Path("_utopia/Utopia_router.re").read_text()
> for needle in [
>     'let isFormData = ReactServerDOMEsbuild.encodedReplyIsFormData(body);',
>     'if (isFormData) {',
>     '("Accept", "application/react.action"),',
>     '("Content-Type", "text/plain;charset=utf-8"),',
>     '("ACTION_ID", id),',
>     '("X-Action-ID", id),',
>     '~body=Obj.magic(body),',
> ]:
>     assert needle in router_source, needle
> helper_source = Path("_utopia/ReactServerDOMEsbuild.re").read_text()
> for needle in [
>     'type encodedReply;',
>     'external encodeReply: list(\'arg) => Js.Promise.t(encodedReply) = "encodeReply";',
>     'let encodedReplyIsFormData = (reply: encodedReply): bool =>',
> ]:
>     assert needle in helper_source, needle
> PY
