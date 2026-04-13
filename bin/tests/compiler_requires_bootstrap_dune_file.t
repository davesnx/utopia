  $ mkdir app
  $ printf "let page = ()\n" > app/page.re
  $ utopia.compiler > /dev/null || true
