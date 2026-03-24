  $ mkdir pages
  $ printf "let page = ()\n" > pages/Home.re
  $ utopia.compiler > /dev/null || true
