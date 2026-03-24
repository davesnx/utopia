  $ mkdir -p pages/client _utopia
  $ touch _utopia/dune
  $ printf "let run = ()\n" > pages/client/home_client.re
  $ printf "/* @utopia.script ./client/home_client.re */\n/* @utopia.script ./client/home_client.re */\nlet page = ()\n" > pages/Home.re
  $ utopia.compiler > /dev/null
  
    Invalid page declarations:
      - In pages/Home.re: duplicate @utopia.script for 'pages/client/home_client.re' at line 2 (first seen at line 1)
  
    Supported Next.js-style segments:
      * [id]
      * [...slug]
      * [[...slug]]
      * route groups: (marketing)
      * parallel slots: @slot (ignored for URL path)
    Script directive format:
      * @utopia.script ./relative/path/to/module.re
  [1]
