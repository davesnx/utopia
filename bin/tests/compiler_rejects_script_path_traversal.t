  $ mkdir pages _utopia
  $ touch _utopia/dune
  $ printf "/* @utopia.script ../outside.re */\nlet page = ()\n" > pages/Home.re
  $ utopia.compiler > /dev/null
  
    Invalid page declarations:
      - In pages/Home.re: Script path cannot contain '..' segments ('../outside.re')
  
    Supported Next.js-style segments:
      * [id]
      * [...slug]
      * [[...slug]]
      * route groups: (marketing)
      * parallel slots: @slot (ignored for URL path)
    Script directive format:
      * @utopia.script ./relative/path/to/module.re
  [1]
