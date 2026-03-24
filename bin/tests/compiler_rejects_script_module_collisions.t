  $ mkdir -p pages/client _utopia
  $ touch _utopia/dune
  $ printf "let run = ()\n" > pages/client/a-b.re
  $ printf "let run = ()\n" > pages/client/a_b.re
  $ printf "/* @utopia.script ./client/a-b.re */\nlet page = ()\n" > pages/Home.re
  $ printf "/* @utopia.script ./client/a_b.re */\nlet page = ()\n" > pages/About.re
  $ utopia.compiler > /dev/null
  
    Invalid page declarations:
      - Script module collision for Script__pages_client_a_b. Conflicting sources: pages/client/a-b.re (declared by /home), pages/client/a_b.re (declared by /about)
  
    Supported Next.js-style segments:
      * [id]
      * [...slug]
      * [[...slug]]
      * route groups: (marketing)
      * parallel slots: @slot (ignored for URL path)
    Script directive format:
      * @utopia.script ./relative/path/to/module.re
  [1]
