  $ mkdir -p demo/basic/app demo/basic/lib demo/basic/_utopia demo/notes/app demo/notes/lib demo/notes/_utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard demo)\n" > dune
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > demo/basic/dune
  $ printf "(data_only_dirs _utopia)\n(include _utopia/dune)\n" > demo/notes/dune
  $ touch demo/basic/_utopia/dune demo/notes/_utopia/dune
  $ printf "let page = ()\n" > demo/basic/app/page.re
  $ printf "let page = ()\n" > demo/notes/app/page.re
  $ printf "let value = \"basic\"\n" > demo/basic/lib/BasicShared.re
  $ printf "let value = \"notes\"\n" > demo/notes/lib/NotesShared.re
  $ (cd demo/basic && utopia.compiler > /dev/null)
  $ (cd demo/notes && utopia.compiler > /dev/null)
  $ grep -qE '\(library \(name pages_\S*demo_basic\) \(wrapped false\)' demo/basic/_utopia/dune
  $ grep -qF '(executable (name server_main) (modules server_main)' demo/basic/_utopia/dune
  $ grep -qE 'libraries pages_\S*demo_basic utopia_\S*demo_basic utopia' demo/basic/_utopia/dune
  $ grep -qF '../../lib/BasicShared.re' demo/basic/_utopia/dune
  $ grep -qE '\(library \(name pages_\S*demo_notes\) \(wrapped false\)' demo/notes/_utopia/dune
  $ grep -qF '(executable (name server_main) (modules server_main)' demo/notes/_utopia/dune
  $ grep -qE 'libraries pages_\S*demo_notes utopia_\S*demo_notes utopia' demo/notes/_utopia/dune
  $ grep -qF '../../lib/NotesShared.re' demo/notes/_utopia/dune
  $ cat demo/basic/_utopia/paths.mjs
  export const projectPath = "demo/basic";
  export const buildMode = "development";
  export const nodeEnv = "development";
  $ cat demo/notes/_utopia/paths.mjs
  export const projectPath = "demo/notes";
  export const buildMode = "development";
  export const nodeEnv = "development";
