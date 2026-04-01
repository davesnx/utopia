  $ mkdir -p demo/basic/pages demo/basic/lib demo/basic/_utopia demo/notes/pages demo/notes/lib demo/notes/_utopia
  $ printf "(lang dune 3.8)\n(using melange 0.1)\n" > dune-project
  $ printf "(dirs :standard demo)\n" > dune
  $ printf "(dirs :standard _utopia)\n" > demo/basic/dune
  $ printf "(dirs :standard _utopia)\n" > demo/notes/dune
  $ touch demo/basic/_utopia/dune demo/notes/_utopia/dune
  $ printf "let page = ()\n" > demo/basic/pages/Home.re
  $ printf "let page = ()\n" > demo/notes/pages/Home.re
  $ printf "let value = \"basic\"\n" > demo/basic/lib/BasicShared.re
  $ printf "let value = \"notes\"\n" > demo/notes/lib/NotesShared.re
  $ (cd demo/basic && utopia.compiler > /dev/null)
  $ (cd demo/notes && utopia.compiler > /dev/null)
  $ python3 - <<'PY'
  > import re
  > from pathlib import Path
  > basic = Path('demo/basic/_utopia/dune').read_text()
  > notes = Path('demo/notes/_utopia/dune').read_text()
  > assert re.search(r'\(library \(name pages_\S*demo_basic\) \(wrapped false\)', basic), 'basic library name'
  > assert '(modules server_main Utopia_server)' in basic, 'basic server modules'
  > assert re.search(r'pages_\S*demo_basic .* cmarkit', basic), 'basic server libraries'
  > assert '../lib/BasicShared.re' in basic, 'basic lib dep'
  > assert '../../lib/BasicShared.re' in basic, 'basic native lib dep'
  > assert re.search(r'\(library \(name pages_\S*demo_notes\) \(wrapped false\)', notes), 'notes library name'
  > assert '(modules server_main Utopia_server)' in notes, 'notes server modules'
  > assert re.search(r'pages_\S*demo_notes .* cmarkit', notes), 'notes server libraries'
  > assert '../lib/NotesShared.re' in notes, 'notes lib dep'
  > assert '../../lib/NotesShared.re' in notes, 'notes native lib dep'
  > PY
  $ grep -oE '"_utopia/target/demo/basic/_utopia"|"\.\.\/\.\.\/_build/default/demo/basic/_utopia/target/demo/basic/_utopia"|"_utopia/target/demo/notes/_utopia"|"\.\.\/\.\.\/_build/default/demo/notes/_utopia/target/demo/notes/_utopia"' demo/basic/_utopia/esbuild.config.mjs demo/notes/_utopia/esbuild.config.mjs
  demo/basic/_utopia/esbuild.config.mjs:"_utopia/target/demo/basic/_utopia"
  demo/basic/_utopia/esbuild.config.mjs:"../../_build/default/demo/basic/_utopia/target/demo/basic/_utopia"
  demo/notes/_utopia/esbuild.config.mjs:"_utopia/target/demo/notes/_utopia"
  demo/notes/_utopia/esbuild.config.mjs:"../../_build/default/demo/notes/_utopia/target/demo/notes/_utopia"
