---
title: Markdown and Syntax Highlighting
date: 2025.06.01
description: Rendering markdown with ochre syntax highlighting.
---

This blog renders markdown files stored on the filesystem. The rendering
pipeline is: read the `.md` file, parse it with cmarkit, convert the AST to
React elements, and render to static HTML.

Syntax highlighting is handled by ochre, which uses TextMate grammars. This
means the same highlighting rules used by VS Code work here at build time,
producing accurate coloring for dozens of languages.

Here is some OCaml:

```ocaml
let fibonacci n =
  let rec aux a b = function
    | 0 -> a
    | n -> aux b (a + b) (n - 1)
  in
  aux 0 1 n
```

And some JavaScript:

```javascript
function debounce(fn, ms) {
  let timer;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => fn(...args), ms);
  };
}
```

The highlighting runs at build time so there is no client-side JavaScript
involved. The rendered HTML contains inline styles from the Gruvbox color
theme.
