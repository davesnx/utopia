---
title: OCaml on the Web
date: 2025.06.05
description: Using OCaml and Reason for web development.
---

Using OCaml for web development is unusual. Most people reach for JavaScript or
TypeScript, and the ecosystem is built around those languages. So why bother?

The type system is the main draw. OCaml's type inference means you rarely write
type annotations, but the compiler still catches entire categories of bugs.
Pattern matching with exhaustiveness checking means you handle every case---the
compiler tells you when you miss one.

Here is a server component that reads data and renders it:

```ocaml
let make () =
  let posts = Blog_data.posts in
  List.map (fun post -> post.title) posts
```

No `useEffect`. No `useState`. No loading spinners. The data is available
synchronously because the component runs on the server at build time.

Melange compiles OCaml to JavaScript when you need client-side interactivity.
The same language, the same types, both sides of the stack. A function defined
in a shared `lib/` directory is available to both server and client code.

The tooling has matured. Dune handles builds, opam manages packages, and
editors support OCaml well through `ocaml-lsp`. The experience is not as
polished as the JavaScript ecosystem, but it is solid and improving.

For a content site like this blog, OCaml on the server is a natural fit: read
some markdown files, render them to HTML, write the output. No runtime
surprises.
