---
title: OCaml on the Web
date: 05/06/2025
description: Using OCaml for web development.
---

Using OCaml for web development might be unusual. Most people reach for JavaScript/
TypeScript, and the ecosystem is built around those languages. So why bother?

The type system is the main draw. OCaml's type inference means you rarely write
type annotations, but the compiler still catches entire categories of bugs.
Pattern matching with exhaustiveness checking means you handle every case, the
compiler tells you when you miss one.

Here is a server component that reads data and renders it:

```ocaml
let make () =
  let posts = Blog_data.posts in
  List.map (fun post -> post.title) posts
```

The data is available synchronously because the component runs on the server at build time.

Melange compiles OCaml to JavaScript when you need client-side interactivity.
The same language, the same types, both sides of the stack. A function defined
in a shared `lib/` directory is available to both server and client code.

The tooling has matured. Dune handles builds, opam manages packages, and
editors support OCaml well through `ocaml-lsp`. The experience is not as
polished as the JavaScript ecosystem, yet, but it is solid and improving.
