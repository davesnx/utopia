---
title: Hello utopia
date: 15/06/2025
description: An introduction to building sites with the utopia.
---

Hello, **utopia** is a static site generator built on OCaml and server-reason-react. It
takes a file-based routing approach. Drop a `.mlx` file into `pages/` and you
get a route.

What makes it different from the usual suspects is the language: OCaml gives you
a type system that catches mistakes at compile time, pattern matching that makes
data transformations readable, and a build system (dune) that tracks
dependencies automatically.

Here is a minimal page component:

```ocaml
let[@react.component] make () =
  <div className="p-8">
    <h1> (React.string "Hello from utopia") </h1>
  </div>
```

Pages are server components by default. There is no client-side JavaScript
unless you explicitly opt in with `[@react.client.component]`. For a blog like
this one, that means zero JS shipped to the reader.

The SSG mode renders every static page at build time, producing plain HTML files
you can host anywhere, no server process required.
