---
title: Why Static Sites
date: 10/06/2025
description: The case for generating HTML at build time.
---

Static sites have a straightforward pitch: generate HTML at build time, serve
files from a CDN, avoid running application servers in production. The benefits
compound quickly.

**Performance** A static file served from the edge is the fastest response you
can get. No database queries, no template rendering, no cold starts.

**Reliability** Fewer moving parts means fewer failure modes. A directory of
HTML files does not go down at 3 AM because a process ran out of memory.

**Security** No server means no server to exploit. The attack surface shrinks
to your CDN configuration and the files themselves.

**Cost** Hosting static files is effectively free at any reasonable scale.

The tradeoff is that you give up per-request dynamism. But for content that
changes at authoring time rather than request time: blogs, documentation,
marketing pages. That is not a tradeoff at all.

In utopia, pages are static by default. Any page without a `before` hook is
rendered at build time:

```ocaml
let[@react.component] make () =
  <article>
    <p> (React.string "This page is rendered at build time.") </p>
  </article>
```

For pages with dynamic segments, you enumerate the paths:

```ocaml
let paths () = [
  [("slug", "hello-utopia")];
  [("slug", "why-static-sites")];
]
```

The compiler detects these exports, and `--ssg` mode renders each combination
to an HTML file.
