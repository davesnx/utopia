# SSG

Add opt-in static site generation. Pages are static by default; pages with `let before` are dynamic.

---

## Goal

Pages without a `before` hook are rendered at build time. The resulting HTML is served directly without server-side rendering at request time. Pages with `let before` are rendered at request time (SSR).

---

## Dependencies

- `plan/02-compiler-rsc.md` -- compiler generates server_main.ml
- `plan/03-server-rewrite.md` -- server library with DreamRSC rendering
- `plan/09-rendering-modes-and-before-hook.md` -- rendering mode derivation contract

---

## Detect static pages in the compiler

The compiler scans page source files using the `Analysis` module (`bin/compiler/Analysis.ml`), a lightweight lexical scanner that tokenizes code while ignoring comments, string literals, and char literals. A page is static when it does **not** export `let before`.

The scanner detects:
- `let before` as token sequence `["let"; "before"]` -- page is dynamic
- `let paths` as token sequence `["let"; "paths"]` -- page has build-time path enumeration
- No `before` -- page is static

Markdown pages are always static (they cannot declare `before`).

---

## Record static flag in route metadata

The route metadata types carry a `static` boolean and `has_paths` flag:

```ocaml
type page_route_meta = {
  ...
  static : bool;
  has_paths : bool;
}
```

The compiler's `Diagnostics.detect_static_for_entry` reads each page source, runs `Analysis.analyze`, and sets:
- `static = (before_export_origin = None)` for code pages
- `static = true` for markdown pages
- `has_paths = (paths_origin <> None)`

These propagate into generated route registries (`_utopia/Routes.ml`).

---

## Build-time rendering

During `dune build`, static pages are rendered to HTML files. The build pipeline:

1. Compiler marks pages as static in the generated route registries
2. The generated `server_main.ml` includes a `--ssg` mode that renders static pages through the same server/RSC HTML rendering path used for normal requests
3. A dune rule runs the server executable in SSG mode

Using the shared HTML path ensures layout composition, head metadata, and client bootstrap behavior stay consistent between SSR and SSG.

---

## Serve static pages

The server checks the static flag when handling requests. For static pages:

1. Check if the pre-rendered HTML file exists in `_utopia/static/`
2. If yes, serve it directly (no React rendering)
3. If no, fall back to server-side rendering (graceful degradation)

In `--dev` mode, the server always falls back to server-side rendering, bypassing pre-rendered static HTML even when available.

---

## Static pages with dynamic segments

A static page with dynamic segments (e.g., `app/posts/[slug]/page.mlx`) requires `let paths` to enumerate the build-time routes:

```ocaml
let paths () = [
  [("slug", "hello-world")];
  [("slug", "second-post")];
]
```

The SSG renderer calls `paths` to get all param combinations and renders each one.

If a static page (no `before`) has dynamic params but no `paths`, the compiler emits an error suggesting either adding `paths` or adding `let before` to make the page dynamic.

---

## Testing

### Cram tests

**`ssg_static_page_detected.t`**
- Pages without `before` are static; pages with `before` are dynamic

**`ssg_before_makes_page_dynamic.t`**
- `let before` makes a page dynamic (`static = false`)

**`ssg_static_page_rendered.t`**
- Static page produces `_utopia/static/about.html` with rendered content

**`ssg_dynamic_page_requires_paths.t`**
- Static page with dynamic params but no `paths` produces compiler error

**`ssg_dynamic_page_with_paths.t`**
- Dynamic static page with `paths` generates multiple HTML files

**`ssg_non_static_page_ignored.t`**
- Dynamic page (with `before`) produces no static HTML

**`ssg_static_detection_ignores_comments_and_strings.t`**
- `let before` in comments/strings is ignored (page stays static)

**`ssg_build_runs_ssg.t`**
- `utopia export` end-to-end creates static HTML

**`ssg_server_static_serving_and_dev_fallback.t`**
- Server prefers static HTML in production mode
- Fallback to SSR when static file is deleted
- Dev mode always server-renders

### Edge cases

- `let before` in a comment (should not be detected)
- `let before` in a string literal (should not be detected)
- Static page with layouts (layouts rendered into static HTML)
- Static page with client components (client JS included)
- Static page that throws an exception during rendering
- Server functions on static pages (POST handling is independent of static flag)

---

## Performance

Static pages are rendered once at build time. Serving them is a simple file read -- the fastest possible response path.

---

## Files changed

| Action | File |
|--------|------|
| Modify | `bin/compiler/Analysis.ml` (lexical scanner with `before`/`paths` detection) |
| Modify | `bin/compiler/Diagnostics.ml` (derive static from `before`, validate `paths`) |
| Modify | `bin/compiler/Routes.ml` (static + has_paths fields) |
| Modify | `bin/compiler/Generated_routes.ml` (emit static metadata) |
| Modify | `bin/compiler/Server_main.ml` (generate --ssg mode) |
| Modify | `lib/utopia/Utopia_server.ml` (serve static pages, SSG rendering) |
| Modify | `lib/utopia/Utopia_types.ml` (static + has_paths fields) |
| Create | SSG tests (9 cram tests) |

---

## Acceptance criteria

- Pages without `before` are static by default
- Pages with `before` are dynamic
- Markdown pages are always static
- Static pages are rendered at build time to HTML files
- Static HTML is served directly without server-side rendering
- Static pages with dynamic segments require `paths` export
- Fallback to SSR works when static HTML is missing
- Dev mode always server-renders, bypassing static HTML
- All tests pass
