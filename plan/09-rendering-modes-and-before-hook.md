# Rendering Modes And Before Hook

Derive rendering mode from page exports: pages are static by default, and `let before` opts into dynamic (request-time) rendering.

---

## Goal

1. Rendering behavior is derived implicitly from page capabilities.
2. Pages are static by default -- no explicit opt-in declaration needed.
3. `let before` is the sole signal for request-time rendering.
4. SSG remains deterministic and compiler-validated.
5. `static_paths` is renamed to `paths` for clarity.

---

## Why

Rendering mode should follow from what the page actually needs, not from a redundant declaration. If a page has no request-time hook, it can be pre-rendered. If it declares `let before`, it needs the server at request time.

The contract:

- absence of `before` → static (build-time rendering)
- presence of `before` → dynamic (request-time rendering)
- `paths` enumerates build-time routes for static pages with dynamic segments

---

## Design

### Static by default

All code pages without a `let before` export are static. The compiler derives this by scanning for `let before` using the `Analysis` lexical scanner. If `before_export_origin` is `None`, the page is static.

Markdown pages are always static (they cannot declare `before`).

### Dynamic via `before`

A page that exports `let before` is dynamic -- it will be rendered at request time via SSR. The `before` hook receives the request and returns data for the page render (runtime wiring is a follow-up; detection is implemented now).

```ocaml
let before _request = ()
```

### `paths` export (renamed from `static_paths`)

Static pages with dynamic segments (e.g., `app/posts/[slug]/page.mlx`) must export `let paths` to enumerate all param combinations at build time:

```ocaml
let paths () = [
  [("slug", "hello-world")];
  [("slug", "second-post")];
]
```

If a static page has dynamic params but no `paths`, the compiler emits an error suggesting either adding `paths` or adding `let before` to make the page dynamic.

---

## Compiler Contract (implemented)

### Static detection

- `bin/compiler/Analysis.ml` scans for `let before` as token sequence `["let"; "before"]`.
- A page is static when `before_export_origin = None`.
- `let paths` is detected as token sequence `["let"; "paths"]`.
- Comment/string/char-literal-safe scanning prevents false positives.

### Dynamic-segment validation

- `bin/compiler/Diagnostics.ml` validates that static pages with params have `paths`.
- Error messages suggest both `paths` and `before` as remediation options.

### Removed

- `let rendering = \`Static` detection removed (no longer needed).
- `static_paths` renamed to `paths` throughout.

---

## Runtime Contract

- Static routes serve pre-rendered HTML from `_utopia/static/*.html` when present.
- Missing static files fall back to SSR.
- Dev mode (`--dev`) always server-renders, bypassing static HTML.
- Dynamic routes (pages with `before`) always server-render.

---

## Demo Migration (complete)

Blog and md demo pages had `let rendering = \`Static` removed (now static by default):
- `demo/blog/pages/index.mlx`
- `demo/blog/pages/about.mlx`
- `demo/blog/pages/posts/index.mlx`
- `demo/blog/pages/posts/[slug]/index.mlx` (`static_paths` renamed to `paths`)
- `demo/md/app/page.mlx`
- `demo/blog/content/why-static-sites.md` (documentation updated)

Notes demo pages had `let before _request = ()` added (they do DB queries):
- `demo/notes/app/page.mlx`
- `demo/notes/app/notes/page.mlx`
- `demo/notes/app/notes/new/page.mlx`
- `demo/notes/app/notes/[tag]/page.mlx`

---

## Testing (complete)

- `ssg_static_page_detected.t` -- pages without `before` are static, pages with `before` are dynamic
- `ssg_before_makes_page_dynamic.t` -- `let before` makes a page dynamic
- `ssg_static_detection_ignores_comments_and_strings.t` -- `let before` in comments/strings is ignored
- `ssg_dynamic_page_requires_paths.t` -- static page with dynamic params requires `paths`
- `ssg_static_page_rendered.t` -- build-time HTML rendering
- `ssg_dynamic_page_with_paths.t` -- multi-path dynamic static pages
- `ssg_non_static_page_ignored.t` -- dynamic pages produce no static HTML
- `ssg_build_runs_ssg.t` -- `utopia export` end-to-end
- `ssg_server_static_serving_and_dev_fallback.t` -- static serving + dev SSR fallback

---

## Files Changed

| Action | File |
|--------|------|
| Modify | `bin/compiler/Analysis.ml` (remove `rendering` detection, add `before`/`paths` detection) |
| Modify | `bin/compiler/Diagnostics.ml` (derive static from `before`, rename `static_paths`→`paths`) |
| Modify | `bin/compiler/Routes.ml` (rename fields) |
| Modify | `bin/compiler/Generated_routes.ml` (field references) |
| Modify | `bin/compiler/Server_main.ml` (`static_paths`→`paths`) |
| Modify | `bin/compiler/compiler.ml` (rename diagnostic call) |
| Modify | `lib/utopia/Utopia_types.ml` (`has_static_paths`→`has_paths`) |
| Modify | `lib/utopia/Utopia_server.ml` (`static_paths`→`paths`) |
| Modify | demo pages (remove `rendering` export, add `before`, rename `static_paths`→`paths`) |
| Create | `ssg_before_makes_page_dynamic.t` |
| Delete | `ssg_legacy_static_export_ignored.t` |
| Modify | all SSG and compiler tests |
| Modify | `plan/07-ssg.md`, `plan/08-dev-mode.md`, `plan/primitives.md` |

---

## Follow-up (before hook runtime)

Detection is implemented. Remaining work:

1. Define page-level `before` signature and typed return shape.
2. Wire request values (cookies, headers, search params) through page metadata/render context.
3. Pass `before` return value to `make` as props.
4. Add tests for hook execution order and error handling.
