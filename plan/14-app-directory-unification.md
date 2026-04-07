# App directory unification

Unify page routes and API routes under a single `app/` root.

---

## Goal

Replace split route roots (`pages/` and `api/`) with one canonical tree where file basename determines route intent:

- `page.re|.ml|.mlx` (and `page.md`) -> page route
- `route.re|.ml|.mlx` -> API route

This aligns route ergonomics, simplifies compiler traversal, and makes mixed page/API features easier to colocate.

---

## Locked decisions

1. Canonical route root is project-root `app/`.
2. Page modules are only discovered from `page.*` basenames.
3. API handlers are only discovered from `route.*` basenames.
4. API namespace remains `/api/*` and maps from `app/api/**/route.*`.
5. `route.*` outside `app/api/**` is a compile-time error.
6. `page.*` inside `app/api/**` is a compile-time error.
7. API middleware remains `_middleware.re|.ml|.mlx` and composes by physical ancestry under `app/api/`.
8. Layout files remain `layout.re|.ml|.mlx` and apply by ancestry under `app/`.
9. Route segment syntax is unchanged (`[id]`, `[...slug]`, `[[...slug]]`, route groups, parallel slots).
10. Markdown pages move from arbitrary `*.md` under `pages/` to `page.md` under `app/**`.

---

## Target filesystem model

```text
app/
  layout.mlx
  page.mlx                    # /
  about/
    page.mlx                  # /about
  notes/
    [tag]/
      page.mlx                # /notes/:tag
  api/
    _middleware.ml
    health/
      route.ml                # /api/health
    users/
      [id]/
        route.ml              # /api/users/:id
```

---

## Compiler and runtime changes

### 1) Route collection

- Replace dual-root scanning with one recursive pass over `app/`.
- For each directory:
  - collect `layout.*` (max one)
  - collect `page.*` (max one per directory across extensions)
  - collect `route.*` (max one per directory across extensions)
  - collect `_middleware.*` only under `app/api/**`
- Reuse existing segment parser based on directory names.

### 2) Validations

- Error if both `page.re` and `page.ml` (or any multi-extension duplicate) exist in one directory.
- Error if both `route.re` and `route.ml` (or any multi-extension duplicate) exist in one directory.
- Error if `route.*` appears outside `app/api/**`.
- Error if `page.*` appears inside `app/api/**`.
- Keep conflict-key validation for page routes and API routes separately.

### 3) Generated metadata

- `Routes.get_all ()` continues returning page metadata.
- `Routes.Api.get_all ()` continues returning API metadata.
- `source_file` paths now point at `app/**/page.*` or `app/api/**/route.*`.

### 4) Generated dune wiring

- Mirror sources from `app/` instead of `pages/` + `api/`.
- Keep separate native page/API libraries in generated dune for link boundaries.
- Preserve Melange/native shared-lib behavior.

### 5) Request handling

- No request-order change: assets -> API -> actions -> pages.
- API dispatch still keys off `/api/*`.

---

## Migration strategy

### Step 1: Dual-read compatibility window

- Compiler accepts both models:
  - new: `app/`
  - legacy: `pages/` + optional `api/`
- If both are present, `app/` wins and compiler emits a warning listing ignored legacy roots.

### Step 2: Migration assistant

- Add CLI helper: `utopia migrate app-dir` (or equivalent script) that:
  - moves `pages/**` files into `app/**/page.*` shape
  - moves `api/**` files into `app/api/**/route.*` shape
  - updates obvious path references in local docs/tests

### Step 3: Deprecation enforcement

- After one release cycle, legacy roots become hard errors unless an explicit legacy flag is set.
- Remove legacy flag in the following cycle.

---

## Testing plan

Add or update cram tests for:

1. `app/page.re` root routing
2. nested `app/**/page.*` dynamic/catch-all routing
3. `app/api/**/route.*` API routing + middleware ancestry
4. compile error: `route.*` outside `app/api/**`
5. compile error: `page.*` inside `app/api/**`
6. compile error: duplicate `page.*` in same directory
7. compile error: duplicate `route.*` in same directory
8. compatibility mode reading legacy `pages/` + `api/`
9. precedence warning when both legacy and `app/` are present
10. markdown `app/**/page.md` handling

---

## Docs and terminology sync

Update in same change:

- `plan/primitives.md`
- `plan/spec.md`
- `plan/roadmap.md`
- phase docs that explicitly prescribe `pages/`/`api/` conventions when those slices are touched next

This plan defines the new canonical vocabulary: **App Directory**, **Page file (`page.*`)**, and **API route file (`route.*`)**.
