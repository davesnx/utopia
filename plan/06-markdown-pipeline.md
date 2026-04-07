# Markdown pipeline

Add full-YAML frontmatter metadata support, remove markdown renderer crashes, unify rendering paths, and integrate markdown into the RSC pipeline.

---

## Goal

Markdown pages should be first-class citizens in the RSC pipeline. They participate in layouts, render through the same React component tree as code pages, and expose frontmatter metadata through a server-side API. The renderer should never crash on table, footnote, or unknown-node input.

---

## Dependencies

- `plan/03-server-rewrite.md` -- server renders via DreamRSC

---

## Locked decisions

1. Frontmatter is generic metadata, not route/layout/SSG control
2. YAML parsing uses a real dependency (`Yaml`) with full YAML support
3. Frontmatter extraction only succeeds when YAML parses and root is an object/map
4. Parse failures warn and fall back; they do not fail rendering
5. Compiler parses and embeds frontmatter + stripped markdown body
6. Runtime markdown rendering does not read source markdown files
7. Markdown rendering is unified on the React path (`Render.of_doc`)
8. All reachable markdown renderer crash paths (`assert false`) are removed

---

## Frontmatter semantics

Frontmatter is an optional YAML block at the top of a markdown file:

```markdown
---
title: My Guide
author: Ada
is_published: true
tags:
  - docs
  - guide
---

# Content starts here
```

Extraction rules:

1. Candidate frontmatter exists only when file starts with `---\n` and contains a closing `---\n`
2. Text between the fences is parsed with `Yaml`
3. If parse succeeds and root is a YAML object/map:
   - frontmatter data is extracted
   - fenced block is stripped from markdown body
4. If parse fails or root is not an object/map:
   - emit warning on every compile run
   - keep full original markdown body unchanged (do not strip)
5. If closing fence is missing:
   - treat file as normal markdown (no warning)
6. Duplicate keys follow YAML parser semantics; effective behavior is last-key-wins

Frontmatter field policy:

- Any top-level key is allowed
- No reserved routing keys in this phase (`path`, `layout`, `static` are ordinary metadata keys)
- `title` and `description` are special-cased only for `<head>` metadata and only when values are top-level string scalars

---

## Frontmatter value model

Define a Utopia-owned value tree for frontmatter data:

```ocaml
type frontmatter_value =
  | Null
  | Bool of bool
  | Number of float
  | String of string
  | List of frontmatter_value list
  | Object of (string * frontmatter_value) list
```

Public API exposes an object map root (not arbitrary root values):

```ocaml
module Utopia.Markdown : sig
  type frontmatter_value = ...
  type frontmatter_object = (string * frontmatter_value) list

  val frontmatter : path:string -> frontmatter_object option
end
```

API behavior:

- Server-only API
- `path` is a concrete request path (`/posts/hello`, not route pattern)
- Dynamic markdown routes are matched internally against compiled route patterns
- Returns `None` when path is not a markdown route or no valid frontmatter exists

---

## Compiler integration

Compiler reads markdown files and embeds markdown payloads into generated artifacts:

1. Parse frontmatter at compile time
2. Store extracted frontmatter object for markdown routes in a markdown-only generated side-table (not `page_route_meta`)
3. Store stripped markdown body for each markdown route
4. Extract `title`/`description` from frontmatter object for metadata convenience
5. Emit warnings for parse failures/non-object roots, then continue

Implementation notes:

- Parsing internals can use typed error variants:
  - `Parse_error of { message : string; markdown : string }`
  - `Io_error of { message : string }`
- These error variants stay internal (not public API)

---

## Runtime integration and caching

Runtime rendering path for markdown routes:

1. Use compiler-embedded stripped markdown body
2. Parse markdown doc once per process (`Lazy.t` memoization)
3. Render via `Render.of_doc` with markdown components
4. Feed rendered React element into the same layout + DreamRSC path as code pages

Runtime no longer reads markdown files from disk during normal request handling.

---

## Fix table rendering

Current renderer crashes on tables (`assert false`). Implement full table rendering with granular component hooks:

- `table`
- `thead`
- `tbody`
- `tr`
- `th`
- `td`

Alignment output uses classes:

- `utopia-markdown-align-left`
- `utopia-markdown-align-center`
- `utopia-markdown-align-right`

No inline `style="text-align: ..."` output in default renderer.

---

## Fix footnote rendering

Current renderer crashes on footnotes (`assert false`). Implement semantic footnote rendering:

1. References render as `<sup><a ...>[N]</a></sup>`
2. Final footnote block renders as `<section class="footnotes"><ol>...</ol></section>`
3. Footnote items render as `<li id="fn-N">...</li>`
4. Backlinks are emitted from each footnote item to each reference site
5. Repeated references use per-reference backlink targets

Expose granular component hooks:

- `footnotes_section`
- `footnotes_list`
- `footnotes_item`
- `footnote_ref`
- `footnote_backref`

Invalid/partial footnote graphs degrade gracefully (never crash).

---

## Remove crash paths in renderer

Remove all reachable `assert false` branches in markdown rendering, not only tables/footnotes.

Fallback policy for unknown/unsupported nodes:

- render nothing for that node
- emit warning for diagnostics
- continue rendering remaining document

---

## Unify markdown rendering paths

The server currently has two markdown paths:

1. `utopia.markdown` executable via React (`Render.of_doc`)
2. Server markdown page rendering via direct HTML path

Unify to one React path:

- all markdown rendering goes through `Render.of_doc`
- remove markdown-specific HTML-only path in server runtime
- ensure output consistency between CLI markdown renderer and server markdown page rendering

---

## Custom components

`Components.t` remains extensible and gains the new table/footnote hooks. Default components from `markdown/elements.re` remain the baseline implementation for this phase. Project-level runtime configuration wiring for custom markdown components is deferred.

---

## Testing

### Cram tests

**`markdown_frontmatter_extracts_yaml_object.t`**
- Frontmatter includes nested YAML values
- Assert extracted map is available and frontmatter is stripped from body

**`markdown_frontmatter_invalid_yaml_warns_and_falls_back.t`**
- File starts with fenced block that fails YAML parse
- Assert compile warning
- Assert markdown body is not stripped

**`markdown_frontmatter_non_object_root_warns_and_falls_back.t`**
- Frontmatter YAML root is list/scalar
- Assert compile warning and fallback behavior

**`markdown_frontmatter_duplicate_keys_last_wins.t`**
- Duplicate key values in YAML
- Assert exposed value is the last one

**`markdown_frontmatter_title_description_metadata.t`**
- `title` and `description` as strings
- Assert they are available to metadata/head path

**`markdown_frontmatter_lookup_by_path.t`**
- Call `Utopia.Markdown.frontmatter ~path` with static markdown route
- Assert returned object

**`markdown_frontmatter_lookup_dynamic_route_path.t`**
- Dynamic markdown route with concrete request path lookup
- Assert internal pattern matching resolves frontmatter

**`markdown_table_rendering.t`**
- Assert `<table>`, `<thead>`, `<tbody>`, `<th>`, `<td>` are rendered

**`markdown_table_alignment_classes.t`**
- Assert `utopia-markdown-align-left|center|right` classes are present

**`markdown_footnote_rendering.t`**
- Assert `<sup>` references, `<section class="footnotes">`, `<ol>`, `<li>` and backlinks are rendered

**`markdown_footnote_repeated_references.t`**
- Multiple references to same footnote
- Assert per-reference backlink targets

**`markdown_unknown_nodes_no_crash.t`**
- Feed unsupported/edge markdown constructs
- Assert renderer does not crash and still emits output

### Update existing markdown tests

Promote `markdown/tests/main.t` and `markdown/tests/simple.t` expected output after pipeline unification.

### Edge cases

- Frontmatter with unknown fields
- Frontmatter with empty string values (`title: ""`)
- Frontmatter with no closing `---`
- Frontmatter block `---\n---\n` (empty object)
- Frontmatter with very large nested YAML payload
- Frontmatter with special characters and quoted strings
- Frontmatter list/scalar root (warn + fallback)
- Table with mismatched column counts
- Table with empty cells
- Table nested inside list items
- Footnote with multiple paragraphs
- Footnote referenced but not defined
- Footnote defined but not referenced
- Very large markdown body (100KB+)

---

## Performance

Compile-time frontmatter parsing and embedding removes runtime markdown file I/O for normal request handling. Runtime markdown parse/render work is reduced by memoizing parsed docs per markdown page.

---

## Files changed

| Action | File |
|--------|------|
| Create | `markdown/frontmatter.ml` (Yaml-based extraction + value conversion) |
| Modify | `markdown/utopia_markdown.ml` (frontmatter + stripped-body pipeline) |
| Modify | `markdown/render.ml` (table, footnote, crash-safe fallbacks) |
| Modify | `markdown/components.ml` (granular table/footnote hooks) |
| Modify | `markdown/elements.re` (default table/footnote component implementations) |
| Modify | `bin/compiler/Routes.ml` (collect markdown payload metadata) |
| Modify | `bin/compiler/Generated_routes.ml` (emit markdown frontmatter side-table) |
| Modify | `bin/compiler/Server_main.ml` (wire embedded markdown payloads/registry) |
| Modify | `lib/utopia/Utopia_server.ml` (remove markdown HTML-only path; render embedded markdown via React) |
| Create | `lib/utopia/Utopia_markdown.ml` (server-side `Utopia.Markdown.frontmatter ~path`) |
| Modify | `lib/utopia/Utopia.re` (expose `Utopia.Markdown`) |
| Create | `markdown/tests/frontmatter.t` |
| Create | `markdown/tests/tables.t` |
| Create | `markdown/tests/footnotes.t` |
| Modify | `markdown/tests/main.t` |
| Modify | `markdown/tests/simple.t` |

---

## Acceptance criteria

- Frontmatter extraction uses `Yaml` and accepts full YAML only when root is an object/map
- Invalid/non-object frontmatter emits warning and falls back to unchanged markdown body
- Frontmatter is generic metadata (no route/layout/static behavior in this phase)
- `title` and `description` are auto-derived only from top-level string values
- Compiler embeds stripped markdown body and frontmatter object side-table for markdown routes
- Runtime markdown rendering does not read markdown files in normal request path
- `Utopia.Markdown.frontmatter ~path` returns frontmatter object by concrete request path
- Markdown rendering is unified on React (`Render.of_doc`) across CLI and server paths
- Tables render with semantic elements and alignment classes
- Footnotes render with semantic section/list markup and per-reference backlinks
- No reachable `assert false` crash path remains in markdown renderer
- Unknown markdown nodes degrade gracefully (render nothing + warn)
- Markdown cram tests pass
