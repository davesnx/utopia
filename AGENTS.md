# Utopia Agent Rules

## Primitives Maintenance

When any concept, term, or abstraction is **added, modified, renamed, or removed** in the codebase or in any plan/spec document, `plan/primitives.md` must be updated in the same change. This includes:

- New types, modules, or abstractions introduced in source code
- Renamed or redefined terms in specs or plans
- New CLI commands, flags, or configuration options
- New file conventions or directory roles
- Removed or deprecated concepts (mark as deprecated or remove from primitives)

If a PR or commit introduces a concept that is not yet in `plan/primitives.md`, the change is incomplete.

## Reference Documents

- `plan/spec.md` -- North-star feature specification (target state, AI-agent-optimized)
- `plan/primitives.md` -- Canonical glossary of all concepts and terms
- `bin/vision.md` -- Original vision document (historical context)
- `plan/cli-subcommands-plan.md` -- CLI implementation roadmap
- `plan/melange-page-scripts-plan.md` -- Page scripts feature plan (deprecated, replaced by RSC)

## Rules

- All modules, variants, polyvariants should use this casing: Ocaml_case, not PamelCase.
