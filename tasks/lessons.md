# Lessons

- When fixing demo/build tooling regressions, run the exact user-facing entrypoint (`make` target or script) after changes instead of only running lower-level build commands; alias wiring can differ between layers.
- When the user explicitly narrows scope (for example, "skip migrate, update demos"), stop advancing the broader plan and pivot immediately to the requested slice.
- When documenting `app/` routing rules, explicitly define how non-reserved files under `app/**` behave (support modules vs route files) and state their visibility scope for `page.*`/`layout.*` consumers.
- When adding filesystem semantics (for example app-local modules), verify with a real demo build that module references resolve in generated `_utopia` mirrors; route-collection correctness alone is not enough.
- When runtime/module visibility changes, mirror the same visibility in source-owned `_utopia/dune` stanzas used by Merlin/ocamllsp; otherwise editor diagnostics drift from actual build behavior.
- When touching generated public APIs, preserve established names unless the user explicitly approves a compatibility break; default to existing route bindings such as `Routes.route`.
- When adding compiler scanning infrastructure that is expected to grow across roadmap items, choose a reusable abstraction/module name (for example `Analysis`) instead of a feature-specific name.
- Prefer explicit runtime mode flags (for example `--dev`) over introducing new environment variables when CLI process wiring can carry the same intent more clearly.
