# Lessons

- When fixing demo/build tooling regressions, run the exact user-facing entrypoint (`make` target or script) after changes instead of only running lower-level build commands; alias wiring can differ between layers.
