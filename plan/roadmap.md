## Implementation Roadmap

### Phase 1: Compiler changes
1. Update `generate_dune_rules` to emit new PPXes and libraries
2. Remove `@utopia.script` parsing and script manifest generation
3. Generate `client_entry.re` (RSC client shell)
4. Generate `esbuild.config.mjs` (esbuild config with plugin)
5. Generate `server_main.ml` (server executable wiring)
6. Generate esbuild dune rule
7. Generate server executable dune stanza
8. Update `routes.manifest` format if needed for RSC

### Phase 2: Server rewrite
1. Extract server.ml logic into a library (`utopia.server_lib`)
2. Replace HTML string rendering with `DreamRSC.stream_html`
3. Add RSC payload endpoint (check `Accept` header)
4. Add server function POST handler (`DreamRSC.streamFunctionResponse`)
5. Wire route matching to actual React component rendering
6. Asset serving for esbuild `dist/` output
7. Layout nesting via server-reason-react's component tree

### Phase 3: npm / package.json
1. Define expected `package.json` structure
2. Either generate it or validate it exists during `utopia build`
3. Ensure `node_modules/` is available to dune rules

### Phase 4: API routes
1. Compiler scans `api/` directory
2. Generates Dream handlers from API route modules
3. Wires into the generated server executable
4. Middleware support (`_middleware.ml`)

### Phase 5: SSG support
1. Pages with `let static = true` are rendered at build time
2. Output is static HTML served without server rendering
3. Build-time rendering uses the same RSC pipeline

### Phase 6: Markdown RSC integration
1. Markdown pages participate in the layout system
2. Markdown content can include client components (via custom components)
3. Markdown pages are wrapped in the RSC rendering pipeline

### Phase 7: Testing
1. Update existing cram tests for new compiler output
2. Add cram tests for RSC-specific scenarios (client components, server functions)
3. Create RSC demo project (`demo/rsc/`) for end-to-end validation
4. Test dev mode workflow (watch + rebuild + live reload)

### Phase 8: Dev mode
1. Update CLI to manage the per-project server executable
2. Live reload: detect dune rebuild completion, signal browser
3. Verify esbuild dune rule reruns correctly in watch mode

### Phase 9: App directory unification
1. Replace separate `pages/` + `api/` roots with unified `app/`
2. Use filename intent: `page.re|.ml|.mlx` for pages, `route.re|.ml|.mlx` for API handlers
3. Keep API middleware ancestry in `app/api/**/_middleware.*`
4. Add migration diagnostics/codemod support for existing `pages/` and `api/` projects
5. Update compiler/server/docs/tests to treat `app/` as canonical route root
