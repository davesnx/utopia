# Dev mode

Upgrade the development workflow with live reload, npm integration, and proper server restart.

---

## Goal

When a source file changes, the developer sees the update in their browser without manual intervention. The dev server restarts when the compiled server executable changes. esbuild re-runs when melange output changes. npm dependencies are validated and available.

---

## Dependencies

- `plan/02-compiler-rsc.md` -- compiler generates esbuild rules
- `plan/03-server-rewrite.md` -- server is a per-project executable
- `plan/04-client-components.md` -- esbuild pipeline works

---

## Manage the per-project server executable

The current CLI spawns `utopia.server` (the framework's standalone server). After the rewrite, the CLI spawns the per-project server executable at `_build/default/_utopia/server_main.exe`.

Update `cmd_dev` to:
1. Build the project (which compiles `server_main.exe`)
2. Spawn `_build/default/_utopia/server_main.exe` instead of `utopia.server`
3. Monitor the executable file for changes
4. When the executable changes (dune rebuilt it), kill the old process and spawn a new one

---

## Implement server restart on rebuild

Watch the mtime of `_build/default/_utopia/server_main.exe`. When it changes:

1. Send SIGTERM to the running server process
2. Wait for it to exit (with a timeout)
3. If it doesn't exit, send SIGKILL
4. Spawn the new executable
5. Update the tracked PID

This polling can use a dedicated thread or integrate with the Lwt event loop. Use a 500ms polling interval.

```ocaml
let watch_executable ~path ~on_change =
  let last_mtime = ref (file_mtime path) in
  let rec loop () =
    let* () = Lwt_unix.sleep 0.5 in
    let current = file_mtime path in
    if current <> !last_mtime then (
      last_mtime := current;
      on_change ());
    loop ()
  in
  loop ()
```

---

## Implement live reload

For the first version, use full page reload (not HMR). The approach:

1. The server injects a small `<script>` tag into HTML responses during dev mode
2. The script opens an EventSource (SSE) connection to `/_utopia/live-reload`
3. When the CLI detects a successful rebuild, it signals the server
4. The server sends an SSE event to all connected clients
5. The client script calls `window.location.reload()`

The live reload script:

```javascript
const source = new EventSource("/_utopia/live-reload")
source.onmessage = () => window.location.reload()
```

The server endpoint:

```ocaml
let live_reload_handler _request =
  let stream, push = Dream.stream () in
  (* Register this connection *)
  add_sse_client push;
  Dream.respond ~headers:[
    ("Content-Type", "text/event-stream");
    ("Cache-Control", "no-cache");
    ("Connection", "keep-alive");
  ] stream
```

---

## Signal rebuild completion

The CLI already subscribes to dune RPC progress events. When a `Success` progress event arrives:

1. Check if the server executable changed (mtime check)
2. If yes, restart the server
3. Send an SSE "reload" event to all connected live-reload clients

This ties into the existing RPC event loop in `cmd_dev`.

---

## Validate npm dependencies

Before starting the dev server, validate that `package.json` exists and required npm packages are installed:

```ocaml
let validate_npm () =
  if not (file_exists "package.json") then
    print_warn "No package.json found. Client components require npm dependencies.";
  if not (is_directory "node_modules") then (
    print_step "Installing npm dependencies";
    let code = run_command "npm" ["install"] in
    if code <> 0 then
      print_err "npm install failed")
```

Required packages: `react`, `react-dom`, `esbuild`, `server-reason-react-esbuild-plugin`, `server-reason-react-server-dom-esbuild`.

---

## Verify esbuild runs in watch mode

The esbuild dune rule depends on `(alias melange)`. When dune watch mode detects melange output changes, it re-runs the esbuild rule automatically. Verify this works by:

1. Starting `utopia dev`
2. Modifying a client component
3. Checking that `dist/` files are updated
4. Checking that the live reload triggers

---

## Update prod command

Update `cmd_prod` to spawn the per-project server executable instead of `utopia.server`:

```ocaml
let server_exe = "_build/default/_utopia/server_main.exe" in
if not (file_exists server_exe) then (
  print_err "Server executable not found. Run 'utopia build' first.";
  exit 1);
spawn server_exe [] env
```

---

## Update build command

Update `cmd_build` to validate npm dependencies and report on esbuild output.

---

## Testing

### Cram tests

**`dev_starts_per_project_server.t`**
- Build a project with the new compiler
- Verify the per-project server executable exists at the expected path

**`dev_npm_validation.t`**
- Create a project without `package.json`
- Run `utopia dev` (or just the validation step)
- Assert warning is printed

**`prod_uses_per_project_server.t`**
- Build a project
- Run `utopia prod`
- Verify it spawns `_build/default/_utopia/server_main.exe`

### Edge cases

- Server executable doesn't exist at startup (build failed)
- Server executable is deleted while running
- Server crashes during restart
- Multiple rapid file changes (debouncing)
- Very slow build (server restart timeout)
- npm install hangs
- node_modules exists but is corrupted
- SSE connection drops and reconnects
- Multiple browser tabs with live reload
- Live reload during a failed build (should not reload)
- SIGINT during server restart
- Port already in use when restarting server

---

## Performance

- Server restart adds ~100-500ms latency after a rebuild (process spawn time)
- SSE connections are lightweight (one per browser tab)
- Mtime polling at 500ms is negligible
- npm validation runs once at startup

---

## Files changed

| Action | File |
|--------|------|
| Modify | `bin/cli.ml` (per-project server, restart, npm validation, live reload signal) |
| Modify | `lib/utopia_server/utopia_server.ml` (SSE live reload endpoint, dev mode script injection) |
| Create | `bin/tests/dev_starts_per_project_server.t` |
| Create | `bin/tests/dev_npm_validation.t` |
| Create | `bin/tests/prod_uses_per_project_server.t` |

---

## Acceptance criteria

- `utopia dev` spawns the per-project server executable
- Source file changes trigger rebuild + live reload in the browser
- Server restarts automatically when the executable is recompiled
- npm dependencies are validated at startup
- `utopia prod` uses the per-project server executable
- esbuild re-runs automatically in watch mode
- Live reload only triggers on successful builds
- Clean shutdown on SIGINT/SIGTERM
- All tests pass
