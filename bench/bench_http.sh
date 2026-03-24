#!/usr/bin/env bash
# HTTP benchmarks for the Utopia server using wrk.
#
# Prerequisites:
#   - wrk must be installed
#   - The compiler must have been run first (make compile-demo)
#
# Usage:
#   ./bench/bench_http.sh              # uses default port 9876
#   PORT=8080 ./bench/bench_http.sh    # use a specific port
#
# The script starts the server, runs benchmarks, and then stops it.

set -euo pipefail

PORT="${PORT:-9876}"
BASE_URL="http://localhost:${PORT}"
DURATION="${DURATION:-10s}"
THREADS="${THREADS:-4}"
CONNECTIONS="${CONNECTIONS:-200}"

# Colors
BOLD="\033[1m"
DIM="\033[2m"
RESET="\033[0m"
CYAN="\033[36m"
GREEN="\033[32m"
RED="\033[31m"

PROJECT_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DEMO_DIR="${PROJECT_ROOT}/demo/basic"

log() { printf "${CYAN}==> ${RESET}%s\n" "$*"; }
error() { printf "${RED}Error: ${RESET}%s\n" "$*" >&2; exit 1; }

# Check prerequisites
command -v wrk >/dev/null 2>&1 || error "wrk is not installed. Install it first."

if [ ! -f "${DEMO_DIR}/_utopia/routes.manifest" ]; then
  error "Route manifest not found at ${DEMO_DIR}/_utopia/routes.manifest. Run 'make compile-demo' first."
fi

# Build the server
log "Building server..."
(cd "${PROJECT_ROOT}" && opam exec -- dune build bin/Server.exe 2>&1) || error "Failed to build server"

# Start the server in the background (with logging disabled for clean benchmarks)
log "Starting server on port ${PORT}..."
(cd "${DEMO_DIR}" && NO_LOG=1 PORT="${PORT}" opam exec -- dune exec --no-print-directory utopia.server 2>/dev/null) &
SERVER_PID=$!

cleanup() {
  if kill -0 "${SERVER_PID}" 2>/dev/null; then
    kill "${SERVER_PID}" 2>/dev/null
    wait "${SERVER_PID}" 2>/dev/null || true
  fi
}
trap cleanup EXIT

# Wait for server to be ready
log "Waiting for server..."
for i in $(seq 1 30); do
  if curl -s -o /dev/null "${BASE_URL}/" 2>/dev/null; then
    break
  fi
  if ! kill -0 "${SERVER_PID}" 2>/dev/null; then
    error "Server process died during startup"
  fi
  sleep 0.1
done

# Verify server is running
if ! curl -s -o /dev/null "${BASE_URL}/" 2>/dev/null; then
  error "Server did not start in time"
fi

log "Server is ready (PID ${SERVER_PID})"

# Read routes from manifest for benchmarking
ROUTES=()
while IFS=$'\t' read -r route kind source_file matcher params layouts; do
  ROUTES+=("/${route}")
done < "${DEMO_DIR}/_utopia/routes.manifest"

printf "\n"
printf "${BOLD}  Utopia HTTP Benchmarks${RESET}\n"
printf "  %s\n" "$(printf '%.0s-' {1..72})"
printf "  ${DIM}wrk: ${THREADS} threads, ${CONNECTIONS} connections, ${DURATION} duration${RESET}\n"
printf "\n"

# Benchmark: Index page (route listing)
log "Benchmarking: GET / (index page)"
wrk -t"${THREADS}" -c"${CONNECTIONS}" -d"${DURATION}" --latency "${BASE_URL}/" 2>&1 | \
  sed 's/^/    /'
printf "\n"

# Benchmark each route from the manifest
for route in "${ROUTES[@]}"; do
  log "Benchmarking: GET ${route}"
  wrk -t"${THREADS}" -c"${CONNECTIONS}" -d"${DURATION}" --latency "${BASE_URL}${route}" 2>&1 | \
    sed 's/^/    /'
  printf "\n"
done

# Benchmark: 404 (route miss)
log "Benchmarking: GET /nonexistent (404)"
wrk -t"${THREADS}" -c"${CONNECTIONS}" -d"${DURATION}" --latency "${BASE_URL}/nonexistent/path/here" 2>&1 | \
  sed 's/^/    /'
printf "\n"

printf "  %s\n" "$(printf '%.0s-' {1..72})"
printf "  Done.\n\n"
