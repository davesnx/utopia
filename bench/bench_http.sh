#!/usr/bin/env bash
# HTTP benchmarks for the utopia server using wrk.
#
# Prerequisites:
#   - wrk must be installed
#   - The local opam switch must have all dependencies installed
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
DEMO_DIR="${PROJECT_ROOT}/demo/notes"

log() { printf "${CYAN}==> ${RESET}%s\n" "$*"; }
error() { printf "${RED}Error: ${RESET}%s\n" "$*" >&2; exit 1; }

# Check prerequisites
command -v wrk >/dev/null 2>&1 || error "wrk is not installed. Install it first."

SERVER_EXE="${PROJECT_ROOT}/_build/default/demo/notes/_utopia/server_main.exe"

# Build the generated demo
log "Building generated notes demo..."
(make -C "${DEMO_DIR}" build >/dev/null 2>&1) || error "Failed to build generated demo"

if [ ! -x "${SERVER_EXE}" ]; then
  error "Generated server executable not found at ${SERVER_EXE}."
fi

# Start the generated server in the background (with logging disabled for clean benchmarks)
log "Starting generated server on port ${PORT}..."
(cd "${DEMO_DIR}" && NO_LOG=1 PORT="${PORT}" "${SERVER_EXE}" 2>/dev/null) &
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

# Read routes from generated page metadata for benchmarking
ROUTES=()
while IFS= read -r route; do
  ROUTES+=("${route}")
done < <(
  grep 'Utopia_types.page_route_meta' "${DEMO_DIR}/_utopia/Routes.ml" |
    sed -E 's/.*route = "([^"]*)".*/\1/' |
    awk '{ if ($0 == "") print "/"; else print "/" $0 }'
)

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
