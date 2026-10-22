#!/usr/bin/env bash
# Run the Todo example.
#
#   ./run.sh generate       generate examples/todo/gen from openapi.yaml
#   ./run.sh server [PORT]  serve the API (default port: 8080)
#   ./run.sh client         call the running API from Scheme
#   ./run.sh demo           generate, start the server, run the client, stop it
#
# The example uses the development manifest at the repository root, so Guix is
# the only prerequisite.

set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
manifest="$root/manifest.scm"

generate() {
  ( cd "$here" && guix shell -m "$manifest" -- \
      guile -L "$root/src" \
      -c '((@ (openapi-generator cli) main)
           (list "generate" "--config" "openapi-generator.yaml"))' )
}

server() {
  guix shell -m "$manifest" -- \
    guile -L "$here/gen" -L "$root/src" "$here/app.scm" "${1:-8080}"
}

client() {
  guix shell -m "$manifest" -- \
    guile -L "$here/gen" -L "$root/src" "$here/client.scm"
}

demo() {
  generate
  # One environment for the server and the client, so the trap inside bash
  # stops the server it started rather than a wrapper process.
  guix shell -m "$manifest" -- bash -c '
    set -euo pipefail
    example="$1"
    root="$2"
    guile -L "$example/gen" -L "$root/src" "$example/app.scm" 8080 \
      >"$example/.server.log" 2>&1 &
    server_pid=$!
    trap "kill $server_pid 2>/dev/null || true" EXIT
    ready=""
    for attempt in $(seq 1 50); do
      if guile -c "(use-modules (web client))
                   (http-request \"http://127.0.0.1:8080/todos\")" \
           >/dev/null 2>&1; then
        ready="yes"
        break
      fi
      sleep 0.2
    done
    if [ -z "$ready" ]; then
      echo "the Todo server did not start; log follows:" >&2
      cat "$example/.server.log" >&2
      exit 1
    fi
    guile -L "$example/gen" -L "$root/src" "$example/client.scm"
  ' _ "$here" "$root"
}

case "${1:-demo}" in
  generate)
    generate
    ;;
  server)
    shift
    server "${1:-8080}"
    ;;
  client)
    client
    ;;
  demo)
    demo
    ;;
  *)
    echo "usage: $0 [generate|server [PORT]|client|demo]" >&2
    exit 2
    ;;
esac
