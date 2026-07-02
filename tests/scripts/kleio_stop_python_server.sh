#!/usr/bin/env bash
# Stop the PYTHON kleio server started by kleio_start_python_server.sh.
#
# This is the Python counterpart of kleio_stop_server.sh. The Prolog stop script
# talks to a debug REPL on port 4000; the Python server has no such channel, so
# we stop it by PID. We give in-flight translation jobs a moment to finish, then
# send SIGTERM (and SIGKILL if needed).
#
# Usage:
#   source scripts/kleio_stop_python_server.sh
#
# Reads .python_server.pid / .python_server.port (written by the start script)
# if PYTHON_SERVER_PID is not in the environment.

# Run from the tests/ directory.
if [ -z "${PYTHON_SERVER_PID:-}" ] && [ -f .python_server.pid ]; then
  PYTHON_SERVER_PID="$(cat .python_server.pid)"
fi

if [ -z "${PYTHON_SERVER_PID:-}" ]; then
  echo "kleio_stop_python_server.sh: no PID known (PYTHON_SERVER_PID unset, no .python_server.pid)."
  return 0 2>/dev/null || exit 0
fi

echo "Waiting for in-flight translation jobs to finish..."
sleep 5

if kill -0 "${PYTHON_SERVER_PID}" 2>/dev/null; then
  echo "Stopping python kleio server (PID ${PYTHON_SERVER_PID})..."
  kill -TERM "${PYTHON_SERVER_PID}" 2>/dev/null || true
  # Wait up to 15s for a clean exit.
  for _ in $(seq 1 30); do
    kill -0 "${PYTHON_SERVER_PID}" 2>/dev/null || break
    sleep 0.5
  done
  if kill -0 "${PYTHON_SERVER_PID}" 2>/dev/null; then
    echo "Server did not exit on SIGTERM; sending SIGKILL."
    kill -KILL "${PYTHON_SERVER_PID}" 2>/dev/null || true
  fi
  echo "Python kleio server stopped."
else
  echo "Python kleio server (PID ${PYTHON_SERVER_PID}) already exited."
fi

rm -f .python_server.pid .python_server.port
unset PYTHON_SERVER_PID 2>/dev/null || true
unset PYTHON_SERVER_PORT 2>/dev/null || true
