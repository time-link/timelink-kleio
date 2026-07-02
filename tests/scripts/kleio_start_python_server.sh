#!/usr/bin/env bash
# Start the PYTHON kleio server (uvicorn) in the background.
#
# This is the Python counterpart of kleio_start_server.sh. It launches the
# FastAPI/uvicorn server so that kleio_translate_python.sh can drive it through
# the REST contract, mirroring how the Prolog dev server is launched and driven
# by run_tests.sh.
#
# The server writes logs to kleio_start_python_server.log (relative to tests/)
# and prints its PID to stdout so the orchestrator can stop it. The server is
# started with KLEIO_HOME_DIR pointing at tests/kleio-home so it shares the same
# data root as the stable Prolog translator used for the reference leg.
#
# Usage:
#   source scripts/kleio_start_python_server.sh
#
# Exports (so kleio_stop_python_server.sh can find it):
#   PYTHON_SERVER_PID   the uvicorn process id
#   PYTHON_SERVER_PORT  the port the server is listening on
#
# Environment (expected to be set by prepare_tests.sh):
#   KLEIO_HOME_DIR     server home dir
#   KLEIO_SERVER_PORT  port to listen on (default 8088)
#   KLEIO_ADMIN_TOKEN  admin token (auto-grants full access on the server)
#   KLEIO_DEBUG        if "true", start uvicorn with --log-level debug

# Run from the tests/ directory.
PORT="${KLEIO_SERVER_PORT:-8088}"
TOKEN="${KLEIO_ADMIN_TOKEN:-admintoken}"
HOME_DIR="${KLEIO_HOME_DIR:-kleio-home}"

# Resolve HOME_DIR to an absolute path (the server is launched from tests/).
case "${HOME_DIR}" in
  /*) ABS_HOME="${HOME_DIR}" ;;
  *)  ABS_HOME="$(pwd)/${HOME_DIR}" ;;
esac

# The package lives at the repo root (one level up from tests/). Rather than
# `cd` into it (which would mutate the caller's CWD, since this script is
# sourced), we put the repo root on PYTHONPATH so uvicorn can import `kleio`
# from the current directory.
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
export PYTHONPATH="${REPO_ROOT}${PYTHONPATH:+:${PYTHONPATH}}"

LOG_FILE="kleio_start_python_server.log"

export KLEIO_HOME_DIR="${ABS_HOME}"
export KLEIO_SERVER_PORT="${PORT}"
export KLEIO_ADMIN_TOKEN="${TOKEN}"
export KLEIO_PORT="${PORT}"

LOG_LEVEL="info"
if [ "${KLEIO_DEBUG:-}" = "true" ]; then
  LOG_LEVEL="debug"
fi

echo "================================================"
echo "Launching PYTHON kleio server"
echo "  home  : ${ABS_HOME}"
echo "  port  : ${PORT}"
echo "  repo  : ${REPO_ROOT}"
echo "  log   : ${LOG_FILE}"
echo "================================================"

# Start uvicorn in the background. We bind to 127.0.0.1 (the harness only ever
# talks to localhost); use --workers 1 because the in-memory job tracker is not
# shared across worker processes (see translations.py _translation_tasks).
# No `cd` here: this script is sourced by run_tests_python.sh, so a cd would
# leak into the caller and break the subsequent translate/stop scripts (which
# expect to run from tests/). PYTHONPATH above makes `kleio` importable.
uvicorn kleio.api.app:app \
  --host 127.0.0.1 \
  --port "${PORT}" \
  --workers 1 \
  --log-level "${LOG_LEVEL}" \
  >> "${LOG_FILE}" 2>&1 &

PYTHON_SERVER_PID=$!
export PYTHON_SERVER_PID
export PYTHON_SERVER_PORT="${PORT}"

# Record PID+port so the stop script works even if this script is run directly.
echo "${PYTHON_SERVER_PID}" > .python_server.pid
echo "${PORT}" > .python_server.port

echo "Python kleio server PID ${PYTHON_SERVER_PID} on port ${PORT}."
