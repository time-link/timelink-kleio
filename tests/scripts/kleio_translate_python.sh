#!/usr/bin/env bash
# Translate Kleio source files using the PYTHON reimplementation server.
#
# This is the Python counterpart of kleio_translate_remote.sh. It drives the
# candidate Python server through the SAME REST contract that the Prolog dev
# server exposes, so that the output of both translators can be compared with a
# single recursive diff (see compare_test_results.sh / run_tests_python.sh).
#
# Methodology parity with the Prolog harness:
#   - The Python server is the CANDIDATE; the stable Prolog translator
#     (tests/stable/) remains the reference, run by kleio_translate_local.sh.
#   - Translation output files are written next to each source file, exactly as
#     the Prolog server does, so diff -r compares them apples-to-apples.
#
# Differences from the Prolog REST contract (single recursive POST) :
#   The Python `POST /rest/translations` endpoint translates ONE file per call
#   (no recurse=yes mode). This script therefore:
#     1. waits for the server to be reachable,
#     2. for each .cli/.kleio file under the given dir, POSTs a translation job
#        (passing an explicit YAML structure file, since the Python loader reads
#        YAML, not .str), then polls GET /rest/translations/{path} until the job
#        reports completed/error, or a timeout is hit,
#     3. reports per-file status and a summary.
#
# Usage:
#   kleio_translate_python.sh KLEIO_SOURCES_DIR [STRUCTURE_RELATIVE_TO_STRU_DIR]
#
#   KLEIO_SOURCES_DIR  : path relative to the server's sources dir, e.g.
#                        "sources/test_translations" or "" for all sources.
#                        (NOTE: this matches the arg semantics of
#                        kleio_translate_remote.sh, where the value of
#                        $TEST_TRANSLATIONS_REMOTE is passed.)
#   STRUCTURE           : optional path (relative to structures dir, OR an
#                        absolute path) of the YAML structure file. The Python
#                        version uses YAML structures only (legacy .str files
#                        are deprecated). Defaults to
#                        $KLEIO_DEFAULT_STRU_YAML, then "sources-structure.yaml"
#                        (the modular entry point that includes the core groups
#                        and the Portuguese source schemas).
#
# Environment (expected to be set by prepare_tests.sh):
#   KLEIO_ADMIN_TOKEN     admin bearer token for the server
#   KLEIO_SERVER_PORT     port the python server listens on (default 8088)
#   KLEIO_HOME_DIR        server home dir (sources live under .../sources)
#   KLEIO_DEFAULT_STRU_YAML  optional override for the default structure file
#
# This script is run from the tests/ directory.

set -u

KLEIO_SOURCES_DIR="${1:-}"
# The structure argument is interpreted relative to the structures dir unless
# absolute; an empty value means "use the default YAML structure".
# sources-structure.yaml is the modular entry point (includes elements.yaml,
# groups.yaml and the Portuguese pt-sources-structure.yaml). Legacy .str files
# are deprecated and not loadable by the Python schema loader.
STRUCTURE_ARG="${2:-${KLEIO_DEFAULT_STRU_YAML:-sources-structure.yaml}}"

PORT="${KLEIO_SERVER_PORT:-8088}"
TOKEN="${KLEIO_ADMIN_TOKEN:-admintoken}"
HOME_DIR="${KLEIO_HOME_DIR:-kleio-home}"
BASE_URL="http://localhost:${PORT}"

echo
echo "================================================"
echo "Python-translating kleio sources under: ${KLEIO_SOURCES_DIR:-<root>}"
echo "Server            : ${BASE_URL}"
echo "Structure         : ${STRUCTURE_ARG}"
echo "KLEIO_ADMIN_TOKEN : ${TOKEN}"
echo "================================================"

# ----------------------------------------------------------------------------
# Resolve which files to translate.
#
# $KLEIO_SOURCES_DIR is server-relative (e.g. "sources/test_translations" or
# ""). We translate that to a host filesystem path under KLEIO_HOME_DIR so we
# can enumerate the .cli/.kleio files locally (the Python endpoint is
# single-file, so we must drive it once per file).
# ----------------------------------------------------------------------------
if [ -n "${KLEIO_SOURCES_DIR}" ]; then
  HOST_SCAN_ROOT="${HOME_DIR}/${KLEIO_SOURCES_DIR}"
else
  HOST_SCAN_ROOT="${HOME_DIR}/sources"
fi

if [ ! -d "${HOST_SCAN_ROOT}" ]; then
  echo "ERROR: scan root does not exist: ${HOST_SCAN_ROOT}" >&2
  exit 1
fi

# Count source files first (separate find, so TOTAL is known before the loop).
# NOTE: this script must run on macOS bash 3.2 (no mapfile/readarray) and zsh,
# so we avoid bash arrays entirely and use plain POSIX constructs.
TOTAL=$(find "${HOST_SCAN_ROOT}" -type f \
  \( -name '*.cli' -o -name '*.kleio' -o -name '*.CLI' -o -name '*.KLEIO' \) \
  | wc -l | tr -d ' ')
if [ "${TOTAL}" -eq 0 ] || [ -z "${TOTAL}" ]; then
  echo "WARNING: no .cli/.kleio files found under ${HOST_SCAN_ROOT}"
  exit 0
fi
echo "Found ${TOTAL} source file(s) to translate."
echo

# ----------------------------------------------------------------------------
# Wait for the server to accept connections.
# ----------------------------------------------------------------------------
echo -n "Waiting for server at ${BASE_URL} ..."
for _ in $(seq 1 60); do
  if curl -sf -o /dev/null "${BASE_URL}/health"; then
    echo " up."
    break
  fi
  echo -n "."
  sleep 1
done
if ! curl -sf -o /dev/null "${BASE_URL}/health"; then
  echo " ERROR: server not reachable at ${BASE_URL}/health" >&2
  exit 1
fi

# ----------------------------------------------------------------------------
# Per-file: POST translation, then poll GET status until terminal.
#
# Portability: macOS ships bash 3.2 (no mapfile/readarray) and the script may
# also be sourced by zsh. We therefore:
#   - avoid bash arrays entirely,
#   - read the file list via FD 3 (a `while read < 3` loop does NOT run in a
#     subshell on bash 3.2, so the OK/FAILED counters stay live),
#   - accumulate the failure list in a temp file.
# ----------------------------------------------------------------------------

# Max seconds to wait for a single file's translation to complete.
PER_FILE_TIMEOUT="${KLEIO_PER_FILE_TIMEOUT:-120}"
# How long to sleep between status polls.
POLL_INTERVAL="${KLEIO_POLL_INTERVAL:-0.5}"

OK=0
FAILED=0
ERRORS_TMP="$(mktemp -t kleiopyerrs)"
trap 'rm -f "${ERRORS_TMP}"' EXIT

# Build the sorted file list into a temp file (paths may contain spaces, so we
# use one-per-line and read with IFS=).
LIST_TMP="$(mktemp -t kleiopylist)"
trap 'rm -f "${ERRORS_TMP}" "${LIST_TMP}"' EXIT
find "${HOST_SCAN_ROOT}" -type f \
  \( -name '*.cli' -o -name '*.kleio' -o -name '*.CLI' -o -name '*.KLEIO' \) \
  | sort > "${LIST_TMP}"

# Helper: percent-encode a path for use in a URL path segment (safe chars only).
url_encode() {
  python3 -c "import urllib.parse,sys; print(urllib.parse.quote(sys.argv[1], safe=''))" "$1"
}

# Iterate the list via FD 3 so the loop body shares this shell's variables
# (a pipe-fed while loop would run in a subshell and lose OK/FAILED).
while IFS= read -r host_path <&3; do
  [ -n "${host_path}" ] || continue
  # Server-relative source path (what the REST API expects in {path}).
  # host_path looks like .../kleio-home/sources/test_translations/varia/x.cli
  rel_to_home="${host_path#${HOME_DIR}/}"
  # Strip a leading "sources/" if present, since the API resolves under sources.
  api_path="${rel_to_home#sources/}"

  echo -n "  translating ${api_path} ... "

  # POST the translation job. The Python contract takes a JSON body.
  post_resp=$(curl -s -X POST "${BASE_URL}/rest/translations" \
    -H "Authorization: Bearer ${TOKEN}" \
    -H "Content-Type: application/json" \
    --data "{\"path\": \"${api_path}\", \"structure\": \"${STRUCTURE_ARG}\", \"echo\": \"no\"}")

  job_id=$(printf '%s' "${post_resp}" | python3 -c 'import json,sys
try:
    d=json.load(sys.stdin)
    print(d.get("job",{}).get("job_id","") or d.get("job_id",""))
except Exception:
    print("")' 2>/dev/null)

  if [ -z "${job_id}" ]; then
    echo "FAILED (no job_id from server)"
    echo "    response: ${post_resp}"
    FAILED=$((FAILED + 1))
    printf '%s: no job_id (%s)\n' "${api_path}" "${post_resp}" >> "${ERRORS_TMP}"
    continue
  fi

  # Poll GET /rest/translations/{path} until xml_exists or terminal job status.
  # NOTE: variable is named tr_status, not status, because zsh makes `status`
  # a read-only builtin; this script must be safe to source from zsh and bash.
  elapsed=0
  tr_status="queued"
  get_resp=""
  while [ "${elapsed}" -lt "${PER_FILE_TIMEOUT}" ]; do
    sleep "${POLL_INTERVAL}"
    elapsed=$(python3 -c "print(${elapsed} + ${POLL_INTERVAL})")

    get_resp=$(curl -s -X GET "${BASE_URL}/rest/translations/$(url_encode "${api_path}")" \
      -H "Authorization: Bearer ${TOKEN}")

    # Pull xml_exists, job status and message out of the JSON in one python
    # invocation (cheaper than three), printing "status<TAB>xml<TAB>msg".
    parsed=$(printf '%s' "${get_resp}" | python3 -c 'import json,sys
try:
    d=json.load(sys.stdin)
    job=(d.get("job") or {})
    print("\t".join([str(job.get("status","")), str(d.get("xml_exists",False)), str(job.get("message",""))]))
except Exception:
    print("\t".join(["","",""]))' 2>/dev/null)
    job_status=$(printf '%s' "${parsed}" | cut -f1)
    xml_exists=$(printf '%s' "${parsed}" | cut -f2)
    job_msg=$(printf '%s' "${parsed}" | cut -f3)

    if [ "${xml_exists}" = "True" ] || [ "${job_status}" = "completed" ]; then
      tr_status="completed"; break
    fi
    if [ "${job_status}" = "error" ]; then
      tr_status="error"; break
    fi
  done

  if [ "${tr_status}" = "completed" ]; then
    echo "OK"
    OK=$((OK + 1))
  else
    echo "${tr_status} (timeout=${PER_FILE_TIMEOUT}s, job=${job_id})"
    FAILED=$((FAILED + 1))
    # Re-extract the message for the failure log if we broke on a timeout poll.
    msg="${job_msg:-}"
    printf '%s: %s %s\n' "${api_path}" "${tr_status}" "${msg}" >> "${ERRORS_TMP}"
  fi
done 3< "${LIST_TMP}"

echo
echo "================================================"
echo "Python translation summary: ${OK}/${TOTAL} ok, ${FAILED} failed."
if [ -s "${ERRORS_TMP}" ]; then
  echo "Failures:"
  while IFS= read -r line; do echo "  - ${line}"; done < "${ERRORS_TMP}"
fi
echo "================================================"
