#!/usr/bin/env bash
# Semantic equivalence test: PYTHON reimplementation vs STABLE PROLOG reference.
#
# This mirrors run_tests.sh (Prolog-stable-vs-Prolog-dev) but swaps the test leg
# to the Python kleio server. The methodology is deliberately identical so that
# a Python regression is caught the same way a Prolog regression would be:
#
#   reference leg  : stable Prolog translator (tests/stable/) run as a local CLI,
#                    one swipl process per file, writing into reference_translations/
#   test leg      : the Python server (kleio.api.app) driven through its REST
#                    contract, writing into test_translations/
#   compare leg   : diff -r reference_translations/ test_translations/  piped
#                    through the SAME exclude_while_comparing.grep filter.
#
# Reused unchanged from the Prolog harness:
#   - scripts/prepare_tests.sh        (env + corpus seeding)
#   - scripts/kleio_translate_local.sh (stable Prolog reference translation)
#   - scripts/compare_test_results.sh  (filtered recursive diff)
#   - scripts/exclude_while_comparing.grep
#
# New, Python-specific:
#   - scripts/kleio_start_python_server.sh
#   - scripts/kleio_translate_python.sh
#   - scripts/kleio_stop_python_server.sh
#
# Run this from the tests/ directory:
#   ./scripts/run_tests_python.sh
#
# To translate only a subtree, set KLEIO_TEST_SUBDIR, e.g.:
#   KLEIO_TEST_SUBDIR=paroquiais/baptismos ./scripts/run_tests_python.sh
#
# Override the YAML structure used by the Python leg (relative to the structures
# dir, or absolute). The default is sources-structure.yaml, the modular entry
# point that includes the core groups and the Portuguese source schemas. Legacy
# .str files are deprecated and not used by the Python version.
#   KLEIO_DEFAULT_STRU_YAML=sources-structure.yaml ./scripts/run_tests_python.sh
#
# After it finishes, inspect:
#   - tests/reports/test_report_python_<timestamp>.diff   (the filtered diff)
#   - tests/kleio_start_python_server.log                 (server log)
#
# A clean run = an empty diff body after filtering (only "diff -r ..." header
# lines and the start/finish markers).

set -u

# Must be run from the tests/ directory: the helper scripts and the env in
# prepare_tests.sh use tests/-relative paths.
if [ ! -f "scripts/run_tests_python.sh" ]; then
  echo "ERROR: run this from the tests/ directory, e.g. ./scripts/run_tests_python.sh" >&2
  exit 1
fi

echo "================================================================"
echo "===        KLEIO SEMANTIC TESTS: PYTHON vs STABLE PROLOG       ==="
echo "================================================================"
mkdir -p ./reports/
export REPORT_FILE=./reports/"test_report_python_$(date '+%Y-%m-%d_%H:%M:%S').diff"

# prepare_tests.sh sets all KLEIO_* env vars, wipes+reseeds the two translation
# dirs with the corpus, and copies ../src/*.pl into dev/. The Python leg does
# NOT use dev/ (it imports kleio/ from the repo root), but running prepare is
# harmless and keeps the corpus seeding identical to the Prolog harness.
source scripts/prepare_tests.sh >> "$REPORT_FILE"

echo "Reference sources : $REFERENCE_SOURCES"
echo "Reference outputs : $REFERENCE_TRANSLATIONS  (stable Prolog)"
echo "Test outputs      : $TEST_TRANSLATIONS  (Python server)"
echo "(Remote path)     : $TEST_TRANSLATIONS_REMOTE"
echo "Stable translator : ${STABLE_CODE_DIR}/swiStart.pl + ${STABLE_CODE_DIR}/gacto2.str"
echo "Python translator : kleio.api.app (FastAPI/uvicorn)"
echo "Python structure  : ${KLEIO_DEFAULT_STRU_YAML:-sources-structure.yaml}"
echo

echo "$(date) $(pwd)" > "$REPORT_FILE"

# ----------------------------------------------------------------------------
# Reference leg: stable Prolog translator, one swipl per file.
# This is the EXACT call run_tests.sh makes, so the reference output is
# byte-identical to what the Prolog harness would produce.
# ----------------------------------------------------------------------------
echo "================================================================"
echo "Reference translation with STABLE PROLOG translator"
time ./scripts/kleio_translate_local.sh \
  "${STABLE_CODE_DIR}/swiStart.pl" \
  "${STABLE_CODE_DIR}/gacto2.str" \
  "$REFERENCE_TRANSLATIONS"
echo

# ----------------------------------------------------------------------------
# Test leg: Python server, driven through the REST contract.
# ----------------------------------------------------------------------------
echo "Translation of reference sources with PYTHON server"
source scripts/kleio_start_python_server.sh
sleep 3

# Determine the server-relative subtree to translate. By default we translate
# the whole test_translations tree (matching the Prolog recurse=yes behaviour).
PYTHON_SUBDIR="${KLEIO_TEST_SUBDIR:-${TEST_TRANSLATIONS_REMOTE}}"
time source scripts/kleio_translate_python.sh "$PYTHON_SUBDIR"
echo

source scripts/kleio_stop_python_server.sh
echo

# ----------------------------------------------------------------------------
# Compare leg: filtered recursive diff with a per-file summary on top.
#
# Uses compare_test_results_python.sh, which runs the SAME filtered diff as the
# Prolog compare_test_results.sh (same exclude_while_comparing.grep) but prepends
# a per-file breakdown so large diffs stay debuggable. The summary lists each
# differing file with its diff-line count (descending), making it obvious which
# files dominate the diff.
# ----------------------------------------------------------------------------
source scripts/compare_test_results_python.sh >> "$REPORT_FILE"

# Keep stable pointers to the latest report so they're easy to find without
# grepping for a timestamp:
#   reports/latest_python.diff        -> summary report (small, committable)
#   reports/latest_python.fulldiff    -> full filtered diff (large, local)
ln -sf "$(basename "$REPORT_FILE")" reports/latest_python.diff
if [ -f "${REPORT_FILE}.fulldiff" ]; then
  ln -sf "$(basename "${REPORT_FILE}.fulldiff")" reports/latest_python.fulldiff
fi

echo
echo "Tests done."
echo "  Summary report : $REPORT_FILE"
echo "                   (symlink: tests/reports/latest_python.diff)"
if [ -f "${REPORT_FILE}.fulldiff" ]; then
  echo "  Full diff      : ${REPORT_FILE}.fulldiff"
  echo "                   (symlink: tests/reports/latest_python.fulldiff)"
fi
echo
echo "Top of the summary:"
sed -n '1,/The full filtered diff is in/p' "$REPORT_FILE"
