#!/usr/bin/env bash
# Semantic equivalence test: PYTHON reimplementation vs STABLE PROLOG reference,
# using STRUCTURAL comparison (not textual diff).
#
# This is the structural counterpart of run_tests_python.sh. Where that script
# does a textual diff (noisy because the Python XML serialization differs from
# Prolog's in many cosmetic ways), this script parses both sides into a common
# group-tree and compares the semantically meaningful fields: group names, ids,
# parent/child nesting, and core element values.
#
# Both harnesses share the SAME setup:
#   - prepare_tests.sh        seeds reference_translations/ and test_translations/
#                             with the corpus from reference_sources/
#   - kleio_translate_local.sh runs the stable Prolog translator on reference_translations/
#   - kleio_translate_python.sh runs the Python server on test_translations/
#
# The difference is only the compare leg:
#   - run_tests_python.sh        uses compare_test_results_python.sh (textual diff)
#   - run_tests_python_semantic.sh uses compare_semantic.py           (structural)
#
# The real .xml/.err/.rpt/.files.json outputs are written next to each .cli in
# test_translations/ for human inspection, exactly as with the textual harness.
#
# Run this from the tests/ directory:
#   ./scripts/run_tests_python_semantic.sh
#
# To translate only a subtree:
#   KLEIO_TEST_SUBDIR=paroquiais/baptismos ./scripts/run_tests_python_semantic.sh
#
# To recompare without retranslating (skips the translate legs):
#   KLEIO_SEMANTIC_SKIP_TRANSLATE=1 ./scripts/run_tests_python_semantic.sh

set -u

if [ ! -f "scripts/run_tests_python_semantic.sh" ]; then
  echo "ERROR: run this from the tests/ directory, e.g. ./scripts/run_tests_python_semantic.sh" >&2
  exit 1
fi

echo "================================================================"
echo "===   KLEIO SEMANTIC TESTS (STRUCTURAL): PYTHON vs PROLOG    ==="
echo "================================================================"

# ---------------------------------------------------------------------------
# Step 1: prepare the corpus (shared with the textual harness).
# ---------------------------------------------------------------------------
# Skips the corpus reseed/translation when the caller asked to recompare only.
if [ -z "${KLEIO_SEMANTIC_SKIP_TRANSLATE:-}" ]; then
  mkdir -p ./reports/
  # prepare_tests.sh sets all KLEIO_* env vars, wipes+reseeds the two
  # translation dirs with the corpus, and copies ../src/*.pl into dev/.
  source scripts/prepare_tests.sh

  echo "Reference sources : $REFERENCE_SOURCES"
  echo "Reference outputs : $REFERENCE_TRANSLATIONS  (stable Prolog)"
  echo "Test outputs      : $TEST_TRANSLATIONS  (Python server)"
  echo

  # ---------------------------------------------------------------------------
  # Step 2: reference leg (stable Prolog).
  # ---------------------------------------------------------------------------
  echo "================================================================"
  echo "Reference translation with STABLE PROLOG translator"
  time ./scripts/kleio_translate_local.sh \
    "${STABLE_CODE_DIR}/swiStart.pl" \
    "${STABLE_CODE_DIR}/gacto2.str" \
    "$REFERENCE_TRANSLATIONS"
  echo

  # ---------------------------------------------------------------------------
  # Step 3: test leg (Python server). The Python translator writes the real
  # .xml/.err/.rpt/.files.json/.ids files next to each .cli in test_translations/.
  # ---------------------------------------------------------------------------
  echo "Translation of reference sources with PYTHON server"
  source scripts/kleio_start_python_server.sh
  sleep 3

  PYTHON_SUBDIR="${KLEIO_TEST_SUBDIR:-${TEST_TRANSLATIONS_REMOTE}}"
  time source scripts/kleio_translate_python.sh "$PYTHON_SUBDIR"
  echo

  source scripts/kleio_stop_python_server.sh
  echo
else
  echo "(skipping translate legs: KLEIO_SEMANTIC_SKIP_TRANSLATE set)"
  # Still need the env vars that prepare_tests.sh sets.
  source scripts/prepare_tests.sh >/dev/null 2>&1 || true
fi

# ---------------------------------------------------------------------------
# Step 4: structural comparison.
# ---------------------------------------------------------------------------
echo "================================================================"
echo "Structural comparison (Python XML vs Prolog reference XML)"
echo "================================================================"
python3 scripts/compare_semantic.py \
  ${KLEIO_TEST_SUBDIR:+--subdir "$KLEIO_TEST_SUBDIR"} \
  --reference-translations "$REFERENCE_TRANSLATIONS" \
  --test-translations "$TEST_TRANSLATIONS"
RC=$?

echo
echo "Semantic tests done (structural).  exit code: $RC"
echo "  Latest summary symlink: tests/reports/latest_semantic.summary.txt"
exit $RC
