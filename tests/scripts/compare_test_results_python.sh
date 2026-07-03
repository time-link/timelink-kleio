#!/usr/bin/env bash
# Compare translation results: Python server vs stable Prolog reference.
#
# This is the Python-specific counterpart of compare_test_results.sh. It runs
# the SAME filtered recursive diff (so the methodology and the
# exclude_while_comparing.grep filter are identical), but prepends a per-file
# summary so that large diffs (the Python reimplementation currently produces
# multi-million-line diffs) stay debuggable.
#
# The summary lists, for every file that differs:
#     <diff-line-count> <reference-relative-path>
# sorted by descending diff-line-count, so the files responsible for most of
# the diff noise are at the top. It also reports counts of:
#   - files only in reference (Python failed to translate / produced no output)
#   - files only in test (Python produced extra artifacts)
#   - files that differ
#
# Output goes to stdout; run_tests_python.sh redirects it into the report file.
# Run from the tests/ directory; needs REFERENCE_TRANSLATIONS and
# TEST_TRANSLATIONS in the environment (set by prepare_tests.sh).

set -u

echo "Comparing translation results (Python vs stable Prolog)."
echo "$(date) $(pwd)"
echo

# ----------------------------------------------------------------------------
# Produce the raw filtered diff once, into a temp file, so we can summarize it
# and emit it verbatim. The full diff can be very large (multi-MB) while the
# Python reimplementation diverges from Prolog, so we also persist it to a
# separate file next to the report for offline debugging.
# ----------------------------------------------------------------------------
RAW_DIFF_TMP="$(mktemp -t kleiopydiff)"
trap 'rm -f "${RAW_DIFF_TMP}"' EXIT

diff -r -b "${REFERENCE_TRANSLATIONS}/" "${TEST_TRANSLATIONS}/" \
  | grep -v -f scripts/exclude_while_comparing.grep > "${RAW_DIFF_TMP}"

# If REPORT_FILE is set (run_tests_python.sh sets it), mirror the full filtered
# diff into <report>.fulldiff so the summary report stays small and committable
# while the verbose diff remains available locally for debugging.
if [ -n "${REPORT_FILE:-}" ]; then
  cp "${RAW_DIFF_TMP}" "${REPORT_FILE}.fulldiff"
fi

# ----------------------------------------------------------------------------
# Summary: per-file diff-line counts, plus only-in/differ counts.
#
# Each file's diff block starts with a line beginning "diff -r" or "Only in".
# Count the body lines following each such header.
# ----------------------------------------------------------------------------
echo "==================== SUMMARY ===================="
echo

# "Only in reference" => Python produced no output for that source (failed).
ONLY_REF=$(grep -c "^Only in ${REFERENCE_TRANSLATIONS}" "${RAW_DIFF_TMP}" || true)
ONLY_TEST=$(grep -c "^Only in ${TEST_TRANSLATIONS}" "${RAW_DIFF_TMP}" || true)
N_DIFFER=$(grep -c "^diff -r" "${RAW_DIFF_TMP}" || true)
echo "Files only in reference (Python produced no output): ${ONLY_REF}"
echo "Files only in test (Python produced extra output)  : ${ONLY_TEST}"
echo "Files that differ                                  : ${N_DIFFER}"
echo

echo "Per-file differing-line counts (descending):"
echo "  (diff-line-count  file)"
# Walk the raw diff, grouping by header. awk is portable (bash 3.2 / zsh).
# A diff header looks like:  diff -r -b <refpath> <testpath>
# so the reference path is the LAST field but one ($NF-1). Using NF keeps it
# correct regardless of how many flags diff emits.
awk '
/^diff -r / {
    if (hdr != "") printf "%8d  %s\n", n, file;
    hdr = $0;
    # Reference path is the second-to-last field on the diff header line.
    file = $(NF-1);
    sub(/^.*\/reference_translations\//, "", file);
    n = 0;
    next
}
/^Only in / {
    if (hdr != "") { printf "%8d  %s\n", n, file; hdr = "" }
    next
}
{ n++ }
END {
    if (hdr != "") printf "%8d  %s\n", n, file;
}
' "${RAW_DIFF_TMP}" | sort -rn
echo
if [ -n "${REPORT_FILE:-}" ] && [ -f "${REPORT_FILE}.fulldiff" ]; then
  echo "The full filtered diff is in: ${REPORT_FILE}.fulldiff"
else
  echo "==================== FULL FILTERED DIFF ===================="
  echo
  cat "${RAW_DIFF_TMP}"
fi
echo
echo "Comparing translation results finished."
