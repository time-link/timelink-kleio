#!/usr/bin/env bash
# Compare translation results: Python server vs stable Prolog reference.
#
# This is the Python-specific counterpart of compare_test_results.sh. It runs
# the SAME filtered diff (same exclude_while_comparing.grep), but:
#   - diffs file-by-file so one oversized/bloated file cannot hang the whole
#     compare (the Python exporter currently emits ~50x too many GROUPs for
#     some files, producing multi-GB diffs),
#   - prepends a per-file summary so large diffs stay debuggable.
#
# The summary lists, for every file that differs:
#     <diff-line-count> <reference-relative-path>
# sorted by descending diff-line-count. Oversized files (either side larger
# than MAX_FILE_KB) are reported as BLOATED (skipped) rather than diffed.
#
# Output goes to stdout; run_tests_python.sh redirects it into the report file.
# Run from the tests/ directory; needs REFERENCE_TRANSLATIONS and
# TEST_TRANSLATIONS in the environment (set by prepare_tests.sh).

set -u

# Skip diffing a file pair when either side exceeds this size. Prevents the
# compare from running for hours on exporter-bloated XML. Override via env.
MAX_FILE_KB="${KLEIO_COMPARE_MAX_FILE_KB:-20480}"   # 20 MB

echo "Comparing translation results (Python vs stable Prolog)."
echo "$(date) $(pwd)"
echo

REF="${REFERENCE_TRANSLATIONS}"
TST="${TEST_TRANSLATIONS}"
EXCLUDE=scripts/exclude_while_comparing.grep

# Temp file holding the full filtered diff across all comparable files.
RAW_DIFF_TMP="$(mktemp -t kleiopydiff)"
# Temp file listing per-file results for the summary.
SUMMARY_TMP="$(mktemp -t kleiopysum)"
trap 'rm -f "${RAW_DIFF_TMP}" "${SUMMARY_TMP}"' EXIT

ONLY_REF=0
ONLY_TEST=0
N_DIFFER=0
N_BLOATED=0
N_IDENTICAL=0

# Walk every file present in either tree, computing relative paths once.
# Use a temp file so the while-read loop stays in this shell (bash 3.2 has no
# mapfile; a pipe would subshell and lose our counters).
ALL_TMP="$(mktemp -t kleioall)"
trap 'rm -f "${RAW_DIFF_TMP}" "${SUMMARY_TMP}" "${ALL_TMP}"' EXIT
( cd "${REF}" && find . -type f | sort ) > "${ALL_TMP}.ref"
( cd "${TST}" && find . -type f | sort ) > "${ALL_TMP}.tst"
# Union of relative paths, sorted, unique.
cat "${ALL_TMP}.ref" "${ALL_TMP}.tst" | sort -u > "${ALL_TMP}"
rm -f "${ALL_TMP}.ref" "${ALL_TMP}.tst"

while IFS= read -r rel; do
  rel="${rel#./}"
  r="${REF}/${rel}"
  t="${TST}/${rel}"
  if [ -f "$r" ] && [ -f "$t" ]; then
    # Both exist: compare, unless either is oversized.
    rkb=$(( $(stat -f%z "$r" 2>/dev/null || stat -c%s "$r" 2>/dev/null || echo 0) / 1024 ))
    tkb=$(( $(stat -f%z "$t" 2>/dev/null || stat -c%s "$t" 2>/dev/null || echo 0) / 1024 ))
    if [ "$rkb" -gt "$MAX_FILE_KB" ] || [ "$tkb" -gt "$MAX_FILE_KB" ]; then
      N_BLOATED=$((N_BLOATED + 1))
      printf 'BLOATED  %8dKB/%dKB  %s\n' "$rkb" "$tkb" "$rel" >> "${SUMMARY_TMP}"
      continue
    fi
    # Filtered diff for this one file.
    fdiff=$(diff -b "$r" "$t" | grep -v -f "${EXCLUDE}")
    if [ -z "$fdiff" ]; then
      N_IDENTICAL=$((N_IDENTICAL + 1))
    else
      N_DIFFER=$((N_DIFFER + 1))
      nlines=$(printf '%s\n' "$fdiff" | grep -cve '^\s*$' || true)
      printf '%8d  %s\n' "$nlines" "$rel" >> "${SUMMARY_TMP}"
      {
        echo "diff -b ${REF}/${rel} ${TST}/${rel}"
        printf '%s\n' "$fdiff"
        echo
      } >> "${RAW_DIFF_TMP}"
    fi
  elif [ -f "$r" ]; then
    ONLY_REF=$((ONLY_REF + 1))
    printf 'ONLY-REF  %s\n' "$rel" >> "${SUMMARY_TMP}"
  else
    ONLY_TEST=$((ONLY_TEST + 1))
    printf 'ONLY-TEST  %s\n' "$rel" >> "${SUMMARY_TMP}"
  fi
done < "${ALL_TMP}"

# Persist the full filtered diff for offline debugging.
if [ -n "${REPORT_FILE:-}" ]; then
  cp "${RAW_DIFF_TMP}" "${REPORT_FILE}.fulldiff"
fi

# ----------------------------------------------------------------------------
# Summary.
# ----------------------------------------------------------------------------
echo "==================== SUMMARY ===================="
echo
echo "Files identical (filtered)                         : ${N_IDENTICAL}"
echo "Files that differ                                  : ${N_DIFFER}"
echo "Files only in reference (Python produced no output): ${ONLY_REF}"
echo "Files only in test (Python produced extra output)  : ${ONLY_TEST}"
echo "Files skipped (either side > ${MAX_FILE_KB}KB)        : ${N_BLOATED}"
echo

echo "Per-file breakdown (descending by diff-line count; BLOATED/ONLY-* listed after):"
echo "  (diff-line-count  file)"
sort -rn "${SUMMARY_TMP}"
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
