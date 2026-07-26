# Compares the output of two directories with traslation results
# comparing is based on standard diff but "natural occuring" differences
#   are filtered.
echo "Comparing translation results."
echo `date` `pwd`

# Build a patterns file with comments and blank lines removed.
#
# IMPORTANT: grep -f parses EVERY non-empty line of the pattern file as a
# regex, including lines that look like comments. So a comment containing a
# regex metacharacter (e.g. "(e.g." or "(?!...)") would break the ERE
# compiler and make grep abort the entire run - silently dumping the raw,
# unfiltered diff into the report. Stripping comments here keeps comment
# text safe and makes the filter work identically on BSD and GNU grep.
#
# This uses a temp file (not bash process substitution) so it runs under
# /bin/sh (dash) on Linux as well as bash on macOS.
PATFILE=$(mktemp -t kleio_exclude)
grep -v '^[[:space:]]*#' scripts/exclude_while_comparing.grep \
  | grep -v '^[[:space:]]*$' > "$PATFILE"

diff -r -b "$REFERENCE_TRANSLATIONS"/ "$TEST_TRANSLATIONS"/ \
  | grep -v -E -f "$PATFILE"
status=$?

rm -f "$PATFILE"

# grep exits 0 on match, 1 on no-match (normal: all diff lines filtered),
# or 2+ on error (e.g. a malformed regex). 2+ means the filter silently
# failed and the report above is the RAW diff - surface it loudly so it can
# never be mistaken for a clean run.
if [ "$status" -ge 2 ]; then
  echo "*** COMPARISON FILTER FAILED (grep exit $status) - report above is UNFILTERED ***"
fi

echo "Comparing translation results finished."
