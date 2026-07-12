# Re-run tests without re-translating with stable translator
# This script skips the stable translation and only re-runs the dev translation
# run this from the clio/tests directory ./scripts/redo_run_tests.sh
#  or use the `make redo-test-semantics`
# requires swipl in the PATH download from https://www.swi-prolog.org
#
# WARNING: this only works if script run from "tests" directory.
source scripts/env_tests.sh
echo "================================================================"
echo "===              KLEIO TRANSLATOR TESTS (REDO)               ==="
echo "=================================/==============================="
mkdir -p ./reports/
export REPORT_FILE=./reports/"test_report_`date \"+%Y-%m-%d_%H:%M:%S\"`.diff"
echo "Reference sources: $REFERENCE_SOURCES"
echo "Test Translations: $TEST_TRANSLATIONS"
echo "(Remote path)    : $TEST_TRANSLATIONS_REMOTE"
echo "Translator source: $TRANSLATOR_SOURCE"
echo "Stable translator: ${STABLE_CODE_DIR} "
echo
echo "Re-running kleio translator tests (dev only). Run this command from clio/tests e.g. ./scripts/redo_run_tests.sh"
echo "Note: This assumes stable translations already exist in $REFERENCE_TRANSLATIONS"
echo `date` `pwd` > $REPORT_FILE

# Clean dev code directory and copy current source
echo "Cleaning dev code directory and copying current source..."
rm -rf $DEV_CODE_DIR/*
cp -R ${TRANSLATOR_SOURCE}/* $DEV_CODE_DIR/

# Clean only test_translations directory (not reference_translations)
echo "Cleaning test translations directory..."
rm -rf $TEST_TRANSLATIONS/*
cp -R $REFERENCE_SOURCES/ $TEST_TRANSLATIONS

echo "================================================================"
echo "Translation of reference sources with dev version server mode"
source ./scripts/kleio_start_server.sh ${DEV_CODE_DIR}/serverStart.pl  &
sleep 5
time source ./scripts/kleio_translate_remote.sh $TEST_TRANSLATIONS_REMOTE
sleep 5
time source ./scripts/kleio_stop_server.sh
echo
source ./scripts/compare_test_results.sh >> $REPORT_FILE
echo
echo "Tests done. Check $REPORT_FILE"
