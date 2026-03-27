# Run test_translations files with remote translator.
# run this from the clio/tests directory ./scripts/run_remote_translations.sh
echo "================================================================" 
source ./scripts/env_tests.sh

export REPORT_FILE=./reports/"test_report_`date \"+%Y-%m-%d_%H:%M:%S\"`.diff"
echo ref sources: $REFERENCE_SOURCES
echo test transl: $TEST_TRANSLATIONS
echo
echo "Runing kleio translator tests. Run this command from clio/tests e.g. ./scripts/run_tests.sh"
# 1. clean contents of test directories
rm -r $TEST_TRANSLATIONS
# 2. copy current src and reference sources 
cp -Rf ${TRANSLATOR_SOURCE}/ $DEV_CODE_DIR/
cp -R $REFERENCE_SOURCES/ $TEST_TRANSLATIONS

# 3. List reference sources
echo
echo Translators tests setup with following reference sources
ls -lR $REFERENCE_SOURCES/
echo "================================================================" 
echo Translation of reference sources with dev version server mode
source ./scripts/kleio_start_server.sh $DEV_CODE_DIR/serverStart.pl  &
sleep 5
time source ./scripts/kleio_translate_remote.sh $TEST_TRANSLATIONS_REMOTE
sleep 5
time source ./scripts/kleio_stop_server.sh 
echo
./scripts/compare_test_results.sh >> $REPORT_FILE
echo
echo "Tests done. Check $REPORT_FILE"
