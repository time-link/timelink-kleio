# Run tests by translating all the cli files in tests and comparing the output with reference
# run this from the clio/tests directory ./scripts/run_tests.sh
# requires swipl in the PATH download from https://www.swi-prolog.org
#
# WARNING: this only works if script run from "tests" directory.

echo "================================================================" 
echo "===                  KLEIO TRANSLATOR TESTS                  ===" 
echo "================================================================" 
mkdir -p ./reports/
export REPORT_FILE=./reports/"test_report_`date \"+%Y-%m-%d_%H:%M:%S\"`.diff"
source scripts/prepare_tests.sh >> $REPORT_FILE # this sets up the environment variables.
echo "Reference sources: $REFERENCE_SOURCES"
echo "Test Translations: $TEST_TRANSLATIONS"
echo "(Remote path)    : $TEST_TRANSLATIONS_REMOTE"
echo "Translator source: $TRANSLATOR_SOURCE"
echo
echo "Runing kleio translator tests. Run this command from clio/tests e.g. ./scripts/run_tests.sh"
echo `date` `pwd` > $REPORT_FILE

echo "================================================================" 
echo "Translation of reference sources with stable translator"
time ./scripts/kleio_translate_local.sh ${STABLE_CODE_DIR}/clioStart.pl ${STABLE_CODE_DIR}/gacto2.str $REFERENCE_TRANSLATIONS
echo
#echo "Translation of reference sources  with remote dev version of translator"
#./aux/kleio_translate_local.sh dev/swiStart.pl dev/gacto2.str test_translations
echo Translation of reference sources with dev version server mode
source ./scripts/kleio_start_server.sh ${DEV_CODE_DIR}/serverStart.pl  &
sleep 5
time source ./scripts/kleio_translate_remote.sh $TEST_TRANSLATIONS_REMOTE
sleep 5
time source ./scripts/kleio_stop_server.sh 
echo
source ./scripts/compare_test_results.sh >> $REPORT_FILE
echo
echo "Tests done. Check $REPORT_FILE"
