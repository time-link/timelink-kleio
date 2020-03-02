# Run tests by translating all the cli files in tests and comparing the output with reference
# run this from the clio/tests directory ./aux/run_tests.sh
# WARNING: this only works if script run from "tests" directory.
echo "================================================================" 
echo "===                  KLEIO TRANSLATOR TESTS                  ===" 
echo "================================================================" 
export REPORT_FILE="test_report_`date +%Y-%m-%d`.diff"
echo ref sources: $REFERENCE_SOURCES
echo test transl: $TEST_TRANSLATIONS
echo sources dir: $TRANSLATOR_SOURCE  
echo
echo "Runing kleio translator tests. Run this command from clio/tests e.g. ./aux/run_tests.sh"
echo `date` `pwd` > $REPORT_FILE
./aux/prepare_tests.sh >> $REPORT_FILE # this sets up the environment variables.
echo "================================================================" 
echo "Translation of reference sources with stable translator"
time ./aux/kleio_translate_local.sh ${STABLE_CODE_DIR}/clioStart.pl ${STABLE_CODE_DIR}/gacto2.str $REFERENCE_TRANSLATIONS
echo
#echo "Translation of reference sources  with remote dev version of translator"
#./aux/kleio_translate_local.sh dev/swiStart.pl dev/gacto2.str test_translations
echo Translation of reference sources with dev version server mode
./aux/kleio_start_server.sh ${DEV_CODE_DIR}/serverStart.pl  &
sleep 5
time ./aux/kleio_translate_remote.sh $TEST_TRANSLATIONS
sleep 5
time ./aux/kleio_stop_server.sh 
echo
./aux/compare_test_results.sh >> $REPORT_FILE
echo
echo "Tests done. Check $REPORT_FILE"
