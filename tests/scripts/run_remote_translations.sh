# Run test_translations files with remote translator.
# run this from the clio/tests directory ./scripts/run_remote_translations.sh
echo "================================================================" 
export KLEIO_HOME=kleio-home 
export KLEIO_HOME_DIR=kleio-home
export KLEIO_SOURCE_DIR=${KLEIO_HOME_DIR}/sources
export KLEIO_CONF_DIR=${KLEIO_HOME_DIR}/system/conf/kleio
export KLEIO_STRU_DIR=${KLEIO_HOME_DIR}/system/conf/kleio/stru
export KLEIO_TOKEN_DB=${KLEIO_CONF_DIR}/token_db
export KLEIO_DEFAULT_STRU=${KLEIO_CONF_DIR}/stru/gacto2.str
export KLEIO_DEBUGGER_PORT=4000
export KLEIO_SERVER_PORT=8088
export KLEIO_WORKERS=5
export KLEIO_IDLE_TIMEOUT=360
export KLEIO_DEBUG=true
export REFERENCE_SOURCES=${KLEIO_HOME}/sources/reference_sources
export REFERENCE_TRANSLATIONS=${KLEIO_HOME}/sources/reference_translations
export TEST_TRANSLATIONS=${KLEIO_HOME}/sources/test_translations
export TEST_TRANSLATIONS_REMOTE=sources/test_translations
export STABLE_CODE_DIR=stable
export DEV_CODE_DIR=dev
export TRANSLATOR_SOURCE=../src/
export REPORT_FILE=./reports/"test_report_`date \"+%Y-%m-%d_%H:%M:%S\"`.diff"
export KLEIO_ADMIN_TOKEN='1232412341234'
echo ref sources: $REFERENCE_SOURCES
echo test transl: $TEST_TRANSLATIONS
echo
echo "Runing kleio translator tests. Run this command from clio/tests e.g. ./scripts/run_tests.sh"
# 1. clean contents of test directories
rm -r $TEST_TRANSLATIONS
# 2. copy current src and reference sources 
cp ${TRANSLATOR_SOURCE}/*.pl $DEV_CODE_DIR/
cp -f ${TRANSLATOR_SOURCE}/*.str $KLEIO_STRU_DIR/
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
