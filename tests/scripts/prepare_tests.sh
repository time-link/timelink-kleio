# prepare tests
# 1. cleans contents of test directories
# 2. copies clio/src to clio/tests/dev folder
# 3. copies .cli, .kleio .CLI .KLEIO from reference_sources 
#      to reference_translations and test_translations
# 4. cleans contents of test_translations directory
# for more info see clio/tests/README.md
# run this from clio/tests directory
export KLEIO_HOME=kleio-home 
export KLEIO_HOME_DIR=kleio-home
export KLEIO_SOURCE_DIR=${KLEIO_HOME_DIR}/sources
export KLEIO_CONF_DIR=${KLEIO_HOME_DIR}/system/conf/kleio
export KLEIO_STRU_DIR=${KLEIO_HOME_DIR}/system/conf/kleio/stru
export KLEIO_TOKEN_DB=${KLEIO_CONF_DIR}/token_db
export KLEIO_DEFAULT_STRU=${KLEIO_CONF_DIR}/stru/gacto2.str
# export KLEIO_DEBUGGER_PORT=4000
export KLEIO_SERVER_PORT=8088
export KLEIO_WORKERS=5
export KLEIO_IDLE_TIMEOUT=360
export KLEIO_ADMIN_TOKEN=admintoken
export REFERENCE_SOURCES=${KLEIO_HOME}/sources/reference_sources
export REFERENCE_TRANSLATIONS=${KLEIO_HOME}/sources/reference_translations
export TEST_TRANSLATIONS=${KLEIO_HOME}/sources/test_translations
export TEST_TRANSLATIONS_REMOTE=sources/test_translations
export STABLE_CODE_DIR=stable
export DEV_CODE_DIR=dev
export TRANSLATOR_SOURCE=../src/
# 1. clean contents of test directories
rm -rf $REFERENCE_TRANSLATIONS/*
rm -rf $TEST_TRANSLATIONS/*
rm -rf $DEV_CODE_DIR/*
# 2. copy current src and reference sources 
cp ${TRANSLATOR_SOURCE}/*.pl $DEV_CODE_DIR/
cp ${TRANSLATOR_SOURCE}/*.str $KLEIO_STRU_DIR/
cp -R $REFERENCE_SOURCES/ $REFERENCE_TRANSLATIONS
cp -R $REFERENCE_SOURCES/ $TEST_TRANSLATIONS
# 3. List reference sources
echo
echo Translators tests setup with following reference sources
ls -lR $REFERENCE_SOURCES
# 4. create dir for reports
mkdir -p reports

echo ref sources: $REFERENCE_SOURCES
echo test transl: $TEST_TRANSLATIONS
echo sources dir: $TRANSLATOR_SOURCE  

