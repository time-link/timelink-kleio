# Environment variables for Kleio translator tests
# Source this file: source scripts/env_tests.sh
# for more info see clio/tests/README.md
# run this from clio/tests directory

export KLEIO_HOME=kleio-home
export KLEIO_HOME_DIR=kleio-home
export KLEIO_SOURCE_DIR=${KLEIO_HOME_DIR}/sources
export KLEIO_CONF_DIR=${KLEIO_HOME_DIR}/system/conf/kleio
export KLEIO_STRU_DIR=${KLEIO_HOME_DIR}/system/conf/kleio/stru
# Alternate str file for the test run. newer versions get the str here
export KLEIO_STRU_DIR_ALT=${KLEIO_HOME_DIR}/structures
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
