# prepare tests
# 1. cleans contents of test directories
# 2. copies clio/src to clio/tests/dev folder
# 3. copies .cli, .kleio .CLI .KLEIO from reference_sources
#      to reference_translations and test_translations
# 4. cleans contents of test_translations directory
# for more info see clio/tests/README.md
# run this from clio/tests directory
echo "================================================================"
echo "===              PREPARING KLEIO TRANSLATOR TESTS            ==="
echo "================================================================"
# Source environment variables
source scripts/env_tests.sh

# 1. clean contents of test directories
rm -rf $REFERENCE_TRANSLATIONS/*
rm -rf $TEST_TRANSLATIONS/*
rm -rf $DEV_CODE_DIR/*
# 2. copy current src and reference sources
cp -R ${TRANSLATOR_SOURCE}/* $DEV_CODE_DIR/
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
