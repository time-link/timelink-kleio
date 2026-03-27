echo "================================================================" 
echo "Translation of reference sources with stable translator"
echo "RUN FROM tests DIRECTORY!"
echo "Runing from " `pwd`
source ./scripts/env_tests.sh
set -x
rm -r $TEST_TRANSLATIONS
cp -R $REFERENCE_SOURCES/ $REFERENCE_TRANSLATIONS
./scripts/kleio_translate_local.sh $STABLE_CODE_DIR/swiStart.pl $STABLE_CODE_DIR/gacto2.str $REFERENCE_TRANSLATIONS
echo Translation of reference sources done
echo
