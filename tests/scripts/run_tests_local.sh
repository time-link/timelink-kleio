echo "Runing kleio translator tests locally. Run this command from clio/tests e.g. ./aux/run_tests_local.sh"
echo `date` `pwd` > run_tests_report.diff
./aux/prepare_tests.sh >> run_tests_report.diff
echo "================================================================" 
echo "Translation of reference sources with stable translator"
time ./aux/kleio_translate_local.sh stable/clioStart.pl stable/gacto2.str reference_translations
echo
echo "Translation of reference sources  with local dev version of translator"
time ./aux/kleio_translate_local.sh dev/swiStart.pl dev/gacto2.str test_translations
./aux/compare_test_results.sh >> run_tests_report.diff
echo
echo "Tests done. Check run_tests_report.diff"
