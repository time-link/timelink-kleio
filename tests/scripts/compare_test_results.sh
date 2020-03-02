# Compares the output of two directories with trasnlation results
# comparing is based on standard diff but "natural occuring" differences
#   are filtered.
echo "Comparing translation results." 
echo `date` `pwd` 

diff -r $REFERENCE_TRANSLATIONS/ $TEST_TRANSLATIONS/ | grep -v -f aux/exclude_while_comparing.grep 
echo "Comparing translation results finished."
