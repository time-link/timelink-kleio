echo "================================================================" 
echo "Translation of reference sources with stable translator"
echo "RUN FROM tests DIRECTORY!"
echo "Runing from " `pwd`
rm -r reference_translations/*
cp -r reference_sources/ reference_translations/
./aux/kleio_translate_local.sh stable/clioStart.pl stable/gacto2.str reference_translations
echo Translation of reference sources done
echo
