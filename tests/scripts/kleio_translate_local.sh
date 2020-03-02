# Local file translation using swipl called for each file
#
# This can be used with the stable version or the development versuin
#  usage: kleio_translate_local CLIO_START_FILE CLIO_STRU KLEIO_SOURCES
echo "=============================================="
echo "Translating kleio files in dir " ${3}  "with" $1 "and structure file " $2
PWD=`pwd`
kleio_files="$(find ${3} -type f -name *.cli -o -name *.kleio -o -name *.CLI)"
echo
echo $kleio_files
echo
for file in $kleio_files; do
    if [ -e "${file}" ] 
    then
        echo
        echo "Translating " `pwd` $file
        swipl -f  $1 -- -sf $2 -df "$file" -echo no >/dev/null
    else
        echo ${file} " NOT FOUND"
    fi
done
