# Local file translation using local prolog executable
#  usage:
#   ./aux/test_files prologScript StruFile SOURE_DIR
echo "=============================================="
echo "Listing files from dir " ${3}  "with" $1 "and structure file " $2
PWD=`pwd`
kleio_files="$(find ${3} -type f -name *.cli -o -name *.kleio -o -name *.CLI)"
for file in $kleio_files; do
    [ -f "${file}" ] || break
    echo
    echo "Translating " `pwd`/$file
    echo "swipl -f"  $1 "-- -sf" $2 "-df" "$file" "-echo yes >/dev/null"
done
