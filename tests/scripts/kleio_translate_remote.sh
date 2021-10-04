# request file translation to a remote kleio server 
# usage: kleio_translate_remote  KLEIO_SOURCES_DIR
#  example: ./scrips/kleio_translate_remote.sh paroquiais
# to translate every source file do:
# example: ./script/kleio_translate_remote.sh /
# a kleio server must be started before see kleio_start_server.sh
# TODO: much simpler: use update method it will translate all files that need translating
echo
echo "================================================"
echo "Remote translating from kleio source dir " ${1}  
curl --location --request POST "http://localhost:8088/rest/translations/${1}?token=${KLEIO_ADMIN_TOKEN}&id=1234&echo=no&recurse=yes" --data ""
echo "Remote translation scheduled."
