# request file translation to a remote kleio server 
#  usage: kleio_translate_remote  KLEIO_SOURCES_DIR
#  example: ./aux/kleio_translate_remote.sh paroquais
# to translate every source file do:
# example: ./aux/kleio_translate_remote.sh /
# a kleio server must be started before see kleio_start_server.sh
# TODO: much simpler: use update method it will translate all files that need translating
echo
echo "================================================"
echo "Remote translating from kleio source dir " ${1}  
curl --location --request GET "http://localhost:8088/rest/update/${file}?token=ab4d8d2a5f480a137067da17100271cd176607a1&id=1234&echo=no&structure=gacto2.str" --data ""
echo "Remote translation scheduled."
