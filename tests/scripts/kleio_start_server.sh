# kleio_start_server.sh PROLOG_FILE [START_GOAL]
# This script launches de server using PROLOG_FILE on port 8088 of localhost
# example: ./scripts/kleio_start_server.sh dev/serverStart.pl run_server
# If START_GOAL is missing it defaults to "debug_server_until_idle" which lunches a server
# that quits after idle for 60 secs
# See serverStart.pl for examples of server lauching goals.
# run this from tests directory
PWD=`pwd`
SCRIPT_DIR=`dirname $BASH_SOURCE`
START_GOAL="debug_server_until_idle"
if [[ -n $2 ]];
then 
    START_GOAL=$2
fi
echo Running from $BASH_SOURCE
echo "================================================"
echo Lauching kleio translator server from $PWD
echo translator: $1 goal $START_GOAL
# Start server and halt when idle.
swipl -f $1 -g $START_GOAL -t halt >> kleio_start_server.log
echo Kleio server running $2 in port $rest_port 