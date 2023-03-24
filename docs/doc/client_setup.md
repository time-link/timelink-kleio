# Client setup

For clients to connect to a Kleio Server three
parameters are needed:

1. the URL of a running Kleio Server (KLEIO_URL)
2. a token with admin priviliges (KLEIO_ADMIN_TOKEN)
3. the path to the root directory for the files
   the Kleio Server will access (KLEIO_HOME)



1. the client started the server

If the client started the server they it
can setup the KLEIO_ADMIN_TOKEN and KLEIO_HOME
in the environment or in a .env file. See
https://github.com/time-link/timelink-kleio/blob/master/.env-sample for all the variables that a
Kleio server will use when starting up.

For the KLEIO_URL the relevant variable is 
KLEIO_SERVER_PORT setting the port which the
server is running.


