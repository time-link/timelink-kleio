# Kleio translation services for Timelink.

`kleio-server` provides access to translation services of _Kleio_ files. 

## About `Kleio` files

_Kleio_ files are text files with a special notation designed to the transcription of historical sources. The notation was created by Manfred Thaller, as part of the _Kleio historical database system_ (http://web.archive.org/web/20130603204750/http://www.hki.uni-koeln.de/kleio/old.website/).

Although Thaller's Kleio database system is no longer publically available, the notation developped for historical source transcritpion proved very powerfull, providing a concise way for the transcription of complex historical documents.

The _Timelink-Kleio translator_ implements a subset of the Kleio notation designed for the Timelink database system. _Timelink_ provides a set of data models designed for handling person-oriented information collected in historical documents.


This is how a (portuguese) baptism looks like in _Timelink_ Kleio notation:

      bap$b1714-2/12/11714/fl.117v./igreja de sao silvestre/manuel lopes serra (padre)

         celebrante$manuel lopes serra
            ls$profissao/padre

         n$francisca/f

            pn$antonio ferreira
               ls$morada/espinheiro
               ls$freguesia/lousa

            mn$leonarda francisca

            pad$joao fernandes ramalheiro
               ls$morada/moita
               ls$freguesia/lousa

            mad$francisca
               ls$ec/solteira

               pmad$joao goncalves
                  ls$morada/espinheiro
                  ls$freguesia/lousa

Text files implementing the Timelink Kleio notation can be translated by
the _Timelink_ `kleio-server` and imported into the _Timelink_ relational database. 

Translation is _intelligent_ in the sense that it operates a _normalization_ of the source information, infering information from the context, and so greatly reducing the overhead of producing normalized data. 

_Timelink_ database-services then implement a set of functions that allow the identification of people, reconstruction of biographies, inference of personal networks, and other funcionalities.

For more information on Timelink Kleio Server see: https://github.com/time-link/timelink-kleio


## Services provided by the `kleio-server` API

The API is designed to decouple Kleio source handling from other software components. It provides funcionality to translate source files, inspect results of translation for errors or warnings, obtain the generated data in XML format. It also allows basic file management.

Main services privided by the API are:

* translations: translates kleio source files.
* sources: lists available sources for translation.
* file management services: including downloading, uploading and deleting files (sources and structures), creating,copying, moving and deleting directories.
* permission management using tokens: generate_token, invalidate_token,invalidate_use: magament of 'authorization' with token.
* basic git interaction: fetch, pull, commit, push.

## Api documentation

API documentation is available at docs/api/index.html

## Running the server with docker

* Copy or rename  `.env-sample`  to `.env`.
* Change variables according to your setup. 
   * `KLEIO_HOME` should be set to a directory where kleio files reside.
   * `KLEIO_SERVER_IMAGE` can be set to run a specific image otherwise `kleio-server:latest` is used. 
   * `KLEIO_ADMIN_TOKEN` if you want to set a starting admin token. If you do not set the env variable `KLEIO_ADMIN_TOKEN` token and `kleio-server` finds no previously defined tokens then a token named `bootstrap` is created on server startup with permission `generate_token` and a life span of 5 minutes. The token is written to a file in `KLEIO_CONF_DIR`. This token can be used to generate through an api call an initial admin token. 
   * `KLEIO_DEBUG`if "true" will produce debug information in the log.
   * More variables are available to fine tune the settings of the Kleio Server. See `.env-sample` for a full list.
* run with `make start` 

## Development

### Requirements

Kleio server is written in `SWI-Prolog` https://www.swi-prolog.org

Recommended tools:

* `VSCode` with `VSC-Prolog` extension https://marketplace.visualstudio.com/items?itemName=arthurwang.vsc-prolog
* `Postman` https://www.postman.com
  * Directory api/postman contains exported postman collections and environments. 
* `Postman-doc-gen`: generates static documentation from Postman collections https://github.com/karthiks3000/postman-doc-gen
* `newman`: the command line tool for running Postman generated test suites https://learning.postman.com/docs/running-collections/using-newman-cli/command-line-integration-with-newman/
  

### Running the server locally for debugging

Easiest way: 
+ open serverStart.pl on `VSCode` with `VSC-Prolog`
+ Load the file with Option+X+L
+ in the Prolog terminal that appears do 
  

      setenv('KLEIO_ADMIN_TOKEN','your-token-anything-with-more-than-5-chars-will-work').
      
      run_debug_server.
+ note that the server started in this way does not read the content 
  of the .env file. 
+ set spy points with tspy(_predicate_) 
+ Clients can access the kleio-server using the token set above. 

### Debugging inside the docker container

After building the image with `make image` run a shell in the kleio-server container


      docker compose run kleio sh

At the prompt start swi Prolog and change to the kleio-home directory

      # swipl -f serverStart.pl

      ....

      ?- working_directory(_,'/kleio-home').

### Tests

There two type of tests:

* _semantic_ tests compare the result of a new translation to a reference translation.
* _api_ tests check if the exposed API works as expected.

See `README.md` in the directory `tests` for details. The tests directory also contains
reference files and the reference translator necessary to run both semantic and api tests.

### Updating 
## Preparing images
 
 * ```make image```: create a new image, and tags it kleio-server:Knnn, with Knnn being a sequential build number
  
The following tags are used:
* _unique_ - current build number as Knnn. Note that the tag unique is not used as such, it is interpreted as the last build number.
* latest - latest useful build. Used to keep updated with latest release.
* stable - stable build for a specific version. 
* _version_ - version number as MM.mm goes together with stable to provide clients with stable images for specific version.

To help manage the tags the following targets exist

* ```make tag-TAG``` tag last image with TAG in latest | unique | stable, note that _unique_ tags the last 
* ```make inc_NUMBER``` increment version with NUMBER in major | minor
* ```make push-TAG``` push last image with TAG in latest | unique | stable, to the
docker repository defined in the DOCKER_REPOSITORY variable in the Makefile.

## Make targets

Type ```make``` to see more targets that help in development.


      % make
      usage:
         make image                build docker image and tag with new build number
         make build                return the current build number
         make version              return the current version string (major.minor)
         make current              return the current version, build number
         make last                 return the last image build date, version, build number
         make inc-NUMBER           increment version with NUMBER in major | minor
         make token                generate token for KLEIO_ADMIN_TOKEN
         make start                start Kleio server on docker (requires image)
         make stop                 stop Kleio server
         make docs                 generate api docs (requires postman_doc_gen and api files)
         make tag-TAG              tag last image with TAG in latest | unique | stable
         make push-TAG             push image with TAG in latest | unique | stable
         make kleio-run            start server with .env config and tests/docker_compose.yaml
         make kleio-stop           stop running server
         make test-semantics       run semantic tests

## History

* 10.15 2022-01-20
  * generates a bootstrap token when run with no tokens defined. The bootstrap token expires after 5 minutes.

* 10.14  2021-10-09
  * Better token generation, more secure
  * Add json representation of structure, STRU.json
  * Add env variable for admin token
  * Improved tests, new make target
  * Fix old bug that kept ids files after translation
  * Improve definition of bem, so that id at end, and field sizes increased
  * Fix problem with warning on file name and id prefixes
