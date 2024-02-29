# Kleio translation services for Timelink.

`kleio-server` provides access to translation services for _Kleio_ files.

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

The API is designed to decouple Kleio source handling from other software components. It provides funcionality to translate source files, inspect results of translation for errors or warnings, obtain the generated data in XML format. It also allows basic file management and basic git operations, so that it can be used to isolate other software components from directly handling file related operations.

Main services privided by the API are:

* translations: translates kleio source files.
* sources: lists available sources for translation.
* file management services: including downloading, uploading and deleting files (sources and structures), creating,copying, moving and deleting directories.
* permission management using tokens: generate_token, invalidate_token,invalidate_use: magament of 'authorization' with token.
* basic git interaction: fetch, pull, commit, push.

API documentation is available in [docs/api](docs/api/index.html)

## Api documentation

API documentation is available at [docs/api/index.html](docs/api/index.html)

## Running the server with docker

### Run the latest version from docker hub

```bash
$ docker run -v $(PWD):/kleio-home -p 8088:8088 -d  timelink-server/kleio-server
```

### Build and run locally

```bash
make build-local

$ docker run -v $PWD:/kleio-home -p 8088:8088 -d  kleio-server
```

### Change the external linking port

To have the server listening on port 8089

```console
$ docker run -v $(PWD):/kleio-home -p 8088:8089 -d  kleio-server
```

### Set an admin token

```console
$ docker run -v $(PWD):/kleio-home -e KLEIO_ADMIN_TOKEN=myprivatetoken -p 8088:8088 -d  kleio-server
```

If `KLEIO_ADMIN_TOKEN` is not set the server will generate
a token with admin privileges which can be
obtained in the .kleio.json
file in the directory mapped to /kleio-home

```console
$docker run -v $PWD:/kleio-home -p 8088:8088 -d  kleio-server

$cat $PWD/.kleio.json
```

The generated token is associated with user kleio_admin,
and can be invalidated by a client after generating a
new one.

### Run under current user

On linux system docker will run under the root user.
This makes the server generate files that will
generate permission errors when accessed by the current user.

To run the kleio server user the current user:

```console
docker run -v $PWD:/kleio-home -u $(id -u):$(id -g) -p 8088:8088 -d  kleio-server
```

### More configuration options

There are more environment variables that can be
set to control the behaviour of `kleio-server`.

The easiest way to test in to use `docker compose`
with the `docker-compose.yaml`file in this directory
and set the variables in a `.env` file:

* Copy or rename  `.env-sample`  to `.env`.
* Change variables according to your setup.
   * `KLEIO_HOME` should be set to a directory where kleio files reside.
   * `KLEIO_SERVER_IMAGE` can be set to run a specific image otherwise `timelinkserver/kleio-server:latest` is used.
   * `KLEIO_ADMIN_TOKEN` if you want to set a starting admin token. If the env variable `KLEIO_ADMIN_TOKEN` is unset
    then a token named `kleio-admin` is created on server startup with full admin permissions.
     The token is written to file `/.admin_token` in `KLEIO_CONF_DIR`.
     This token can be used to generate another token through an api
      call and invalidate this one
   * `KLEIO_DEBUG`if "true" will produce debug information in the log.
   * More variables are available to fine tune the settings of the Kleio Server. See `.env-sample` for a full list.
* run with `make kleio-run-latest` or `docker compose up`

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
+ install swipl locally
+ install `VSC-Prolog` extension in `VSCode`
+ open serverStart.pl on `VSCode`
+ load the file with Option+X+L
+ in the Prolog terminal that appears do

      setenv('KLEIO_ADMIN_TOKEN','mytoken').
      setup_and_run_server(run_debug_server,[port(8089)]).

+ note that the server started in this way does not read the content
  of the .env file.
+ set spy points with tspy(_predicate_)
+ Clients can access the kleio-server using the token set above.

See the `README.md` file inside the `tests` directory for more information on testing.

### Debugging inside the docker container

After building the image with `make build-local` run a shell in the kleio-server container


      docker compose run kleio sh

At the prompt start swi Prolog and change to the kleio-home directory

      # swipl -f serverStart.pl
      ?- working_directory(_,'/kleio-home').
      ?- run_debug_server.

### Debugging together with MHK

Currently MHK debugging is done by starting
the current build together with the other
containers (mySQL/MariaDB, Caddy, Portainer and Kleio)
through a Docker compose file and by attaching the Intellij
IDEA debugger to the running MHK app.

The recomended way is to stop the kleio server that is
started with MHK and start a local server in VSCode as
explained above, and associating it with the same
mhk-home directory of the MHK beeing jointly tested,
as described bellow.

In order for MHK to connect to the local running Kleio server the property `mhk.kleio.service`
   should be set to  http://host.docker.internal:8088
   in the file mhk-home/system/conf/mhk_system.properties

This means that the MHK instance running in Docker will try
to connect to port 8088 in the local host. Values of
http://127.0.0.1:8088 ot http://localhost:8088

The sequence should be:
1. Build new docker image of MHK
2. Update local MHK install with
   local image: `mhk update --local` (make sure
   mhk is updating from latest image wiht `mhk use-tag latest`)
3. Start mhk with `mhk start`
4. Stop the kleio server `mhk stop kleio`

Steps 1-3 above are normally automated inside Intellij
IDEA with a Debug configuration.

Start the Kleio server inside VSCode:
+ install `VSC-Prolog` extension in `VSCode`
+ open serverStart.pl on `VSCode`
+ load the file with Option+X+L
+ in the Prolog terminal that appears do

      run_from_mhk_home(H,P)

This will start a kleio server from USER_HOME/mhk-home
running in port 8088. If mhk-home used by MHK is at
a different location pass the path in H. In a port
other than 8088 is to be used pass it in P.


### Tests

There two type of tests:

* _semantic_ tests compare the result of a new translation to a reference translation.
* _api_ tests check if the exposed API works as expected.

See `README-tests.md` in the directory `tests` for details. The tests directory also contains
reference files and the reference translator necessary to run both semantic and api tests.

### Updating
#### Preparing images

 * ```make build-local```: create a new local image, and tags it kleio-server:Knnn, with Knnn being a sequential build number

The following tags are used:
* latest - latest useful build. Used to keep updated with latest release.
* stable - stable build for a specific version.
* _version_ - version number as MM.mm goes together with stable to provide clients with stable images for specific version.

To help manage the tags the following targets exist

* ```make tag-multi-TAG | tag-local-TAG``` tag last image with TAG in latest | unique | stable, note that _unique_ tags the last build number
* ```make inc_NUMBER``` increment version with NUMBER in major | minor
* ```make push-TAG``` push last image with TAG in latest | unique | stable, to the
docker repository defined in the DOCKER_REPOSITORY variable in the Makefile.

## Release sequence

* Create docker image `make build-multi` (also pushes to repository tag with current version)
  * __Note that the reference repository is `timelinkserver/kleio-server` login before with
   `docker login --username timelinkserver` or other
   authorized user.__
* Test
* Update version with `make inc-major` or `make inc-minor`
* Update release notes (bellow)
* Commit code
* Check current version with `make show-current` or
   `make show-last`
* Tag image with semantic versioning
  * if new image is stable version do `make tag-multi-stable` (this also tags latest commit with last version number).
  * Move current code to `stable`directory in `tests`.
  * if new image to be latest do `make tag-multi-latest`


## Make targets

Type ```make``` to see more targets that help in development.

      make build-local          build a local docker image and tag with new build number
      make tag-local-TAG        tag last local image with TAG in latest | stable
      make build-multi          build local mage and push multi platform docker imagea tagged with new build number
                                (requires login in dokerhub with 'docker login' )
      make tag-multi-TAG        tag last multi platform image with TAG in latest | stable
      make show-build           return the current build number
      make show-version         return the current version string (major.minor)
      make show-current         return the current version, build number
      make show-last            return the last image build date, version, build number
      make show-env             show KLEIO env variables currently defined
      make inc-NUMBER           increment version with NUMBER in major | minor
      make gen-token            generate a string suitable for KLEIO_ADMIN_TOKEN for .env file
      make bootstrap-token      generate and register a token for 'admin' during bootstrap
                                    (only if no tokens exist and server running < 5 minutes)
      make docs                 generate api docs (requires postman_doc_gen and api files)
      make pull-tag tag=x.y.z   pull image with tag x.y.z from docker hub
      make kleio-run-latest     start server with latest multi platform image, .env config and tests/docker_compose.yaml
      make kleio-run-current    start server with most recent build, .env config and tests/docker_compose.yaml
      make kleio-run-tag tag=x.y.z    start server with image with tag x.y.z, .env config and tests/docker_compose.yaml
      make kleio-stop | stop    stop running server
      make test-semantics       run semantic tests
      make test-api             run api tests (requires newman (npm install newman))


## Release notes

( `make show-last` to have timestamp and version info)

### 2024-02-29 07:40:48 version 12.5.570

Fix bugs, linked data ids now allow for dashes, improve error and warning line reporting

### 2024-02-07 13:03:06 version 12.4.567

* Reverted to previous attribute format for linked data see [#27](https://github.com/time-link/timelink-kleio/issues/27)

### 2024-01-23 07:16:28 version 12.3.561

* Refactoring of linked data tratment. Notation for linked data remains the same
  but it is allowed in any element of any group extending person,object or geoentity.
* Generation of attributes from linked data notation was changed for clarity. See issue#6
* Fixes a very old problem with handling ";" for multiple entries in comments and original wording.
  This became a problem because linked data notation tends to produce comments with a bit of text
  for context, and ";" shows up.

### 2024-01-07 12:20:12 version 12.1.557

* Fixes problem with server failing if kleio-home does not contain system/conf/kleio #16
* Improve json version of stru for better tool tips.
* Fix bug with handling of implicit and explicit list of elements
* Fixes error when using linked data notation with no link$ statement. Now issues warning.

### 2023-09-27 14:23:12 version 12.0.550

* Fixes problems with cleaning translation results.
* Now takes into account new "files.json" file.

### 2023-12-13 18:44:37 version 12.1.554

Fixes a bug in translation of identification files.

### 2023-09-07 16:58:13 version 12.0.545

Fixes bugs, first tests with timelink-py client.

### 2023-07-04 04:34:23 version 12.0.540

Adds linked data notation.

1. Declare the external source, with an attribute of the kleio
   document:
      kleio$...
         link$wikidata/"http://wikidata.org/wiki/$1"

    The format of the link$ group is:
        link$short-name/url-pattern

    where `short-name` is a short name for an external sources
    `url-pattern` should be a url containing a place
    holder ($1) for a specific id of the data item to be linked

2. Anotate element values with external ids

    ls$jesuita-entrada/Goa, Índia# @wikidata:Q1171/15791200

    The format of an external link annotation is:

        @short-name:id

On translation annotation such as

   ls$jesuita-entrada/Goa, Índia# @wikidata:Q1171/15791200

will generate

   ls$ jesuita-entrada@/https://www.wikidata.org/wiki/Q1171/15791200/obs=%Goa, Índia

### 2023-06-05 02:04:07 version 11.1.541

* Improves line number reporting in error messages

Internal:

* Add XSD schema for Kleio export format
* Improve make file (shows port when runing server)
* Uses 8088 for api tests

### 2023-03-28 14:49:22 version 11.1.536

* Implements issue #7 allowing str files
  to be located close to the sources

   * see [docs/doc/stru_file_location.md](docs/doc/stru_file_location.md)

* At startup the server generates a `.kleio.json` file
  with the running configuration, including an admin token.

* New doc about [client setup](docs/doc/client_setup.md)

* New translation result summary in json [translation results](docs/doc/translation_results.md)
### 2022-12-26 18:16:47 version 10.19.484

Clean up targets for build and run
### 2022-12-26 13:32:53 version 10.19.482

Adds multi-architecture docker builds.
### 2022-12-25 10.19.464

Fixes [issue 5](https://github.com/time-link/timelink-kleio/issues/5) generates bootstrap token at
each startup.
### 2022-06-15 10.18.463

Fixes CORS pre-flight requests.

### 2022-06-08 v10.17.456

Adds cors capability. Set env KLEIO_CORS_SITES with
comma separeted list of allowed sites or "*" for all.
### 2022-03-17 v10.16.452

* More consistent usage of semantic versioning.
* Now stable images are tagged with major version number, minor version number and patch
e.g. "10", "10.16" and "10.16.452"
* Development versions are only tagged with full patch tag, e.g. "10.16.452"
### 2022-03-17 v10.16 build k448

* Added make targets for api-testing. Type "make help" for a list of targets
* See `development` section bellow.

### 2022-01-20 v10.15
  * generates a bootstrap token when run with no tokens defined. The bootstrap token expires after 5 minutes.

### 2021-10-09
  * Better token generation, more secure
  * Add json representation of structure, STRU.json
  * Add env variable for admin token
  * Improved tests, new make target
  * Fixed old bug that kept ids files after translation
  * Improve definition of "bem", so that id at end, and field sizes increased
  * Fixed problem with warning on file name and id prefixes
