
# Tests for kleio

## Semantic tests
This directory contains semantic testing support for the Kleio translator. 

Semantic tests check the output of the translator, for compatibility with a reference output.

Testing is done by comparing the output of a new version of the translator to a stable one, with diff.

A filter is used on the result of the comparison in order to filter out differences that are expected (different paths of files, different automatically generated ids where expected, different time stamps in reports).

### Rationale

The translator is a very stable app that has translated thousands of times various formats of Kleio files.

Semantic changes in the last few years have been minimal. The latest significant change was the introduction
 of translation count to avoid the generation of conflicting ids when a new person was added after the first 
translation, in March 2016. 

In August 2018, as part of the dockerization of MHK, the need to have the translator run as a service, 
with a rest API,triggered a re-write of part of the basic functions dealing with temporary data 
structures because the default dynamic database in Swi-Prolog is not thread safe.      
 
The low level changes could introduce subtle bugs, so a mechanism to ensure that the new infra structure 
would produce the same result was introduced, based on running a stable single threaded version of 
the translator over a set of files, running a new multithreaded REST based version of the translator 
on the same set and comparing the results with diff.

Semantic tests are usefull when architectural changes are done to the code, and it is necessary to ensure
that the output is kept the same.

When new features are implemented that change the output specifications deliberately they will trigger at first
false positive errors by this process. There are three ways to deal with that:
* backport the change to the current reference implementation (if you want to keep it)
* make the current version the reference version (by copying it to tests/stable)
* exclude the diference in generated ouput with patterns in the file `tests/scripts/exclude_while_comparing.grep`
### Overall structure
 
The test root directory has the following layout:
 
* `kleio-home` is the root directory of a Kleio/Timelink instalation
    * `kleio-home/system/kleio` contains configuration files, namely 
        * `kleio-home/system/conf/kleio/token_db` with the token database
        * `kleio-home/system/conf/kleio/stru/gacto2.str` with the default stru file (copied automatically during testing).
* `kleio.home/sources` contains source files. For testing purposes:    
    * `kleio-home/sources/reference_sources` directory contains the sample "cli" files 
    * `kleio-home/sources/reference_translations` directory with a copy of the reference sources and the translation output files: err,rpt,org,old,xml, produced by a stable version of the translator (code in `tests/dev`). 
    * `kleio-home/sources/test_translations` directory contains the same "cli" files and the translation output of the development version of the translator.
* `stable` directory contains the stable version of the translator.
* `dev` directory contains the development version of the translator.
* `scripts` contains auxiliary scripts to manage the testing process.

    * `run_tests.sh` 
      * translates a set of reference files by a stable version of the translator and compares with the translation of the same files by a development version. 
        * erases `reference_translations` and `test_translations` and recreates them with the content of `reference_sources`
        *  runs stable code on `reference_translations` and dev code on `test_translations`
        *  compares the results with a diff
    * `prepare_tests.sh` 
      * setup directories and files for testing. Used by `run_tests-sh`. In detail:
        * cleans the directories `reference_translations`, `test_translations` and `dev`
        * copies the source files in `reference_sources` to `reference_translations` and `test_translations`
        * copies the translator code in `../src` to `dev`.
    * `kleio_translate_local.sh` 
      * auxiliary script to translate files by calling SWI Prolog directly. Used for the reference translations.
    * `kleio_translate_reference.sh` 
      * copies sources in `reference_sources`to `reference_translations` and translates them with stable translator.
    * `kleio_translate_remote.sh` 
      * translates files using the server mode of the translator (calls `kleio_server_start.sh` to start the server). 
        Uses `curl`to make a *rest* request to the server.
    * `clean_tests.sh` 
      * cleans the testing files, and starts testing in a clean state. Unlike `prepare_tests.sh` does not copy a new development version from `clio/src` to `tests/dev`
    * `compare_test_results.sh` 
      * compares the result of reference and test translations by making a diff on `reference_translations` and `test_translations`, excluding non relevant differences with the patterns in `exclude_while_comparing.grep`.


### Running semantic tests

From the `clio/tests` directory do

     ./scripts/run_tests.sh

To rerun the translations with the development version and re-compare with the stable version (requires that `run_tests.sh` was previously run)

    .scripts/run_remote_translations.sh

To view the result of tests check the contents of the file `reports/test_report_YYYY-MM-DD_HH:MM:SS.diff`

Here is how the the reports looks like when there is full compatibility between both translations:

    Qui 7 Out 2021 17:14:26 CST /Users/jrc/develop/timelink-kleio/tests
    Comparing translation results.
    Qui 7 Out 2021 17:21:30 CST /Users/jrc/develop/timelink-kleio/tests
    Only in kleio-home/sources/reference_translations/: .gitignore
    diff -r kleio-home/sources/reference_translations/notariais/docsregiospontepisc.err kleio-home/sources/test_translations/notariais/docsregiospontepisc.err
    diff -r kleio-home/sources/reference_translations/notariais/docsregiospontepisc.rpt kleio-home/sources/test_translations/notariais/docsregiospontepisc.rpt
    diff -r kleio-home/sources/reference_translations/notariais/docsregiospontepisc.xml kleio-home/sources/test_translations/notariais/docsregiospontepisc.xml

    ...
    
    diff -r kleio-home/sources/reference_translations/varia/xpto_comuns_soure.err kleio-home/sources/test_translations/varia/xpto_comuns_soure.err
    diff -r kleio-home/sources/reference_translations/varia/xpto_comuns_soure.rpt kleio-home/sources/test_translations/varia/xpto_comuns_soure.rpt
    diff -r kleio-home/sources/reference_translations/varia/xpto_comuns_soure.xml kleio-home/sources/test_translations/varia/xpto_comuns_soure.xml
    Comparing translation results finished.


And this is how an inconformity looks like:

    diff -r kleio-home/sources/reference_translations/varia/nommiz.err kleio-home/sources/test_translations/varia/nommiz.err
    diff -r kleio-home/sources/reference_translations/varia/nommiz.rpt kleio-home/sources/test_translations/varia/nommiz.rpt
    diff -r kleio-home/sources/reference_translations/varia/nommiz.xml kleio-home/sources/test_translations/varia/nommiz.xml
    25788,25790c25788
    <          <core><![CDATA[rios"]]></core>   </ELEMENT>
    <    <ELEMENT NAME="obs" CLASS="obs">
    <          <core><![CDATA[na listagem de irmaos de 1729 fl.cento e quarenta e dois afirma-se serem "dos rios"]]></core>   </ELEMENT>
    >          <core><![CDATA[rios"/obs=na listagem de irmaos de 1729 fl.cento e quarenta e dois afirma-se serem "dos rios"]]></core>   </ELEMENT>
    >
## Api and server tests

There is also a test suite of API calls in REST and JSON-RPC format. This tests that each function works as expected, but does not check the
semantic aspects related to the output of the translations.

The test suite is generated with postman. it can be run with  the `newman` tool: https://learning.postman.com/docs/running-collections/using-newman-cli/command-line-integration-with-newman/

In the `api/postman` directory at the top level there is a collection special for testing purposes: `api/postman/tests.json`.

To run the test collection make sure you `newman` installed, and from the top level directory do:

    export KLEIO_ADMIN_TOKEN=sometoken;\
     make kleio-run; \
     newman run api/postman/tests.json -e api/postman/tests.postman_environment.json --env-var "testadmintoken=$KLEIO_ADMIN_TOKEN"

You can also run Kleio Server inside VSCode and use swi-prolog debugging tools while running the api tests produced by `postman`.  

+ install `VSC-Prolog` extension in `VSCode`
+ open serverStart.pl on `VSCode` 
+ load the file with Option+X+L
+ in the Prolog terminal that appears do 


      setenv('KLEIO_ADMIN_TOKEN','mytoken').
      
      run_debug_server.

+ From the terminal then do: 

        export KLEIO_ADMIN_TOKEN=mytoken;\
        newman run api/postman/tests.json \
              -e api/postman/tests.postman_environment.json \
              --env-var "testadmintoken=$KLEIO_ADMIN_TOKEN"

+ or

      make tests-api
  
See `postman` documentation about exporting api tests and environment information.

Note that if you change the upload tests for different files than those in the released source you need to update the paths. See https://blog.postman.com/run-collections-with-file-uploads-using-newman/ . 
 * Look for call named "sources_post_upload_new_file" and "sources_upload_new_version"
 * Change `src` parameters:

        "body": {
                  "mode": "formdata",
                  "formdata": [
                    {
                      "key": "file",
                      "type": "file",
                      "src": [""]
                    },
                    {
                      "key": "id",
                      "value": "{{request_id}}",
                      "description": "Id of the request",
                      "type": "text"
                    },
                    {
                      "key": "file",
                      "description": "If \"yes\" overwrite existing file.",
                      "type": "file",
                      "src": []
                    }