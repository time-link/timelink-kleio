
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
* `.env-tests` is the centralized configuration file for all test scripts.

## Configuration: `.env-tests`

All test scripts share a single configuration file: `.env-tests`. This file defines directory paths,
server settings, and other parameters used throughout the testing pipeline.

Edit `.env-tests` to change defaults such as the server port, admin token, or directory paths.
All scripts accept an optional `[env-file]` parameter to use an alternative configuration file.

Key variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `KLEIO_HOME` | `kleio-home` | Root of the Kleio installation |
| `KLEIO_SERVER_PORT` | `8088` | Server port for REST API testing |
| `KLEIO_ADMIN_TOKEN` | `admintoken` | Token for API authentication |
| `REFERENCE_SOURCES` | `kleio-home/sources/reference_sources` | Directory with input `.cli` files |
| `REFERENCE_TRANSLATIONS` | `kleio-home/sources/reference_translations` | Output from stable translator |
| `TEST_TRANSLATIONS` | `kleio-home/sources/test_translations` | Output from dev translator |
| `STABLE_CODE_DIR` | `stable` | Stable translator code |
| `DEV_CODE_DIR` | `dev` | Dev translator code (copied from `../src/`) |
| `TRANSLATOR_SOURCE` | `../src/` | Current translator source code |
| `REPORT_DIR` | `./reports` | Where test reports are saved |

## Test Scripts

All scripts must be run from the `tests/` directory.
Every script sources `.env-tests` automatically. You can override this by passing an alternative
environment file as the last argument.

### Quick Reference

| Script | Purpose | Can run independently? |
|--------|---------|----------------------|
| `prepare_tests.sh` | Setup environment, copy files | Yes |
| `kleio_translate_local.sh` | Translate files with local Prolog | Yes |
| `kleio_start_server.sh` | Start the Kleio test server | Yes |
| `kleio_translate_remote.sh` | Translate files via REST API | Yes (needs running server) |
| `kleio_stop_server.sh` | Stop the test server | Yes |
| `compare_test_results.sh` | Compare reference vs test output | Yes |
| `clean_tests.sh` | Clean test directories | Yes |
| `test_files.sh` | List files that would be translated | Yes |
| `run_tests.sh` | Full pipeline (all steps) | Yes |
| `run_tests_local.sh` | Full pipeline without server | Yes |
| `run_stable_translations.sh` | Translate with stable only | Yes |
| `run_remote_translations.sh` | Translate with dev server only | Yes |

### Running the full test suite

From the top level of the repository:

    make test-semantics

Or from the `tests/` directory:

    ./scripts/run_tests.sh

This runs the complete pipeline: prepare, translate with stable, translate with dev server, compare.

### Running individual steps

The key advantage of the refactored scripts is that each step can be run independently.
This means you can, for instance, redo the comparison without having to retranslate everything.

#### 1. Prepare the test environment

    cd tests
    ./scripts/prepare_tests.sh

This cleans the test directories, copies the current source code from `../src/` to `dev/`,
copies structure files to the configuration directories, and copies the reference sources
into both `reference_translations` and `test_translations`.

#### 2. Translate with the stable translator (reference)

    ./scripts/kleio_translate_local.sh stable/swiStart.pl stable/gacto2.str kleio-home/sources/reference_translations

Or use the convenience wrapper:

    ./scripts/run_stable_translations.sh

This produces the baseline output in `reference_translations/` that the dev output will be compared against.

#### 3. Translate with the dev translator

**Local mode** (calls SWI-Prolog directly, no server):

    ./scripts/kleio_translate_local.sh dev/swiStart.pl dev/gacto2.str kleio-home/sources/test_translations

**Server mode** (uses REST API):

    # Start the server (runs in background)
    ./scripts/kleio_start_server.sh dev/serverStart.pl &
    sleep 5

    # Send translation requests
    ./scripts/kleio_translate_remote.sh sources/test_translations

    # Stop the server when done
    ./scripts/kleio_stop_server.sh

#### 4. Compare results

    ./scripts/compare_test_results.sh

This compares `reference_translations/` with `test_translations/`, filtering out expected
differences (timestamps, auto-generated IDs, file paths) using the patterns in
`scripts/exclude_while_comparing.grep`.

To save the comparison to a report file:

    ./scripts/compare_test_results.sh > reports/my_comparison.diff

To compare two specific directories:

    ./scripts/compare_test_results.sh /path/to/reference /path/to/test

### Common workflows

**Re-run comparison after adjusting `exclude_while_comparing.grep`:**

    # No need to retranslate, just re-compare
    ./scripts/compare_test_results.sh

**Translate and compare in local-only mode (no server):**

    ./scripts/run_tests_local.sh

**Re-run only the dev translation and compare:**

    # Translate with dev (local mode)
    ./scripts/kleio_translate_local.sh dev/swiStart.pl dev/gacto2.str kleio-home/sources/test_translations

    # Compare
    ./scripts/compare_test_results.sh

**Use a custom configuration:**

    ./scripts/run_tests.sh ./my-custom.env

### Testing small changes

The best way to do unit tests is:

1. create a test data file in `tests/kleio-home/sources/test_translations/reference_sources/issues`. All the files in the `reference_sources` directory and sub-directories will be copied to `tests/kleio-home/sources/api` where they can be translated.

2. In `src/serverStart.pl` scroll down to the `translate_file` predicate and add a clause with the
   relative path to the new file, e.g. `sources/api/issues/issue34.cli`. Each `translate_file` clause with `true` in the second argument will be translated.


```prolog

translate_file('sources/api/varia',false).
translate_file('sources/api/issues/issue34.cli',true).

```
3. In this way you can keep a set of test files and activate or deactivate them for testing purposes.

4. In the `src/serverStart.pl` execute `run_tests(server)`. This will copy the files from `reference_sources` to `api` and trigger the translation of those which comply to `translate_file(F,true)`.
5. The easiest way is to use the VSC-Prolog extension, do ALT+XL to load the server code and type
   `run_tests(server)` in the prolog prompt that pops up. Subsequent tests can reuse the terminal and cursor
   keys to recover the command. If changes are made to the code in the testing session, do `make.` in the
   prolog prompt to reload the server code.
6. Since the code will be running in a separe thread, use  `tspy` to set breakpoints. See https://www.swi-prolog.org/pldoc/man?section=threaddebug.

### Running semantic tests

From the top level of the repository do:

     make test-semantics


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

### Filtering expected differences

The file `scripts/exclude_while_comparing.grep` contains regex patterns for differences
that are expected between the stable and dev translators (timestamps, auto-generated IDs,
version strings, file paths, etc.).

When a new feature changes output deliberately, add a pattern to this file to suppress
the expected difference in future comparisons.

## Api and server tests

There is also a test suite of API calls in REST and JSON-RPC format. This tests that each function works as expected, but does not check the
semantic aspects related to the output of the translations.

The test suite is generated with postman. it can be run with  the `newman` tool: https://learning.postman.com/docs/running-collections/using-newman-cli/command-line-integration-with-newman/

In the `api/postman` directory at the top level there is a collection special for testing purposes: `api/postman/tests.json`.

To run the test collection make sure you `newman` installed and from the top level directory do:

    make test-api

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
