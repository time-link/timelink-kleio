
## Tests for kleio

This directory contains semantic testing support for the Kleio translator. 

It allows new versions of the translator and/or the gacto2.str scheme file to be tested in what regards the generated output.

Testing is done by comparing the output of a new version of the translator to a stable one. 

A filter is used on the result of the comparison in order to filter out differences that are expected and not errors (different paths of files, different automatically generated ids where expected, different time stamps in reports).
## Rationale

The translator is a very stable app that has translated thousands of times various formats of Kleio files.

Changes in the last few years have been minimal. The latest significant change was the introduction of translation count to avoid
the generation of conflicting ids when a new person was added after the first translation, in March 2016. 

In August 2018, as part of the dockerization of MHK, the need to have the translator run as a service, with a rest API,
triggered a re-write of part of the basic functions dealing with temporary data structures because the default 
dynamic database in Swi-Prolog is not thread safe.      
 
The low level changes could introduce subtle bugs so a mechanism to ensure that the new infra structure would produce
the same result was introduced based on running a stable single threaded version of the translator over a set of files,
running a new multithreaded REST based version of the translator on the same set and comparing the results with diff.
 
## Overall structure
 
The test root directory has the following layout:
 
* `kleio-home` is the root directory of a kleio/Timelink instalation
    * `kleio-home/system/kleio` contains configuration files, namely 
        * `kleio-home/system/conf/kleio/token_db` with the token database
        * `kleio-home/system/conf/kleio/stru/gacto2.str` with the default stru file.
* `kleio.home/sources` contains source files. For testing purposes:    
    * `kleio-home/sources/reference_sources` directory contains the sample "cli" files 
    * `kleio-home/sources/reference_translations` directory with a copy of the reference sources and the translation output files: err,rpt,org,old,xml, produced by a stable version of the translator. Note that in this directory the "cli" files will be transformed by the translator (the original files from `reference_sources` will be kept with the "org" extension, as is normal operation of the translator).
    * `kleio-home/sources/test_translations` directory contains the same "cli" files and the translation output of the development version of the translator.
* `stable` directory contains the stable version of the translator.
* `dev` directory contains the development version of the translator.
* `scripts` contains auxiliary scripts to manage the testing process.

    * `run_tests.sh` 
      * translates a set of reference files by a stable version of the translator and compares with the translation of the same files by a development version. 
    * `prepare_tests.sh` 
      * setup directories and files for testing. Used by `run_tests-sh`. In detail:
        * cleans the directories `reference_translations`, `test_translations` and `dev`
        * copies the source files in `reference_sources` to `reference_translations` and `test_translations`
        * copies the translator code in `../src` to `dev`.
    * `kleio_translate_local.sh` 
      * auxiliary script to translate files by calling SWI Prolog directly.
    * `kleio_translate_reference.sh` 
      * copies sources in `reference_sources`to `reference_translations` and translates them with stable translator.
    * `kleio_translate_remote.sh` 
      * translates files using the server mode of the translator. The Kleio server must be running (`kleio_server_start.sh` starts the server). 
        Uses `curl`to make a *rest* request to the server.
    * `clean_tests.sh` 
      * cleans the testing files, and starts testing in a clean state. Unlikely `prepare_tests.sh` does not copy a new development version from clio/src.
    * `compare_test_results.sh` 
      * compares the result of reference and test translations by making a diff on `reference_translations` and `test_translations`, excluding non relevant differences with the patterns in `exclude_while_comparing.grep`.


## Running testes

From the `clio/tests` directory do

     ./scripts/run_tests.sh

To rerun the translations with the development version and re-compare with the stable version (requires that `run_tests.sh` was previously run)

    .scripts/rerun_tests.sh (not implemented)

To view the result of tests check the contents of the file `reports/run_tests_report.diff`

    open run_tests_report.diff
