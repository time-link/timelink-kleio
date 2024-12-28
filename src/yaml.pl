/** <module> Processing YAML files
 *
 * Structuture files are now expected to be in YAML format.
 * This module contains predicates to process YAML files.
% https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/yaml.html%27)#yaml_read/2
*/

:-use_module(library(yaml)).

read_yaml_file(Filename,Data):-
    yaml_read(Filename,Data).

