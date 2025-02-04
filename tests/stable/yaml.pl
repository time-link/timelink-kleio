/** <module> Processing YAML files
 *
 * Structuture files are now expected to be in YAML format.
 * This module contains predicates to process YAML files.
% https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/yaml.html%27)#yaml_read/2
*/

:-use_module(library(yaml)).
:-use_module(library(pprint)).

read_yaml_str(Filename,Data):-
    yaml_read(Filename,Data),
    inspect_yaml_str(Data.get(structure)).

% example read mappings
% File='/Users/jrc/develop/timelink-kleio/tests/kleio-home/mappings/sample-mapping.yml',
%    read_yaml_file(File,D),print_term(D,[]).
% example read str
% File='/Users/jrc/develop/timelink-kleio/tests/kleio-home/structures/yaml/sample-str.yaml',
%     read_yaml_file(File,D),print_term(D,[]).
inspect_yaml_str(YamlList):-
    member(YamlCMD, YamlList),
    inspect_yaml_str_cmd(YamlCMD),
    fail,
    !.
inspect_yaml_str(_):-!.

inspect_yaml_str_cmd(YamlTerm):-
    Params = YamlTerm.Command,
    format('Command: ~w~n',[Command]),
    inspect_yaml_str_cmd(Command,Params),
    nl.

inspect_yaml_str_cmd(extends,FileSpec):-
    format('Read ~w~n', [FileSpec]).

inspect_yaml_str_cmd(element,Pars):-
    is_dict(Pars),
    bagof(Par=Value, Value = Pars.Par, ParList),
    % writeln('Bagof Process element with pars '-ParList),
    option(name(Name),ParList,name-missing),
    member(Par=Value,ParList),
    writeln('Processing element '-Name-Par-Value),
    fail.
inspect_yaml_str_cmd(element,_):-!.

inspect_yaml_str_cmd(group,Pars):-
    is_dict(Pars),
    bagof(Par=Value, Value = Pars.Par, ParList),
    % writeln('Bagof Process group with pars '-ParList),
    option(name(Name),ParList,name-missing),
    member(Par=Value,ParList),
    format(' (~w) ~w = ~w ~n', [Name, Par,Value]),
    fail.
inspect_yaml_str_cmd(group,_):-!.






