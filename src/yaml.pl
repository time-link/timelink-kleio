/** <module> Processing YAML files
 *
 * Structuture files are now expected to be in YAML format.
 * This module contains predicates to process YAML files.
% https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/yaml.html%27)#yaml_read/2
*/

:-use_module(library(yaml)).
:-use_module(library(pprint)).
:-use_module(persistence).

read_yaml_str(Filename,Data):-
    yaml_read(Filename,Data),
    put_value(yaml_file,Filename),
    inspect_yaml_str(Data).

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

inspect_yaml_str_cmd(element,Pars):-!,
    is_dict(Pars),
    bagof(Par=Value, Value = Pars.Par, ParList),
    % writeln('Bagof Process element with pars '-ParList),
    option(name(Name),ParList,name-missing),
    member(Par=Value,ParList),
    format('(~w)   ~w = ~w ~n', [Name,Par,Value]),
    fail.
inspect_yaml_str_cmd(element,_):-!.

inspect_yaml_str_cmd(group,Pars):-!,
    is_dict(Pars),
    bagof(Par=Value, Value = Pars.Par, ParList),
    % writeln('Bagof Process group with pars '-ParList),
    option(name(Name),ParList,name-missing),
    member(Par=Value,ParList),
    format(' (~w) ~w = ~w ~n', [Name, Par,Value]),
    fail.
inspect_yaml_str_cmd(group,_):-!.

inspect_yaml_str_cmd(include,Par):-
    atomic(Par),!,
    format('Include file: ~w ~n',[Par]),
    include_yaml_str(Par,Data),
    inspect_yaml_str(Data),!.

inspect_yaml_str_cmd(Other,Pars):-
    is_dict(Pars),
    bagof(Par=Value, Value = Pars.Par, ParList),
    % writeln('Bagof Process group with pars '-ParList),
    member(Par=Value,ParList),
    format('Uknown element (~w) ~w = ~w ~n', [Other, Par,Value]),
    fail.

include_yaml_str(File,Data):-
    normalize_str_path(File,Path),
    absolute_file_name(Path,AbsPath),
    read_yaml_str(AbsPath,Data).

normalize_str_path(File,Path):-
    atomic_list_concat(Dirs, '/',File),
    create_str_path(Dirs,DirsExpanded),
    atomic_list_concat(DirsExpanded,'/',Path).

create_str_path([system|MoreDirs],[SysStruDir|MoreDirs]):-!,
    kleioFiles:kleio_stru_dir(SysStruDir).

create_str_path([structures|MoreDirs],[LocalStruDir|MoreDirs]):-!,
    % replace this with structures from user token
    kleio_user_structure_dir(LocalStruDir,[structures='tests/kleio-home/structures']).

create_str_path(['.'|MoreDirs],[MainFileDir|MoreDirs]):-!,
    get_value(yaml_file,MainFilePath),
    file_directory_name(MainFilePath,MainFileDir).


create_str_path([FileOnly|[]], [MainFileDir,FileOnly]):-
    atomic(FileOnly),
    get_value(yaml_file,MainFilePath),
    file_directory_name(MainFilePath,MainFileDir),
    !.

create_str_path(OtherPath, [MainFileDir|OtherPath]):-!,
    get_value(yaml_file,MainFilePath),
    file_directory_name(MainFilePath,MainFileDir).



