:-module(yamlSupport, [
    stru_yaml/1
    ]).

/** <module> Processing YAML files
 *
 * Structuture files are now expected to be in YAML format.
 * This module contains predicates to process YAML files.
% https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/yaml.html%27)#yaml_read/2
*/

% example read mappings
% File='/Users/jrc/develop/timelink-kleio/tests/kleio-home/mappings/sample-mapping.yml', new_yaml_str(File,D),print_term(D,[]).
% example read str
% File='/Users/jrc/develop/timelink-kleio/tests/kleio-home/structures/yaml/sample-str.yaml',stru_yaml(File), show_stru).
:-use_module(library(yaml)).
:-use_module(library(pprint)).
:-use_module(persistence).
:-use_module(kleioFiles).
:-use_module(struCode).
:-use_module(struSyntax). % need to check vocabulary
:-use_module(dataSyntax).
:-use_module(errors).


stru_yaml(F):-
    atom_string(Filename,F), % normalize name as atom
    put_value(stru_file,Filename),
    set_prop(Filename,type,str_yaml),
    new_yaml_str(Filename,_).

new_yaml_str(Filename,Data):-
    struCode:initStru(Filename),
    read_yaml_str(Filename,Data),
    struCode:closeStru(Filename).

% this read a yaml file and processes the configuration
read_yaml_str(Filename,Data):-
    yaml_read(Filename,Data),
    inspect_yaml_str(Data),
    !.

% loop through the yaml structure
inspect_yaml_str(YamlList):-
    member(YamlCMD, YamlList),
    inspect_yaml_str_cmd(YamlCMD),
    fail,
    !.
inspect_yaml_str(_):-!.

% process each command in the yaml structure
% first extract the parameters
inspect_yaml_str_cmd(YamlTerm):-
    Params = YamlTerm.Command,
    format('Command: ~w~n',[Command]),
    format('  Pars: ~w~n', Params),
    process_str_command(Command,Params),
    nl.

% file command starts a file of definitions
process_str_command(file,Pars):-!,
    is_dict(Pars),
    bagof(Par=Value, Value = Pars.Par, ParList),
    % writeln('Bagof Process element with pars '-ParList),
    option(name(Name),ParList,name-missing),
    member(Par=Value,ParList),
    format('(~w)   ~w = ~w ~n', [Name,Par,Value]),
    fail.
process_str_command(file,_):-!.

% include command
process_str_command(include,Par):-
    atomic(Par),!,
    format('  Include file: ~w ~n',[Par]),
    include_yaml_str(Par,Data),
    inspect_yaml_str(Data),!.

% extends (synonym of include)

process_str_command(extend,Par):-
    process_str_command(include, Par),!.


% process the command, bridge to struCode
process_str_command(Command, Params):-
    % the InternalCommand is the original Kleio latin command
    % in latter versions could be in english
    % this legacy code is to keep the old commands working
    struSyntax:is_kw(Command,InternalCommand),!,
    struCode:init_command(InternalCommand),
    process_str_params(InternalCommand, Params),
    struCode:close_command(InternalCommand,_).


% bad command
process_str_command(Command, Params):-
    error_out(['*** Unknow command. Check spelling. ',[Command, ' '|Params]]),!.

process_str_params(InternalCommand, Params):-
    is_dict(Params),
    bagof(Par=Value, Value = Params.Par, ParList),
    % need to ensure that the fons command is processed after name
    prepend_if_member(source=_, ParList, NewParList),
    prepend_if_member(name=_, NewParList, NewParList2),
    member(Par=Value,NewParList2),
    process_str_param(InternalCommand, Par, Value),
    fail.
process_str_params(_, _):-!.

prepend_if_member(Term, List, [Term|Rest]) :-
    select(Term, List, Rest), !.
prepend_if_member(_, List, List).

process_str_param(Command, Par, Value):-
    struSyntax:is_kw(Par,IPar),!,   % for equivalence to latin forms
    sanitize_value(Value, SValue),
    struCode:execParam(Command, IPar, SValue).

% sanitize_value(+Value,-SValue) is det.
% Value: value from yaml str file
% SValue: Value with strings replace by atoms
sanitize_value([],[]):-!.
sanitize_value([V|Rest],[SV|SRest]):-
    sanitize_value(V,SV),!,
    sanitize_value(Rest,SRest).
sanitize_value(V,SR):-
    atomic(V),!,
    atom_string(SR,V).
sanitize_value(V,V):-!.

% including file
include_yaml_str(File,Data):-
    normalize_str_path(File,Path),
    absolute_file_name(Path,AbsPath),
    read_yaml_str(AbsPath,Data).

% include normalize the path
normalize_str_path(File,Path):-
    atomic_list_concat(Dirs, '/',File),
    create_str_path(Dirs,DirsExpanded),
    atomic_list_concat(DirsExpanded,'/',Path).

% resolve system dir
create_str_path([system|MoreDirs],[SysStruDir|MoreDirs]):-!,
    kleio_stru_dir(SysStruDir).

% resolve structures dir
create_str_path([structures|MoreDirs],[LocalStruDir|MoreDirs]):-!,
    % replace this with structures from user token
    kleio_user_structure_dir(LocalStruDir,[structures='tests/kleio-home/structures']).

% resolve . separator
create_str_path(['.'|MoreDirs],[MainFileDir|MoreDirs]):-!,
    get_value(stru_file,MainFilePath),
    file_directory_name(MainFilePath,MainFileDir).

% include file by name
create_str_path([FileOnly|[]], [MainFileDir,FileOnly]):-
    atomic(FileOnly),
    get_value(stru_file,MainFilePath),
    file_directory_name(MainFilePath,MainFileDir),
    !.

% include other paths
create_str_path(OtherPath, [MainFileDir|OtherPath]):-!,
    get_value(yaml_file,MainFilePath),
    file_directory_name(MainFilePath,MainFileDir).



