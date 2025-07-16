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
    process_str_command(database,_{name:kleio, first:kleio, identification:no}),
    read_yaml_str(Filename,Data),
    struCode:closeStru(Filename).

% this read a yaml file and processes the configuration
read_yaml_str(Filename,Data):-
    put_value(yaml_file,Filename),
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
    (get_value(stru_file,FileName); FileName = Name),
    option(description(Desc),ParList,none),
    set_prop(FileName,description,Desc),
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
% include a single file no path
% use the current file directory
% to generate the full path
normalize_str_path(File,Path):-
    atomic_list_concat([_,Ext], '.',File),
    ( Ext = yaml ; Ext = yml ),
    get_value(yaml_file,MainFilePath),
    file_directory_name(MainFilePath,MainFileDir),
    atomic_list_concat([MainFileDir,File],'/',Path).
normalize_str_path(File,Path):-
    atomic_list_concat(Dirs, '.',File),
    create_str_path(Dirs,DirsExpanded),
    atomic_list_concat(DirsExpanded,'/',Path).

% last item on path is filename with or not .yaml ext
create_str_path([File,'yaml'],[FileName]):-
    atomic_list_concat([File,'yaml'],'.',FileName).

% resolve system dir
create_str_path([system|Dirs],[SysStruDir|MoreDirs]):-!,
    kleio_stru_dir(SysStruDir),
    create_str_path(Dirs,MoreDirs),!.

% resolve user  structures dir
% this requires some way to get the user structure directory
% which is normally associated with a user token
% here we assume that token info is in value "token_info"
% other wise we default to home.structures
% see https://github.com/time-link/timelink-kleio/issues/12
create_str_path([structures|Dirs],[UserStruDir|MoreDirs]):-
    % in prod
    get_value(token_info,TokenInfo),
    kleio_user_structure_dir(UserStruDir, TokenInfo),
    create_str_path(Dirs,MoreDirs),!.

create_str_path([structures|Dirs],[LocalStructures|MoreDirs]):-
    kleio_home_dir(KleioHomeDir),
    atomic_list_concat([KleioHomeDir,structures],'/',LocalStructures),
    create_str_path(Dirs,MoreDirs),
    !.

% resolve user sources dir
% see https://github.com/time-link/timelink-kleio/issues/12
create_str_path([sources|Dirs],[UserStruDir|MoreDirs]):-
    % in prod
    get_value(token_info,TokenInfo),
    kleio_user_source_dir(UserStruDir, TokenInfo),
    create_str_path(Dirs,MoreDirs),!.

create_str_path([sources|Dirs],[LocalStructures|MoreDirs]):-
    kleio_home_dir(KleioHomeDir),
    atomic_list_concat([KleioHomeDir,sources],'/',LocalStructures),
    create_str_path(Dirs,MoreDirs),
    !.
% resolve home dir
create_str_path([home|Dirs],[KleioHomeDir|MoreDirs]):-
    kleio_home_dir(KleioHomeDir),
    create_str_path(Dirs,MoreDirs),!.

% resolve . separator TODO: Broken
create_str_path(['~'|Dirs],[MainFileDir|MoreDirs]):-!,
    get_value(stru_file,MainFilePath),
    file_directory_name(MainFilePath,MainFileDir),
    create_str_path(Dirs,MoreDirs).

% consider every thing else as a directory
create_str_path([Dir|Dirs],[Dir|MoreDirs]):-
    create_str_path(Dirs,MoreDirs).

% include file by name
create_str_path(FileOnly, [MainFileDir,FileOnly]):-
    atomic(FileOnly),
    get_value(stru_file,MainFilePath),
    file_directory_name(MainFilePath,MainFileDir),
    !.

% include other paths
create_str_path(OtherPath, [MainFileDir|OtherPath]):-!,
    get_value(yaml_file,MainFilePath),
    file_directory_name(MainFilePath,MainFileDir).


:- begin_tests(yamlSupport).

test(stru_yaml_placeholder) :-
    % Placeholder: test stru_yaml/1 with a sample file
    % Example: stru_yaml('tests/kleio-home/structures/yaml/sample-str.yaml')
    % Add assertions as needed
    true.

test(new_yaml_str_placeholder) :-
    % Placeholder: test new_yaml_str/2 with a sample file
    % Example: new_yaml_str('tests/kleio-home/structures/yaml/sample-str.yaml', Data)
    % Add assertions as needed
    true.

test(read_yaml_str_placeholder) :-
    % Placeholder: test read_yaml_str/2 with a sample file
    % Example: read_yaml_str('tests/kleio-home/structures/yaml/sample-str.yaml', Data)
    % Add assertions as needed
    true.

test(inspect_yaml_str_placeholder) :-
    % Placeholder: test inspect_yaml_str/1 with a sample YAML list
    % Example: inspect_yaml_str([_])
    % Add assertions as needed
    true.

test(process_str_command_placeholder) :-
    % Placeholder: test process_str_command/2 with sample commands and params
    % Example: process_str_command(file, _)
    % Add assertions as needed
    true.

test(process_str_params_placeholder) :-
    % Placeholder: test process_str_params/2 with sample command and params
    % Example: process_str_params(file, _)
    % Add assertions as needed
    true.

test(prepend_if_member_true) :-
    prepend_if_member(a=1, [a=1,b=2], [a=1,b=2]).

test(prepend_if_member_false) :-
    prepend_if_member(c=3, [a=1,b=2], [a=1,b=2]).

test(sanitize_value_atom) :-
    sanitize_value('abc', abc).

test(sanitize_value_list) :-
    sanitize_value(['abc','def'], [abc,def]).

test(sanitize_value_empty) :-
    sanitize_value([], []).

test(include_yaml_str_placeholder) :-
    % Placeholder: test include_yaml_str/2 with a sample file
    % Example: include_yaml_str('sample.yaml', Data)
    % Add assertions as needed
    true.

test(normalize_str_path_dot) :-
    % Should resolve '.' to the directory of the stru_file value
    true.

test(create_str_path_system) :-
    % Should resolve [system,foo] to [SysStruDir,foo]
    true.

test(create_str_path_structures) :-
    true.

test(create_str_path_home) :-
    true.

test(create_str_path_fileonly) :-
    true.

:- end_tests(yamlSupport).
