:-module(yamlSupport, [
    stru_yaml/1,
    normalize_str_path/2
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
% File='/Users/jrc/develop/timelink-kleio/tests/kleio-home/structures/yaml/sample-str.yaml', stru_yaml(File), show_stru).
:-use_module(library(yaml)).
:-use_module(library(pprint)).
:-use_module(persistence).
:-use_module(kleioFiles).
:-use_module(reports).
:-use_module(struCode).
:-use_module(struSyntax). % need to check vocabulary
:-use_module(dataSyntax).
:-use_module(errors).


stru_yaml(F):-
    atom_string(Filename,F), % normalize name as atom
    put_value(stru_file,Filename),
    put_value(stru_files_read,[]),
    set_prop(Filename,type,str_yaml),
    push(stru_files_stack, ''),
    new_yaml_str(Filename,_).

new_yaml_str(Filename,Data):-
    report([format('~nProcessing YAML structure (schema):~n   ~w~n~n',[Filename])]),
    struCode:initStru(Filename),
    process_str_command(database,_{name:kleio, first:kleio, identification:no}),
    read_yaml_str(Filename,Data),
    struCode:closeStru(Filename),
    report([perror_count]),
    report([writeln('Structure processing finished.')]).

% this read a yaml file and processes the configuration
read_yaml_str(Filename,Data):-
    put_value(yaml_file,Filename),
    get_value(stru_files_read,ReadFiles),
    get_value(stru_files_stack,Stack),
    (member(Filename, ReadFiles) -> 
        errors:warning_out(
            ['WARNING: Ignoring previously processed file'],
        [file(Filename),file_type(stru)])
    ;
        (
        length(Stack,Len),
        Ident is Len * 3,
        report([format('~n~*c >> Reading: ~w~n',[Ident,32,Filename])]),
        yaml_read(Filename,Data),
        report([format('~*c << Read ~w~n',[Ident,32,Filename])]),
        push(stru_files_stack, Filename),
        add_value(stru_files_read,Filename),
        inspect_yaml_str(Data),
        pop(stru_files_stack,_)
        )
    ),
    !.

% loop through the yaml structure
inspect_yaml_str(YamlList):-
    get_value(yaml_file,Filename),
    get_value(stru_files_stack,Stack),
    length(Stack,Len),
    Ident is Len * 3,
    report([format('~*c ++ Inspecting: ~w~n',[Ident,32,Filename])]),
    member(YamlCMD, YamlList),
    inspect_yaml_str_cmd(YamlCMD),
    fail.

inspect_yaml_str(_):-!,
    get_value(yaml_file,Filename),
    get_value(stru_files_stack,Stack),
    length(Stack,Len),
    Ident is Len * 3,
    report([format('~*c -- Inspected ~w~n',[Ident,32,Filename])]),
    !.

% process each command in the yaml structure
% first extract the parameters
inspect_yaml_str_cmd(YamlTerm):-
    Params = YamlTerm.Command,
    % format('Command: ~w~n',[Command]),
    % format('  Pars: ~w~n', Params),
    process_str_command(Command,Params),
    nl.

% file command starts a file of definitions
process_str_command(file,Pars):-!,
    put_value(current_command,file),
    is_dict(Pars),
    bagof(Par=Value, Value = Pars.Par, ParList),
    % writeln('Bagof Process element with pars '-ParList),
    option(name(Name),ParList,name-missing),!,
    get_value(yaml_file,Filename),
    get_value(stru_files_stack,Stack),
    length(Stack,Len),
    Ident is Len  * 3,
    report([format('~n~*c == Structure name: ~w~n~*c      from ~w ~n',[Ident,32,Name,Ident,32,Filename])]),
    (get_value(stru_file,Filename); Filename = Name),
    option(description(Desc),ParList,none),
    set_prop(Filename,description,Desc),
    !.
process_str_command(file,_):-!.

% include command
process_str_command(include,Par):-
    put_value(current_command,include),
    get_value(yaml_file,Filename),
    atomic(Par),!,
    get_value(stru_files_stack,Stack),
    length(Stack,Len),
    Ident is Len * 3,
    report([format('~*c >> Including file: ~w~n~*c      from ~w ~n',[Ident,32,Par,Ident,32,Filename])]),
    include_yaml_str(Par,_),
    report([format('~*c << Included file: ~w~n~*c      from ~w ~n',[Ident,32,Par,Ident,32,Filename])]),
    !.

% process the command, bridge to struCode
process_str_command(Command, Params):-
    % the InternalCommand is the original Kleio latin command
    % in latter versions could be in english
    % this legacy code is to keep the old commands working
    struSyntax:is_kw(Command,InternalCommand),
    struSyntax:command(InternalCommand,ok),!,
    struCode:init_command(InternalCommand),
    process_str_params(InternalCommand, Params),
    struCode:close_command(InternalCommand,_).

process_str_command(name, Params):-
    put_value(current_command,name),
    get_value(stru_file,Filename),
    error_out(['*** "name" command out of context, should be in file command: ',[Params]],[file(Filename)]),!.

process_str_command(description, Params):-
    put_value(current_command,description),
    get_value(stru_file,Filename),
    error_out(['*** "description" command out of context, should be in file command: ',[Params]],[file(Filename)]),!.

% bad command
process_str_command(Command, Params):-
    put_value(current_command,Command),
    get_value(stru_file,Filename),

    error_out(['*** Unknow command in YAML file. Check spelling. ',[Command, ' '|Params]],[file(Filename)]),!.

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
