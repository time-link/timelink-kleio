:-module(apiStructures,[
    structures/5,
    structures_get/3
]).

/** <module> API operations for Structure files

Retrieve structure (.str, .yaml, .srpt) files and their metadata.
Supports resolving structures associated with Kleio source files.
*/

:-use_module(library(http/json)).
:-use_module(library(http/http_dispatch)).
:-use_module(library(option)).

:-use_module(restServer).
:-use_module(kleioFiles).
:-use_module(tokens).
:-use_module(logging).
:-use_module(apiTranslations).  % for get_stru_for_file/3

%! structures(+Method, +Path, +Mode, +Id, +Params) is det.
%
% ### Method = get or json structures_get
%
% Returns structure file information.
%
% If Params contains kleio(KleioPath), returns the structure associated
% with the Kleio file at KleioPath (using get_stru_for_file/3 logic).
%
% Otherwise, if Path is a file, returns the structure file content/info.
% If Path is a directory, returns list of structure files (.str, .yaml, .srpt).
%
% Parameters:
% $ kleio: path to a Kleio source file to find its associated structure
% $ recurse: if 'yes', recurse into subdirectories (default: no)

structures(get, Path, Mode, Id, Params) :-
    option(token_info(TokenInfo), Params),
    option(token(Token), Params),
    (tokens:is_api_allowed(Token, files) ->
        true
    ;
        (
        option(request(Request), Params, []),
        option(path_info(Url), Request, structures_get),
        throw(http_reply(forbidden(Url), ['Request-id'(Id)]))
        )
    ),
    % Check if kleio parameter is provided for structure resolution
    (option(kleio(KleioPath), Params) ->
        % Resolve structure for specific Kleio file
        kleio_resolve_source_file(KleioPath, AbsKleioPath, TokenInfo),
        (exists_file(AbsKleioPath) ->
            kleiofiles:kleio_default_stru(DefaultStru),
            get_stru_for_file(AbsKleioPath, DefaultStru, StruFile),
            structures_get_results(Mode, Id, [kleio(KleioPath), structure(StruFile)])
        ;
            throw(http_reply(not_found(KleioPath), ['Request-id'(Id)]))
        )
    ;
        % Regular path-based structure retrieval
        kleio_resolve_structure_file(Path, AbsPath, TokenInfo),
        structures_abs_get(Mode, Id, [path(Path), absolute_path(AbsPath) | Params], Results),
        structures_get_results(Mode, Id, Results)
    ), !.

structures(get, Path, _Mode, Id, _) :-
    throw(http_reply(not_found(Path), ['Request-id'(Id)])).

%! structures_abs_get(+Mode, +Id, +Params, -Results) is det.
%
% Core logic for structure retrieval.
% If absolute_path is a file, return file info.
% If absolute_path is a directory, return list of structure files.

structures_abs_get(__Mode, Id, Params, Results) :-
    option(absolute_path(AbsPath), Params),
    option(path(RelPath), Params),
    (exists_file(AbsPath) ->
        % Single structure file
        file_attributes(AbsPath, Attrs),
        Results = [file(Attrs)]
    ; exists_directory(AbsPath) ->
        % Directory - list structure files
        option(recurse(RecursePar), Params, no),
        (RecursePar = 'yes' -> Recurse = true; Recurse = false),
        find_structure_files(AbsPath, Recurse, Files),
        Results = [directory(RelPath), files(Files)]
    ;
        throw(http_reply(not_found(RelPath), ['Request-id'(Id)]))
    ).

%! find_structure_files(+Dir, +Recurse, -Files) is det.
%
% Find all structure files (.str, .yaml, .srpt) in Dir.
% If Recurse is true, recurse into subdirectories.

find_structure_files(Dir, false, Files) :-
    !,
    directory_files(Dir, Entries),
    include(is_structure_file, Entries, StructEntries),
    maplist(add_dir_path(Dir), StructEntries, Files).

find_structure_files(Dir, true, Files) :-
    find_structure_files_recursive(Dir, Files).

%! is_structure_file(+File) is semidet.
%
% True if File has a structure file extension (.str, .yaml, .srpt).

is_structure_file(File) :-
    file_name_extension(_, Ext, File),
    member(Ext, [str, yaml, srpt]).

%! add_dir_path(+Dir, +File, -Path) is det.
%
% Concatenate Dir and File to form Path.

add_dir_path(Dir, File, Path) :-
    atomic_list_concat([Dir, '/', File], Path).

%! find_structure_files_recursive(+Dir, -Files) is det.
%
% Recursively find all structure files in Dir and its subdirectories.

find_structure_files_recursive(Dir, AllFiles) :-
    directory_files(Dir, Entries),
    include(is_structure_file, Entries, StructEntries),
    maplist(add_dir_path(Dir), StructEntries, StructFiles),
    % Find subdirectories
    include(is_directory_entry(Dir), Entries, SubDirs),
    maplist(find_structure_files_recursive, SubDirs, SubDirFilesList),
    flatten([StructFiles | SubDirFilesList], AllFiles).

%! is_directory_entry(+ParentDir, +Entry) is semidet.
%
% True if Entry is a subdirectory (not . or ..).

is_directory_entry(ParentDir, Entry) :-
    Entry \= '.',
    Entry \= '..',
    atomic_list_concat([ParentDir, '/', Entry], FullPath),
    exists_directory(FullPath).

%! structures_get(+Mode, +Id, +Params) is det.
%
% JSON-RPC entry point for structures_get method.

structures_get(json, Id, Params) :-
    option(path(Path), Params, ''),
    structures(get, Path, json, Id, Params).

%! structures_get_results(+Mode, +Id, +Results) is det.
%
% Output results in appropriate format for REST or JSON.

structures_get_results(rest, Id, Results) :-
    print_content_type(rest),
    format('OK~nId:~w~n~n', [Id]),
    print_option_list(Results).

structures_get_results(json, Id, Results) :-
    Id \= null,
    convert_to_dict(Results, Dict),
    Return = json([
        jsonrpc='2.0',
        result=Dict,
        id=Id
    ]),
    current_output(Out),
    print_content_type(json),
    json_write(Out, Return).

%! convert_to_dict(+Results, -Dict) is det.
%
% Convert results list to JSON dictionary format.

convert_to_dict([kleio(K), structure(S)], Dict) :-
    !,
    Dict = json([kleio=K, structure=S]).
convert_to_dict([directory(D), files(F)], Dict) :-
    !,
    Dict = json([directory=D, files=F]).
convert_to_dict([file(Attrs)], Dict) :-
    !,
    Dict = json([file=json(Attrs)]).
convert_to_dict(Results, json(Results)).
