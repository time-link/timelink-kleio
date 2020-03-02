:-module(apiDirectories,[
    directories/5,
    directories_get/3,
    directories_delete/3,
    directories_create/3
    ]).
/** <module> Api operations dealing with Source Directories
  
List, create,copy and remove directories.

**/
:- use_module(restServer).
:- use_module(kleioFiles).
:- use_module(tokens).
:- use_module(logging).


directories(get,Path,Mode,Id,Params):-
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (is_api_allowed(Token,files) -> 
        true
    ;
        throw(http_reply(method_not_allowed(get,Path),['Request-id'(Id)]))  
    ),
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    option(recurse(RecursePar),Params,no),   
    (RecursePar = 'yes' -> Recurse = true;Recurse=no), 
    (exists_directory(AbsPath) ->
        (directory_subdirs(AbsPath,AbsDirs,Recurse),
        kleio_resolve_source_list(Dirs,AbsDirs,TokenInfo))
    ;
        throw(http_reply(not_found(Path),['Request-id'(Id)]))
    ), 
    diretories_results(Mode,Id,Dirs).

directories(delete,Path,Mode,Id,Params):-
    option(token(Token),Params),
    (is_api_allowed(Token,delete) -> 
        true
    ;
        throw(http_reply(method_not_allowed(delete,Path),['Request-id'(Id)]))  
    ),
    rmdir(Id,Path,Params,Result),
    diretories_results(Mode,Id,[Result]).

% POST with origin: copy dir
%
directories(post,Path,Mode,Id,Params):-
    option(origin(SourceDir),Params),!, % if param origin consider this a copy
    option(token(Token),Params),
    (is_api_allowed(Token,mkdir) -> 
        true
    ;
        throw(http_reply(method_not_allowed(delete,Path),['Request-id'(Id)]))  
    ),
    cpdir(Id,SourceDir,Path,Params,Result),
    %TODO: should return 201 when file created
    diretories_results(Mode,Id,[Result]).

directories(post,Path,Mode,Id,Params):-
    option(token(Token),Params),
    (is_api_allowed(Token,mkdir) -> 
        true
    ;
        throw(http_reply(method_not_allowed(delete,Path),['Request-id'(Id)]))  
    ),
    mkdir(Id,Path,Params,Result),
    %TODO: should return 201 when file created
    diretories_results(Mode,Id,[Result]).


directories_get(json,Id,Params):-
    option(path(Path),Params,''),
    directories(get,Path,json,Id,Params).

directories_delete(json,Id,Params):-
    option(path(Path),Params,''),
    directories(delete,Path,json,Id,Params).

directories_create(json,Id,Params):-
    option(path(Path),Params,''),
    directories(post,Path,json,Id,Params).

directories_copy(json,Id,Params):-
    option(origin(_SourceDir),Params), % if param origin consider this a copy
    option(path(Path),Params,''),
    directories(post,Path,json,Id,Params).

diretories_results(Mode,Id,Dirs):-
    default_results(Mode,Id,_,Dirs).
        
%! rmdir(+Id,+Path,+Params,-Results) is det.
%
% Removes a directory. Used by Json-RPC `rmdir` and REST DELETE /directories/PATH
%
% Params:
% * contents(yes/no) : if `yes`the directory is removed even if not empty. if `no` removal will fail if directory not empty. Beware of invisible files
% like those created by the MacOs finder or git.
%
rmdir(Id,Path,Params,Path):-
    option(token_info(TokenInfo),Params), % token info was put into Params by json_decode_command
    option(force(DeleteContents), Params, no),
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    (exists_directory(AbsPath) -> 
        true
        ; 
        throw(http_reply(not_found(Path),['Request-id'(Id)])) 
    ),
    (DeleteContents = yes -> 
        delete_directory_and_contents(AbsPath)
        ; 
        catch(delete_directory(AbsPath),Error,
            (log_error('delete_directory error:~w',[Error]),
            throw(error(Id,-32003,'Delete failed. Directory not empty?'-Path))))
    ).


mkdir(Id,Path,Params,Path):-
    option(token_info(TokenInfo),Params), 
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    (exists_directory(AbsPath) -> 
        throw(error(Id,-32005,'Directory already exists '-Path))
        ; 
        true
    ),
    catch(make_directory_path(AbsPath),Error,
        (log_error('make_directory_path:~w',[Error]),
        throw(error(Id,-32004,'Could not create directory. Check error log.'-Path)))).

cpdir(Id,Source,Dest,Params,Dest):-
        option(token_info(TokenInfo),Params), 
        kleio_resolve_source_file(Source,AbsSourcePath,TokenInfo),
            (exists_directory(AbsSourcePath) -> 
            true
            ; 
            throw(http_reply(not_found(Source),['Request-id'(Id)])) 
        ),
        kleio_resolve_source_file(Dest,AbsDestPath,TokenInfo),
        (exists_directory(AbsDestPath) -> 
            throw(error(Id,-32005,'Directory already exists '-Dest))
            ; 
            true
        ),
        catch(copy_directory(AbsSourcePath,AbsDestPath),Error,
            (log_error('copy_directory:~w',[Error]),
            throw(error(Id,-32004,'Could not copy directory. Check error log.')))).

% NOT used see directory_subdirs/3 bellow
directory_subdirs(AbsPath, Subdirs):-
    directory_files(AbsPath,Contents),
    setof(D,
        D^AbsSDir^(member(D,Contents),
        not(atom_prefix(D,'.')),
        atomic_list_concat([AbsPath,D],'/',AbsSDir),
        exists_directory(AbsSDir)
        ),
    Subdirs),!.
directory_subdirs(_, []):-!.

directory_subdirs(AbsPath,SubDirs,Recursive):-
    (setof(SubDir,
        SubDir^directory_member(AbsPath,SubDir,[recursive(Recursive),file_type(directory),hidden(false)]),
            SubDirs)
    ;
    SubDirs=[]),!.
    
