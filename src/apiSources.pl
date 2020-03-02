:-module(apiSources,[
    sources/5,
    sources_get/3,
    sources_delete/3,
    sources_copy/3,
    sources_move/3,
    sources_in_dir/3]).
/** <module> Api operations dealing with entity Sources
  
Download, upload, fetch and list source files.

*/ 

:-use_module(library(http/json)).
:-use_module(library(http/http_dispatch)).
:-use_module(library(http/http_header)).
:-use_module(library(http/http_client)).

:-use_module(apiTranslations).

:-use_module(restServer).
:-use_module(logging).
:-use_module(kleioFiles).
:-use_module(tokens).
:-use_module(threadSupport).
:-use_module(persistence).

%! sources(+Method,+Object,+Mode,+Id,+Params) is det.
%
% ### Method = get or json sources_get
%
% if Params contains path(Path) and Path is a file, 
%  returns the text of the file, resolving the path
% relative the user sources directory. If json output was request with header
% Accept: application/json then the link to download the file is returned.
% 
% If Path is a directory returns the list of cli and kleio files
% in the directory. 
%
% #### Get URls for retrieving the files
%
% If path is a directory and the optional parameter url=yes is present
% then a list of links to retrieve the files is returned.
% 
%
% #### Recurse down the directory tree
%
% If path is a directory and the optional parameter recurse=yes is present then the directory 
% tree at path will be examined. If no recurse=yes then just the first level will be retrieved.
%
% NOTE: swi returns a file by throwing a http_reply(file(Mime,Path),Headers) exception.
% Then swi server code catches this and returns the file.
% So the exception must propagate to the top level, 
% otherwise does not work. See return_error/2.
%
%
% ### Method = delete or json sources_delete
%
% if Params contains path(Path) and Path is a file, 
%  delete the file, resolving the path
% relative the user sources directory, and all files produced by the translation of this file. 
% 
% If Path is a directory deletes all the files in the directory. 
% If parameter result=yes recurse in subdirectories
% 
% If a file is being processed or is queued for processing then it is not deleted.
% Returns list of files deleted.
%
% ### Method = post multipart: upload (no json equivalent)
%
% If method is post and and the content is multipart encoded with a file file 
%    then the file is uplodaded to the path. File must not exist.
% 
% ### Method = put multipart: update (no json equivalent)
%
% If method is put and and the content is multipart encoded with a file file 
%    and the path correspons to an existing file then the file is uplodaded 
%    and replaces the existing file.
%
% ### Method = post and param contains origin=Path to existing file json sources_copy
%
% Post with origin param corresponds to a copy
%
% ### Method = put and param contains origin=Path to existing file json sources_move
%
% Put with origin param corresponds to a move operation.
%
%
sources(get,Path,Mode,Id,Params):-
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (is_api_allowed(Token,files) -> 
        true
    ;
        (
        option(request(Request),Params,[]),
        option(path_info(Url),Request, sources_get),   
        throw(http_reply(forbidden(Url),['Request-id'(Id)])) 
        ) 
    ),
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    logging:log_debug('API sources Absolute: ~w',[AbsPath]),
    sources_abs_get(Mode,Id,[path(Path),absolute_path(AbsPath)|Params],Results),
    sources_get_results(Mode,Id,_,Results),!.

sources(get,Path,_Mode,Id,_):-
    throw(http_reply(not_found(Path),['Request-id'(Id)])).

sources(delete,Path,Mode,Id,Params):-
    option(token(Token),Params),
    (is_api_allowed(Token,delete) -> 
        true
    ;
        (
        option(path_info(Url),Params, sources_delete),   
        throw(http_reply(forbidden(Url),['Request-id'(Id)])) 
        ) 
    ),
    option(token_info(TokenInfo),Params),
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    path_type(AbsPath,Type),
    delete_source(Path,Type,Mode,Id,Params,Results),
    sources_delete_results(Mode, Id, [type(Type)],Results).

sources(Method,Path,Mode,Id,Params):-
    (Method=put; Method=post),
    memberchk(file=file(_, _), Params), % check if uploaded file
    option(token(Token),Params),
    (is_api_allowed(Token,upload) -> 
        true
    ;
        (
        option(path_info(Url),Params, sources_upload),   
        throw(http_reply(forbidden(Url),['Request-id'(Id)])) 
        ) 
    ),    
    bagof(Result,    
        FileName^Saved^(member(file=file(FileName,Saved),Params),
        source_upload(Method,Path,Id,[file(FileName),saved(Saved),id(Id)| Params],Result)),
        Results),
    %TODO: should return 201 when file created
    default_results(Mode,Id,_,Results),!.

% sources POST to copy file
sources(post,DestPath,Mode,Id,Params):-
    option(origin(FilePath), Params),
    option(token(Token),Params),
    (is_api_allowed(Token,upload) -> 
        true
    ;
        (
        option(path_info(Url),Params, sources_upload),   
        throw(http_reply(forbidden(Url),['Request-id'(Id)])) 
        ) 
    ), 
    source_copy(FilePath,DestPath,[id(Id)|Params],Result),
    %TODO: should return 201 when file created
    default_results(Mode,Id,_,[Result]),!. 

% sources PUT to move file
sources(put,DestPath,Mode,Id,Params):-
    option(origin(FilePath), Params),
    option(token(Token),Params),
    (is_api_allowed(Token,upload) -> 
        true
    ;
        (
        option(path_info(Url),Params, sources_upload),   
        throw(http_reply(forbidden(Url),['Request-id'(Id)])) 
        ) 
    ), 
    source_move(FilePath,DestPath,[id(Id)|Params],Result),
    default_results(Mode,Id,_,[Result]),!. 
        
sources(Method,_,_,Id,_):-
    (Method=put; Method=post),
    throw(http_reply(bad_request(bad_file_upload),['Request-id'(Id)],[id(Id)])).

%% sources_get(+Mode,+Id,+Params) is det.
%
% This is the entry point for json-rpc.
%
sources_get(json,Id,Params):-
    option(path(Path),Params,''),
    sources(get,Path,json,Id,Params).

%% sources_delete(+Mode,+Id,+Params) is det.
%
% This is the entry point for json-rpc.
%
sources_delete(json,Id,Params):-
    option(path(Path),Params,''),
    sources(delete,Path,json,Id,Params).

%% sources_copy(+Mode,+Id,+Params) is det.
%
% This is the entry point for json-rpc.
%
sources_copy(json,Id,Params):-
    option(path(Path),Params,''),
    sources(post,Path,json,Id,Params).

%% sources_move(+Mode,+Id,+Params) is det.
%
% This is the entry point for json-rpc.
%
sources_move(json,Id,Params):-
    option(path(Path),Params,''),
    sources(put,Path,json,Id,Params).


sources_abs_get(rest,Id,Params,ignore):-
    option(absolute_path(AbsPath),Params),
    option(path(Path),Params),
    exists_file(AbsPath),
    kleio_mime_type(AbsPath,Mime),
    httpd_wrapper:http_current_request(Request),
    http_reply_file(AbsPath,[unsafe(true),mime_type(Mime),headers(['Request-id'(Id),location(Path)])],Request).

% for json we return a link to download the file. The link is relative to the current server
sources_abs_get(json,_Id,Params,[URL]):-
    option(absolute_path(AbsPath),Params),
    exists_file(AbsPath),
    option(path(Path),Params,''),
    make_rest_url(Path,'sources/',URL).

% for directory listing we return the same format and later sources_get_results/4 will check if json or not required.
sources_abs_get(_Mode, _Id,Params,Results):-
    option(absolute_path(AbsPath),Params),
    exists_directory(AbsPath),
    option(path(Path),Params,''),
    sources_in_dir(Path,Params,Results).

%% sources_get_results(+RequestType,+Id,+Params,+Results) is det.
% Output the result of the sources_get api call.
%
sources_get_results(rest,_,_Params,ignore):-!.
sources_get_results(rest,Id,_Params,Results):-
    is_list(Results),
    default_results(rest,Id,_,Results),!.
    
sources_get_results(json,null,_,_):-!. % "notification" in jsonrpc spec. Id = null
sources_get_results(json,Id,_Params,URLS):- % return link to download file
    Id \= null,
    default_results(json,Id,_,URLS).


%% sources_delete_results(+RequestType,+Id,+Params,+Results) is det.
% Output the result of the sources_get api call.
%
sources_delete_results(Mode,Id,_,Results):-
    default_results(Mode,Id,_,Results).
    
print_source((File,TCode,QCode,PCode)):-
    format('~w~w~w ~w~n',[TCode,QCode,PCode,File]).
    
%! sources_in_dir(+Dir,+Params,-Result) is det.
% 
% Lists sources under Dir with extensions cli or kleio. Uses token info (must be in Params) 
% to determine the source home directory of the user.
% For translation status use translations_get or status_get
sources_in_dir(Dir,Params,Results):-
    option(token_info(TokenInfo),Params),
    kleio_resolve_source_file(Dir,AbsDir,TokenInfo),
    logging:log_debug('API sources Absolute: ~w',[AbsDir]),
    option(recurse(R),Params,no),
    (R = no ->
        (
        atomic_list_concat([AbsDir,'/{*.cli,*.kleio}'],Pattern),
        expand_file_name(Pattern, CKFiles)
        )
    ;
        (
        find_files_with_extension(AbsDir,cli,_,CFiles),
        find_files_with_extension(AbsDir,kleio,_,KFiles),
        append(CFiles,KFiles,CKFiles)
        )
    ),
    sort(CKFiles,Files),
    kleio_resolve_source_list(RelFiles,Files,TokenInfo),
    ( option(url(yes),Params) ->
        (bagof(URL,P^(member(P,RelFiles),make_rest_url(P,'sources/',URL)),Results);Results=[])
        ;
        Results=RelFiles)
    .

%% delete_source(+Path,+Type,+Mode,+Id,+Params) is det.
%
% If Type=file delete source at Path (resolving Path to absolute) . 
% If Type = directory, delete all sources in Path . 
% All files derivated from the translation process are also
% deleted.
% 
delete_source(Path,directory,_Mode,_Id,Params,_Results):-
    sources_in_dir(Path,Params,Result),
    set_prop(kleio,delete,[]),
    option(token_info(TokenInfo),Params),
    member((File,TCode,PCode, QCode),Result),
    log_debug('delete file candidate:~w~n',[File]),
    TCode \= 'D', PCode \='P', QCode \='Q',
    log_debug('calling ~w~n',[kleio_file_delete(File)]),
    kleio_resolve_source_file(File,AbsFile,TokenInfo),
    kleio_file_delete(AbsFile),
    add_to_prop(kleio,delete,File),
    fail.

delete_source(Path,file,_Mode,_Id,Params,_Results):-
    set_prop(kleio,delete,[]),
    option(token_info(TokenInfo),Params),
    log_debug('delete file candidate:~w~n',[Path]),
    kleio_resolve_source_file(Path,AbsFile,TokenInfo),
    log_debug('calling ~w~n',[kleio_file_delete(Path)]),
    kleio_file_delete(AbsFile),
    add_to_prop(kleio,delete,Path),
    fail.

delete_source(Path,notfound,_Mode,Id,_Params,_Results):-
    throw(http_reply(not_found(Path),['Request-id'(Id)])).

delete_source(_Path,_Type,_Mode,_Id,_Params,Results):-
    get_prop(kleio,delete,Results).



%% source_upload(+Method,+FilePath,+Id,+Params,-Results) is det.
%
%  
% The rule is:
% * if method is post and the file exists throws error
% * if method is put and the file does not exist throws error
% Uploads a file. From http://www.swi-prolog.org/howto/http/FileUpload.html
% How does file upload works in html see: https://tools.ietf.org/html/rfc1867
% And a discussion in 
source_upload(Method,FilePath, Id,Params,DestFile) :-
    option(file(FileName),Params),
    option(saved(Saved),Params),
    option(token_info(TokenInfo),Params),
    option(id(Id),Params),
    atomic_list_concat([FilePath,FileName],'/',DestFile),
    kleio_resolve_source_file(DestFile,AbsFile,TokenInfo),
    (exists_file(AbsFile) -> Exists=true;Exists=false),
    ((Exists=true, Method = post) -> 
        throw(http_reply(bad_request(destination_file_exists(DestFile)),['Request-id'(Id)],[id(Id),file(DestFile)]))
        ;
        ((Exists=false,Method = put) -> 
            throw(http_reply(not_found(DestFile),['Request-id'(Id)],[id(Id),file(DestFile)]))
            ;
            true)   
    ),
    directory_file_path(Directory, _, AbsFile),
    (exists_directory(Directory) -> true
        ; throw(http_reply(bad_request(directory_not_exists(Directory)),['Request-id'(Id)],[id(Id),directory(Directory)]))),
    copy_file(Saved,AbsFile).

%% source_copy(+Origin,+Destination,+Params,+Result) is det.
%
% Copies file from Origin to Destination. If sucessful Result contains path to new file.
% Destination cannot exist. If needed delete destination first.
%
% If Destination does not include file name (no extension) then file name from Origin is
% appended to Destination.
%
% TODO: should it copy the org file too?
%
source_copy(Origin,Destination, Params, DestFile):-
    option(token_info(TokenInfo),Params),
    option(id(Id),Params,null),
    kleio_resolve_source_file(Origin,AbsOriginFile,TokenInfo),
    (exists_file(AbsOriginFile) -> 
        true
        ;
        throw(http_reply(not_found(Origin),['Request-id'(Id)],[id(Id),file(Origin)]))
    ),
    get_destination_path(Origin,Destination,DestFile),
    kleio_resolve_source_file(DestFile,AbsDestFile,TokenInfo),
    (exists_file(AbsDestFile) -> 
        throw(http_reply(bad_request(destination_file_exists(DestFile)),['Request-id'(Id)],[id(Id),file(DestFile)]))
    ;
        true),
    directory_file_path(Directory,_,AbsDestFile), % check if destination exists
    (exists_directory(Directory) -> true
    ; throw(http_reply(bad_request(directory_not_exists(Destination)),['Request-id'(Id)],[id(Id),directory(Destination)]))),
    copy_file(AbsOriginFile,AbsDestFile).

%% source_move(+Origin,+Destination,+Params,+Result) is det.
%
% Moves file from Origin to Destination. If sucessful Result contains path to new location of file.
% Destination cannot exist. If needed delete destination first.
% Origin is deleted along with all derived artifacts.
% TODO: should it move the org file too?
%
source_move(Origin,Destination, Params, DestFile):-
    option(token_info(TokenInfo),Params),
    option(id(Id),Params,null),
    get_destination_path(Origin,Destination,DestFile),
    kleio_resolve_source_file(Origin,AbsOriginFile,TokenInfo),
    (exists_file(AbsOriginFile) -> 
        true
        ;
        throw(http_reply(not_found(Origin),['Request-id'(Id)],[id(Id),file(Origin)]))
    ),
    kleio_resolve_source_file(DestFile,AbsDestFile,TokenInfo),
    (exists_file(AbsDestFile) -> 
        throw(http_reply(bad_request(destination_file_exists(DestFile)),['Request-id'(Id)],[id(Id),file(DestFile)]))
    ;
        true),
    directory_file_path(Directory,_,AbsDestFile), % check if destination exists
    (exists_directory(Directory) -> true
    ; throw(http_reply(bad_request(directory_not_exists(Destination)),['Request-id'(Id)],[id(Id),directory(Destination)]))),
    copy_file(AbsOriginFile,AbsDestFile),
    kleio_file_delete(AbsOriginFile).
    
    %% get_destination_path(+Origin,+Destination,?Path) is det.
    %
    %  If Destination has an extension, it must denote a file path, so Path=Destination
    %  If Destination does not have an extension then it must be a directory and Path=Destination +
    %   filename of Origin.
    %
    get_destination_path(Origin,Destination,Path):-
        file_name_extension(Destination, '', Destination),% destination is a dir
        directory_file_path(_BaseDir,Filename,Origin),% get the filename of the origin
        atomic_list_concat([Destination,Filename],'/',Path),!. % append to destination
    get_destination_path(_Origin,Destination,Destination):-!.
    

        