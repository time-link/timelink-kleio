:-module(apiIdentifications,[
    identifications/5,
    identifications_get/3
    ]).

:-use_module(library(http/http_dispatch)).

:-use_module(apiCommon).
:-use_module(kleioFiles).
:-use_module(tokens).
:-use_module(restServer).

/** <module> Api operations dealing with identifications files (mhk_identification*.json files)

Retrieve idenfication files
TODO we duplicate lots of code from apiSources, should generalize.

**/

identifications(get,Path,Mode,Id,Params):-
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (is_api_allowed(Token,files) -> %TODO special privilege for fetching identification files?
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
    identifications_abs_get(Mode,Id,[path(Path),absolute_path(AbsPath)|Params],Results),
    identifications_get_results(Mode,Id,_,Results),!.

identifications(get,Path,_Mode,Id,_):-
    throw(http_reply(not_found(Path),['Request-id'(Id)])).

identifications_get(json,Id,Params):-
    option(path(Path),Params,''),
    identifications(get,Path,json,Id,Params).


identifications_abs_get(rest,Id,Params,ignore):-
    option(absolute_path(AbsPath),Params),
    option(path(Path),Params),
    exists_file(AbsPath),
    kleio_mime_type(AbsPath,Mime),
    httpd_wrapper:http_current_request(Request),
    http_reply_file(AbsPath,[unsafe(true),mime_type(Mime),headers(['Request-id'(Id),location(Path)])],Request).

% for json we return a link to download the file. The link is relative to the current server
identifications_abs_get(json,_Id,Params,[URL]):-
    option(absolute_path(AbsPath),Params),
    exists_file(AbsPath),
    option(path(Path),Params,''),
    make_rest_url(Path,'sources/',URL).

% for directory listing we return the same format and later sources_get_results/4 will check if json or not required.
identifications_abs_get(_Mode, _Id,Params,Results):-
    option(absolute_path(AbsPath),Params),
    exists_directory(AbsPath),
    option(path(Path),Params,''),
    identifications_in_dir(Path,Params,Results).

%% identifications_get_results(+RequestType,+Id,+Params,+Results) is det.
% Output the result of the sources_get api call.
%
identifications_get_results(rest,_,_Params,ignore):-!.
identifications_get_results(rest,Id,_Params,Results):-
    is_list(Results),
    default_results(rest,Id,_,Results),!.
    
identifications_get_results(json,null,_,_):-!. % "notification" in jsonrpc spec. Id = null
identifications_get_results(json,Id,_Params,URLS):- % return link to download file
    Id \= null,
    default_results(json,Id,_,URLS).

%! identifications_in_dir(+Dir,+Params,-Result) is det.
% 
% Lists identifications files (mhk_identificatifications*) under Dir. Uses token info (must be in Params) 
% to determine the source home directory of the user.
identifications_in_dir(Dir,Params,Results):-
    option(token_info(TokenInfo),Params),
    kleio_resolve_source_file(Dir,AbsDir,TokenInfo),
    logging:log_debug('API id file Absolute: ~w',[AbsDir]),
    option(recurse(R),Params,no),
    (R = no ->
        (
        atomic_list_concat([AbsDir,'/mhk_identification*'],Pattern),
        expand_file_name(Pattern, CKFiles)
        )
    ;
        (
        find_files_by_pattern(AbsDir,'mhk_identification*',_,CKFiles)
        )
    ),
    sort(CKFiles,Files),
    kleio_resolve_source_list(RelFiles,Files,TokenInfo),
    ( option(url(yes),Params) ->
        (bagof(URL,P^(member(P,RelFiles),make_rest_url(P,'sources/',URL)),Results);Results=[])
        ;
        Results=RelFiles)
    .