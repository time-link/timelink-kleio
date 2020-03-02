 :-module(apiGit,[
    versions/5,
    versions_get_global_status/3,
    versions_get_remotes_branches/3
    ]). 

:- use_module(gitUtilities).
:- use_module(kleioFiles).
:- use_module(tokens).
:- use_module(logging).
:- use_module(restServer).

%% versions(+HttpMethod,+OutputMode,+Id,+Params) is det.
%
% returns a global status of the repository
% See gitUtilities:git_global_status/3 for details.
%

versions(get,PseudoPath,Mode,Id,Params):-
    string_concat("remotes/branches",Path,PseudoPath),
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (is_api_allowed(Token,files) -> 
        true
    ;
        throw(http_reply(method_not_allowed(get,Path),['Request-id'(Id)]))  
    ), 
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    (exists_directory(AbsPath) ->
        git_remotes_branches_info(AbsPath,Branches,[])
    ;
        throw(http_reply(not_found(Path),['Request-id'(Id)]))
    ),    
    versions_results(Mode,Id,Branches).

versions(get,PseudoPath,Mode,Id,Params):-
    string_concat("status/global",Path,PseudoPath),
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (is_api_allowed(Token,files) -> 
        true
    ;
        throw(http_reply(method_not_allowed(get,Path),['Request-id'(Id)]))  
    ),
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    (exists_directory(AbsPath) ->
        git_global_status(AbsPath,GitStatus,Params) % we pass the params as options see git_global_status for valid options
    ;
        throw(http_reply(not_found(Path),['Request-id'(Id)]))
    ), 
    with_output_to(string(StatusString), print_git_global_status(GitStatus)),
    % TODO: should make directory and git_root relative before returning results.
    versions_results(Mode,Id,GitStatus.put(report,StatusString)).

%% json-rpc entry points for version(get,...)
versions_get_global_status(json,Id,Params):-
    option(path(Path),Params,''),
    string_concat("status/global/",Path,PseudoPath),
    versions(get,PseudoPath,json,Id,Params).
versions_get_remotes_branches(json,Id,Params):-
    option(path(Path),Params,''),
    string_concat("remotes/branches/",Path,PseudoPath),
    versions(get,PseudoPath,json,Id,Params).

versions_results(json,Id,Result):-
    default_results(json,Id,[],Result). %.put(report,"No report until further notice")
versions_results(rest,Id,GitStatus):-   
        is_dict(GitStatus,_),
        default_results(rest,Id,[],[GitStatus.report]).   
versions_results(rest,Id,Result):-   
    default_results(rest,Id,[],Result).


