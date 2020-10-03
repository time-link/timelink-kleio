 :-module(apiGit,[
    versions/5,
    versions_get_global_status/3,
    versions_get_remotes_branches/3,
    versions_pull/3,
    versions_push/3,
    versions_commit/3,
    versions_set_user_info/3,
    versions_get_user_info/3,
    versions_reset/3
    ]). 
/** <module> Access to git repositories 
  
Status, Fetch, Push, Pull

*/ 
:- use_module(gitUtilities).
:- use_module(kleioFiles).
:- use_module(tokens).
:- use_module(logging).
:- use_module(restServer).

%! versions(+HttpMethod,+OutputMode,+Id,+Params) is det.
%
% Interface to Git workflow auxiliary functions.
% Method GET retrieves information from git (status,diff,logs), without changing the working directory in any way, using pseudo-paths to define which information to get.
% Method PUT provides Add, Commit, Checkout, Fetch, Merge and Pull functionality, these commands can change the contents of the working directory
% Method POST provides Push functionality.
% See details in individual description of API calls.
% returns a global status of the repository
% See gitUtilities:git_global_status/3 for details.
%
%
versions(get,PseudoPath,Mode,Id,Params):-
    string_concat("remotes/branches",Path,PseudoPath),!,
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
    string_concat("status/global",Path,PseudoPath),!,
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

versions(get,PseudoPath,Mode,Id,Params):-
    string_concat("pull",Path,PseudoPath),!,
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (is_api_allowed(Token,files) -> 
        true
    ;
        throw(http_reply(method_not_allowed(put,Path),['Request-id'(Id)]))  
    ), 
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    git_pull(AbsPath,[lines(L),error(E),status(S)| Params]),    
    versions_results(Mode,Id,git{git_output:L,git_error:E,git_exit_status:S}).

versions(get,PseudoPath,Mode,Id,Params):-
    string_concat("user-info",Path,PseudoPath),!,
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (is_api_allowed(Token,files) -> 
        true
    ;
        throw(http_reply(method_not_allowed(put,Path),['Request-id'(Id)]))  
    ),    
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    git_user_info(AbsPath,Name,Email,Params),
    versions_results(Mode,Id,git{git_user_name:Name,git_user_email:Email}).

versions(put,PseudoPath,Mode,Id,Params):-
    string_concat("push",Path,PseudoPath),!,
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (is_api_allowed(Token,files) -> 
        true
    ;
        throw(http_reply(method_not_allowed(put,Path),['Request-id'(Id)]))  
    ), 
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    git_push(AbsPath,[lines(L),error(E),status(S)| Params]),    
    versions_results(Mode,Id,git{git_output:L,git_error:E,git_exit_status:S}).

versions(put,PseudoPath,Mode,Id,Params):-
    string_concat("commit",Path,PseudoPath),!,
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (is_api_allowed(Token,files) -> 
        true
    ;
        throw(http_reply(method_not_allowed(put,Path),['Request-id'(Id)]))  
    ), 
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    option(commit_files(CommitFiles),Params,""),
    option(add_files(AddFiles),Params,""),
    option(commit_message(CMessage),Params,""),
    git_commit(AbsPath,AddFiles,CommitFiles,CMessage,[lines(L),error(E),status(S)| Params]),    
    versions_results(Mode,Id,git{git_output:L,git_error:E,git_exit_status:S}).

versions(put,PseudoPath,Mode,Id,Params):-
    string_concat("set-user-info",Path,PseudoPath),!,
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (is_api_allowed(Token,files) -> 
        true
    ;
        throw(http_reply(method_not_allowed(put,Path),['Request-id'(Id)]))  
    ), 
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    option(user_name(Name),Params,'none'),
    option(user_email(Email),Params,'none'),
    git_set_user_info(AbsPath,Name,Email,[error(E),lines(L),status(S)|Params]),    
    versions_results(Mode,Id,git{git_output:L,git_error:E,git_exit_status:S}).

versions(delete,PseudoPath,Mode,Id,Params):-
    string_concat("reset",Path,PseudoPath),!,
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    option(reset_mode(RMode),Params,'--soft'),
    option(commit_ref(CRef),Params,'HEAD'),
    (is_api_allowed(Token,files) -> 
        true
    ;
        throw(http_reply(method_not_allowed(put,Path),['Request-id'(Id)]))  
    ), 
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    git_reset(AbsPath,RMode,CRef,[lines(L),error(E),status(S)| Params]),    
    versions_results(Mode,Id,git{git_output:L,git_error:E,git_exit_status:S}).

%% json-rpc entry points for versions(get,...)
versions_get_global_status(json,Id,Params):-
    option(path(Path),Params,''),
    string_concat("status/global/",Path,PseudoPath),
    versions(get,PseudoPath,json,Id,Params).
versions_get_remotes_branches(json,Id,Params):-
    option(path(Path),Params,''),
    string_concat("remotes/branches/",Path,PseudoPath),
    versions(get,PseudoPath,json,Id,Params).
versions_get_user_info(json,Id,Params):-
    option(path(Path),Params,''),
    string_concat("user-info/",Path,PseudoPath),
    versions(get,PseudoPath,json,Id,Params).
versions_pull(json,Id,Params):-
    option(path(Path),Params,''),
    string_concat("pull/",Path,PseudoPath),
    versions(get,PseudoPath,json,Id,Params).
versions_push(json,Id,Params):-
    option(path(Path),Params,''),
    string_concat("push/",Path,PseudoPath),
    versions(put,PseudoPath,json,Id,Params).
versions_commit(json,Id,Params):-
    option(path(Path),Params,''),
    string_concat("commit/",Path,PseudoPath),
    versions(put,PseudoPath,json,Id,Params).
versions_set_user_info(json,Id,Params):-
    option(path(Path),Params,''),
    string_concat("set-user-info/",Path,PseudoPath),
    versions(put,PseudoPath,json,Id,Params).
versions_reset(json,Id,Params):-
    option(path(Path),Params,''),
    string_concat("reset/",Path,PseudoPath),
    versions(delete,PseudoPath,json,Id,Params).

%
versions_results(json,Id,Result):-
    default_results(json,Id,[],Result). %.put(report,"No report until further notice")
versions_results(rest,Id,GitStatus):-   
        is_dict(GitStatus,_),
        default_results(rest,Id,[],[GitStatus.report]).   
versions_results(rest,Id,Result):-   
    default_results(rest,Id,[],Result).


