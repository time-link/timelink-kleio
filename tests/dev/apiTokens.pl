:-module(apiTokens,[
    tokens/5,users/5,
    tokens_generate/3,
    tokens_invalidate/3,
    users_invalidate/3]).

:- use_module(tokens).
:- use_module(persistence).

/** <module> Api operation dealing with tokens.

Generate tokens, invalidade tokens and users.

*/

%!tokens(+Method,+User,+ResultType,+Id,+Params) is det.
%
% Entry point for REST calls related to tokens.
%
tokens(post,User,ResultType,Id,Params):-
    tokens_generate(ResultType,Id,[user(User)|Params]).

tokens(delete,Token,ResultType,Id,Params):-
    tokens_invalidate(ResultType,Id,[user_token(Token)|Params]).

%!users(+Method,+User,+ResultType,+Id,+Params) is det.
%
% Entry point for REST calls related to users.
% 
% | _entity_      | _Method_     | _function_       | _meaning_                   |
% | users       | DELETE            | userss_invalidate| invalidate a user token |
%
% 
users(delete,Token,ResultType,Id,Params):-
    users_invalidate(ResultType,Id,[user_token(Token)|Params]).

%!tokens_generate(+ResultType,+Id,+Params) is det.
%
%  Generates a token for a user and associates it with info.
%  The predicate is agnostic regarding the info, it simply stores it.
%  The kleio translator uses the following structure.
%
% * info: information associated to the user. A structure containing:
% 	$ comment  : (optional) a string with a comment about the user.
% 	$ api      : (required). List of allowed actions:
% 		* files (download files, only get method allowed)
% 		* structures (download files, only get method allowed)
% 		* translations (translate files)
% 		* upload (upload files, also allows methods post and put on files)
% 		* sources (search for source files)
% 		* kleioset (get information on translation of a kleio file)
% 		* generate_token (generate token for new users)
% 		* invalidate_token (revokes token)
% 		* invalidate_user (revoke user - alternative to invalidate_token, when token is forgotten)
% 		* token_info (return information associated with a token - not yet implemented)
% 		* delete (delete a file, also method delete on files)
% 		* mkdir (create a directory)
% 		* rmdir (remove)
% 	* structures: (optional) base directory for user provided str files.
% 	* sources: (optional, required for user that do translations)
%
tokens_generate(ResultType,Id,Params):-
    option(user(UserName),Params), % TODO: generate exception
    option(info(json(Info)),Params), % TODO: generate exception
    option(token(Token),Params),
    (tokens:is_api_allowed(Token,generate_token) -> 
    true
    ;
    throw(http_reply(method_not_allowed(tokens_generate,UserName),['Request-id'(Id)]))  
    ),
    restServer:convert_to_options(Info,TokenInfo),
    tokens:generate_token(UserName,TokenInfo,NewToken),
    (get_shared_value(bootstrap_token,token(BToken))-> % if we have a bootstrap token and no longer need it...
        (tokens:invalidate_token(BToken),put_shared_value(bootstrap_token,none))
        ;
        true
        ), 
    restServer:default_results(ResultType,Id,_,NewToken).

%!tokens_invalidate(+ResultType,+Id, +Params) is det.
% 
% Invalidates previously issued token
%
tokens_invalidate(ResultType,Id, Params):-
    option(token(Token),Params),
    (is_api_allowed(Token,invalidate_token) -> 
    true
    ;
    throw(http_reply(method_not_allowed(invalidate_token,Token),['Request-id'(Id)]))  
    ),
    option(user_token(UserToken),Params),
    (tokens:get_user(UserToken,_) -> true; throw(error(Id,param_error('Invalid token')))),
   tokens:invalidate_token(UserToken),
   restServer:default_results(ResultType,Id,Params,UserToken).

%!users_invalidate(+ResultType,+Id, +Params) is det.
% 
% Invalidates previously issued tokens associated with an user.
%
users_invalidate(ResultType,Id,Params):-
    option(user(User),Params),
    option(token(Token),Params),
    (is_api_allowed(Token,invalidate_user) -> 
    true
    ;
    throw(http_reply(method_not_allowed(tokens_user,User),['Request-id'(Id)]))  
    ),
    (tokens:get_user(_,User) -> true; throw(error(Id,param_error('Invalid user')))),
    tokens:invalidate_user(User),
    restServer:default_results(ResultType,Id,Params,User).


