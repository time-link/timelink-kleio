% utilities for debugginh
:-module(apiLog,[
    client_log/5,
    client_log_send/3
    ]).

:-use_module(logging).
:-use_module(tokens).
:-use_module(restServer).

/** <module> Api utility to send debug messages to kleio-server logs

**/

client_log(post,_Path,_Mode,Id,Params):-
    % log(debug,'log_debug: path=~w, mode=~wparams:~n~w~n',[Path, Mode, Params]),
    option(token(Token),Params),
    (tokens:is_api_allowed(Token,files) ->
    true
    ;
    throw(http_reply(method_not_allowed(log_debug,'user needs files privilege'),['Request-id'(Id)]))
    ),
    option(message(Message),Params),
    option(level(Level),Params,debug),
    log(Level,'~w~n',[Message]),
    default_results(rest,Id,Params, [Level, Message]),
    !.


client_log_send(json,Id,Params):-
    option(path(Path),Params,''),
    client_log(post,Path,json,Id,Params),
    default_results(json,Id,Params, _{message:'log sent'}),!.


