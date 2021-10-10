:- module(tokens,
    [
    generate_token/3,           % +UserName, +Options=[data_dir(Dir),stru_dir(Dir),api(List)], ?AccessToken
    decode_token/3,             % +Token, ?UserName, ?Options
    invalidate_token/1,         % +Token 
    invalidate_user/1,          % +User
    attach_token_db/1,          % +File
    update_token_options/2,       % +Token,+Permission
    get_user/2,                 % +Token,?User
    get_token_options/2,          % +Token,?Permissions.
    get_stru_dir/2,             % +Token,?StruDir
    get_data_dir/2,             % +Token,?DataDir
    is_api_allowed/2,          % +Token,?API_EndPoint, backtracks on all allowed API calls for this token
    token_db_attached/1,        % ?File currently attached database, fails if none
    user_has_token/2                % ?User, ?Token - backtracks on all the users defined
    ]).

/** <module> user_tokens with API Tokens
 
## User Management with tokens.

To access the api incoming requests must provide an API Token.

The token is used as a key to the username, and a list of user information which include:

* the path to the data dir, which can be absolute or relative to the server data dir.
* the path to the structure dir, which can be absolute or relative to the server structure dir.
* permissions of the user, to be implemented but would be a list of API calls allowed for this user and constrains 
like expiry date, or source IP.

To obtain a token the predicate generate_token/3 is used. It will persist the user information using swipl `persistency` API.

To extract the information associated with a token use decode_token/3.

Tokens can by invalidated with invalidate_token/1.

This module uses the swi prolog persistency module to store the tokens in a database.

The file for the database can be set with attach_token_db/1 and inspected with token_db_attached/1.

If no file is defined for token persistence then default_token_db/1 is used to get a default DB.

An initial token is taken from env variable KLEIO_SERVER_ADMIN_TOKEN with permissions to issue and revoke tokens

See: http://www.swi-prolog.org/pldoc/doc/_SWI_/library/persistency.pl
*/

:-use_module(library(persistency)).
:-use_module(persistence).
:-use_module(kleioFiles).
:-use_module(logging).

:-persistent
        user_token(token:atom,name:atom,options:list).

:- set_test_options([run(manual)]).

default_token_db(D):-
    kleiofiles:kleio_conf_dir(CD),
    atom_concat(CD, '/token_db',D).

%% attach_token_db(+File) is det.
% Use File to store generated tokens
attach_token_db(F):-
    (
        (db_attached(F1) -> % Module already attached to DB 
            (F1 \= F ->  % Previous db is different, sync and detach
                (db_sync(_What),
                db_detach) 
            ;   % already attached to the file, do nothing
                true
            )
        ; % Module not attached, attach.
            db_attach(F,[])
        )
    ).

%% token_db_attached(?File) is det.
% Returns current token database file, fails if none is defined.
token_db_attached(F):-db_attached(F).

%% ensure_db is det.
%
% Ensures that a token database is attached. If none currently attached, attaches the default one.
ensure_db:-
    token_db_attached(_),
    !.
ensure_db:-
    kleiofiles:kleio_token_db(D),
    attach_token_db(D),
    !.

%% generate_token(+UserName,+Options,?AccessToken) is det.
% Generates an access token associating UserName with Options.
% Options should contain a list API endpoints and other contraints like expire(Date)
% Example: Options=[api([kleioset,translation,files,structures]),expire('2019-03-29')]
%
% If a valid token exists throws an exception. Use invalidate_token, or invalidade_user first.
% 
generate_token(UserName,_,AccessToken):-
    ensure_db,
    user_has_token(UserName,AccessToken),
    throw(error(UserName,-32600,'User already associated with token, invalidate user first.')).

generate_token(UserName,Options,AccessToken):-
    ensure_db,
    get_time(T),format_time(atom(S),'%s',T),
    random(1,32767,R),
    atomic_list_concat([UserName,S,R],C),
    sha_hash(C,H,[]),
    hash_atom(H,AccessToken),
    ( % check if token database was initialized, if not use default location
        token_db_attached(_F) ;
        (
            default_token_db(DF),
            attach_token_db(DF)
        )
    ),
    with_mutex(user_token,
        (
            retractall_user_token(_,UserName,_),
            assert_user_token(AccessToken,UserName,[created(T)|Options])
            )
        ),
    db_sync(_W),
    !.

%% decode_token(+Token,?UserName,?Options) is det.
% Returns UserName and Options associated with a Token.
decode_token(Token,UserName,Options):-
    ensure_db,
    (atom_concat('Bearer ',Token2,Token)->true;Token2=Token),
    user_token(Token2,UserName,Options),!.

decode_token(Token,UserName,Options):-
    get_kleio_admin(Token,UserName,Options).

%% get_kleio_admin(+TOKEN,-UserName,-Options) is det.
% returns TOKEN for KLEIO_ADMIN if set in env variable
get_kleio_admin(Token,User,Options):-
    getenv('KLEIO_ADMIN_TOKEN', Token),
    User = 'KLEIO_ADMIN',
    Options = [
        comment('KLEIO_ADMIN can translate, upload and delete files, and also create and remove directories'),
        api([sources,kleioset,files,structures,translations,upload,delete,mkdir,rmdir,generate_token,invalidate_token,invalidate_user]),
        structures(''),
        sources('')],
    !.

%% invalidate_token(+Token) is det.
% Removes a token.
invalidate_token(Token):-
    ensure_db,
    with_mutex(user_token,retractall_user_token(Token,_,_)).

%% invalidate_user(+User) is det.
% Removes a User.
invalidate_user(User):-
    ensure_db,
    user_token(_Token,User,_),
    with_mutex(user_token,retractall_user_token(_,User,_)).

%% user_has_token(?User,?Token) is nondet.
% True if Token is associated with User. If both unbound backtracks on all the users.
user_has_token(User,Token):-
    ensure_db,
    user_token(Token,User,_).

%% update_token_options(+Token,+Options) is det.
% Update the options associated with Token.
% @see get_token_options/2
update_token_options(Token,Options):-
    ensure_db,
    get_user(Token,UserName),
    with_mutex(user_token,
        (
            retractall_user_token(Token,_,_),
            assert_user_token(Token,UserName,Options)
        )
        ).        % +Token,+Permission

%% get_user(?Token,?User) is nondet.
% Token is associated with user. On backtracking gives all user-token pairs.
get_user(Token,User):-
    ensure_db,
    user_has_token(User,Token).             % +Token,?User
    
%% get_token_options(+Token,?Options) is det.
% Get the options associated with a Token.
% Checks the KLEIO_ADMIN_TOKEN env variable for admin 
get_token_options(Token,Options):-      % +Token,?Permissions.
    ensure_db,
    decode_token(Token,_,Options),!.


%% get_stru_dir(+Token,-SD) is det.
% Return the stru_dir associated with Token, or '.' if none associated.
%
get_stru_dir(Token,SD):-
    ensure_db,
    get_token_options(Token,P),
    option(stru_dir(SD),P,'.').

%% get_data_dir(+Token,-DD) is det.
% Return the data_dir associated with Token, or '.' if none associated.
%
get_data_dir(Token,DD):-
    ensure_db,
    get_token_options(Token,P),
    option(data_dir(DD),P,'.').

%% is_api_allowed(+Token,?APICall) is det.
% True if APICall is allowed by the options associated with Token. On backtracking gives on APICalls allowed for this token.
%
is_api_allowed(Token,APICall):-     % +Token,?API_EndPoint, backtracks on all allowed API calls for this token
    ensure_db,
    get_token_options(Token,P),
    option(api(CALLS),P),
    utilities:member(APICall, CALLS),
    check_age(P).

check_age(Options):-
    option(created(When),Options,0),
    When == 0.

check_age(Options):-
    option(life_span(S),Options,eternal),
    S == eternal.

check_age(Options):-
    option(created(T0),Options),
    option(life_span(S),Options),
    get_time(T1),
    Age is T1 - T0,
    Age < S.

check_age(Options):-
    format('Token too old ~w~n',[Options]),
    fail.


%% list_tokens is det.
% List defined tokens
list_tokens:-
    get_user(T,U),
    get_token_options(T,O),
    writeln(U-O-T),
    fail.
list_tokens.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TESTS 
% For docs see http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-begin_tests(tokens,[setup(setup_tests)]).

setup_tests:- % we create a token
    Options = [
        api([translations,sources,files]),
        data_dir('sources/testes/'),
        stru_dir('system/conf/kleio/stru')
        ],   
    (invalidate_user('username');true), % invalidate just in case
    !,
    catch(generate_token('username',Options, Token),_Catcher,true),
    get_user(Token,username),
    persistence:put_value(token,Token).

test(generate_token,[true]):-
        (invalidate_user('username');true), % invalidate just in case
        !,
        generate_token('username',[
                api([translations,sources,files]),
                data_dir('sources/testes/'),
                stru_dir('system/conf/kleio/stru')
        ], Token),
        persistence:put_value(token,Token),
        token_db_attached(F),
        decode_token(Token,'username',Options),
        format('~nToken database at ~w~nToken:~w~nOptions: ~w~n',[F,Token,Options]),!.

test(generate_token_duplicate,[
    throws(error(username,-32600,'User already associated with token, invalidate user first.'))
    ]):-
        generate_token('username',[
                api([translations,sources,files]),
                data_dir('sources/testes/'),
                stru_dir('system/conf/kleio/stru')
        ], _Token1),
        generate_token('username',[
                api([translations,sources,files]),
                data_dir('sources/testes/'),
                stru_dir('system/conf/kleio/stru')
        ], _Token2),!.

test(decode_token):-
    get_value(token,T),
    decode_token(T,UserName,Options),
    UserName = username,
    Options = [
        created(_),
        api([translations,sources,files]),
        data_dir('sources/testes/'),
        stru_dir('system/conf/kleio/stru')
        ]. 

test(invalidate_token,[fail]):-
    generate_token(invalid_user,[],Token),
    decode_token(Token,invalid_user,_),
    invalidate_token(Token),
    decode_token(Token,_,_).

test(invalidate_user,[fail]):-
    generate_token(invalid_user,[],Token),
    decode_token(Token,invalid_user,_),
    invalidate_user(invalid_user),
    decode_token(Token,_,_).

test(user_has_token):-
    persistence:get_value(token,T),
    user_has_token(username,T).

test(update_token_options):-
    persistence:get_value(token,T),
    get_token_options(T,Options),
    NewOptions=[newOption(true)|Options],
    update_token_options(T,NewOptions),
    get_token_options(T,Options2),
    NewOptions=Options2.

test(get_stru_dir):-
    persistence:get_value(token,T),
    get_stru_dir(T,SD),
    format('~nstru_dir:~w~n',[SD]).

test(get_data_dir):-
    persistence:get_value(token,T),
    get_data_dir(T,SD),
    format('~ndata_dir:~w~n',[SD]).

test(is_api_call):-
    persistence:get_value(token,T),
    is_api_allowed(T,translations),!.

test(short_lived_but_young,[true]):-
    (invalidate_user('young_enough');true), % invalidate just in case
    generate_token('young_enough',[
            life_span(3),
            api([translations,sources,files]),
            data_dir('sources/testes/'),
            stru_dir('system/conf/kleio/stru')
    ], Token),!,
    is_api_allowed(Token,translations),!.

test(too_old,[fail]):-
    (invalidate_user('too_old');true), % invalidate just in case
    generate_token('too_old',[
            life_span(3),
            api([translations,sources,files]),
            data_dir('sources/testes/'),
            stru_dir('system/conf/kleio/stru')
    ], Token),
    writeln('Waiting for the token to expire'),
    sleep(5),!,
    is_api_allowed(Token,translations),!.


:-end_tests(tokens).