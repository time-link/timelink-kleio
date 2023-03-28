:-module(restServer,[
    start_debug_server/0,
    start_rest_server/0,
    server_exec/3,
    show_server_activity/0,
    server_idle/1,
    show_processing_status/0,
    process_rest/1,
    ps/0,
    ssa/0,
    print_server_config/0,
    json_out/1,
    default_results/4,
    convert_to_options/2,
    print_content_type/2,
    print_content_type/1,
    print_list/1,
    print_option_list/1,
    get_authorization_token/2,
    return_error/2,
    make_rest_url/3
    ]).
/** <module> REST and JSON-RPC server for Kleio


## Implementation

### Json

The server catches the /json request and passes it to process_json_rpc/1.


§
    :- http_handler(root('json/'), process_json_rpc, [time_limit(1800)]). % time limit 30m.


 The predicate process_json_rpc/1 tries to process a json request catching errors.
 Errors are passed to process_json_rpc_error/1.




        process_json_rpc(Request):-
            format('Content-type: application/json~n~n',[]),
            catch(process_json_rpc_(Request),
                    Error,
                    process_json_rpc_error(Error)
                    ).


In the first step process_json_rpc_/1 gets json payload and branches to batch request or single request


    process_json_rpc_(Request):-
        catch(http_read_json(Request, JSONPayload),ParseError,
            throw(parse_error(null, -32700,ParseError))),
        % if JSONRequest is a list we have a batch.
        (is_list(JSONPayload) ->
            process_json_batch(JSONPayload);
            process_json_request(JSONPayload)
        ),
        !.

Errors are caught by process_json_rpc_error/1, logged and passed to return_error/2 for proper output.


        process_json_rpc_error(Error):-
            with_output_to(string(Message),write(Error)),
            log_error(Message,[]),
            return_error(json,Error),!.


Each individual json request is executed by process_json_request_/1 in three steps:

* Decoding the command, extracting the Id, Method and Params with json_decode_command/4.
* Calling json_exec/4 to execute the method
* Formating and outputing the results with return_sucess/5


        process_json_request_(JSONRequest):-
            json_decode_command(JSONRequest,Id,Method,Params),
            json_exec(Method, Id, Params,Results),
            return_sucess(json,Method,Id,Params,Results),!.


json_exec/4 tries to call a predicate named Method(json,Id,Params,Results). Here is the code used to call the predicate:


        json_exec(Method,Id,Param,Results):-
            Goal =.. [Method,json,Id,Param,Results],
            catch(call(Goal),error(existence_error(_,_),context(_,_)),throw(method_not_found(Id,Op))).

return_sucess/5 tries to call a predicated named Method_results(json,Id,Params,Results). Here is the code used:

        return_sucess(json,Method,Id,Params,Results) :-
            atom_concat(Method,'_results',Predicate),
            Goal =.. [Predicate,json,Id,Params,Results],
            catch(call(Goal),error(existence_error(_,_),context(_,_)),fail).

Note that is Goal does not exist or generates an exception

Each new API function to be implemented needs to add new instances of three predicates:

method(json,+Id,+Param,-Results) to execute the function and set Results to the output to be returned
method_results(json,+Id,+Param,-Results)

## Environment variables checked by the server

$ KLEIO_HOME_DIR        : Kleio base directory default ~/kleio-home, ~/timelink-home, ~/mhk-home, '.'
$ KLEIO_SOURCE_DIR      : Defaul KLEIO_HOME/sources
$ KLEIO_CONF_DIR        : For configuration information KLEIO_HOME/system/conf/kleio
$ KLEIO_STRU_DIR        : Defaults KLEIO_HOME/system/conf/kleio/stru/
$ KLEIO_TOKEN_DB        : Defaults KLEIO_CONF_DIR/token_db -- maintains the token database.
$ KLEIO_DEFAULT_STRU    : Default KLEIO_HOME/system/conf/kleio/stru/gacto2.str structure used by default
$ KLEIO_DEBUGGER_PORT   : Port for the debug server (default 4000).
$ KLEIO_SERVER_PORT     : Port for the REST server (default 8088).
$ KLEIO_SERVER_WORKERS   : Number of worker threads used by the rest server.

## References
   *  http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/http.html%27)
   *  http://www.swi-prolog.org/pldoc/man?section=httpjson

## NEXT STEPS
* TODO: interaction with database server (vocabularies, etc..)
* TODO: EndPoint to generate kleiodoc
   
**/


:- use_module(library(prolog_server)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_multipart_plugin)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_host)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(debug)).
:- use_module(library(pprint)).
:- use_module(library(option)).

% local imports
:- use_module(threadSupport).
:- use_module(reports).
:- use_module(kleioFiles).
:- use_module(utilities).
:- use_module(persistence).
:- use_module(logging).
:- use_module(topLevel).
:- use_module(tokens).
:- use_module(errors).
:- use_module(counters).
:- use_module(apiCommon).


% we keep related predicates together in the sources code.
:- discontiguous json_exec/4, rest_exec/4, return_sucess/5,server_exec/3.
:- multifile mime_extension/2.
:- multifile prolog:message//1.



% NOTE: when run in debug mode in VS Code the env variables are not acessible
% because it runs in another process.
%
default_value(server_port,SP):-getenv('KLEIO_DEBUGGER_PORT',SSP), atom_number(SSP,SP),!.
default_value(server_port,4000):-!.
default_value(rest_port,RP):-getenv('KLEIO_SERVER_PORT',SRP), atom_number(SRP,RP),!.
default_value(rest_port,8088):-!.
default_value(workers,Workers) :- getenv('KLEIO_SERVER_WORKERS',Atom),atom_number(Atom,Workers),!.
default_value(workers,3):-!.
default_value(timeout,Timeout) :- getenv('KLEIO_IDLE_TIMEOUT',Atom),atom_number(Atom,Timeout),!.
default_value(timeout,900):-!.
default_value(cors,CorsList):- getenv('KLEIO_CORS_SITES',ListString),atomic_list_concat(CorsList, ',', ListString).
default_value(cors,['*']) :- !.

%! print_server_config is det.
%
% prints the configuration values of the server.
print_server_config:-
    restServer:default_value(server_port,SP),
    restServer:default_value(rest_port,RP),
    restServer:default_value(workers,Workers),
    restServer:default_value(timeout,Timeout),
    restServer:default_value(cors,Cors),
    kleiofiles:kleio_home_dir(Home),
    kleiofiles:kleio_conf_dir(Conf),
    kleiofiles:kleio_stru_dir(StruDir),
    kleiofiles:kleio_default_stru(Stru),
    kleiofiles:kleio_token_db(Tokens),
    tokens:ensure_db,
    (tokens:token_db_attached(TokensCurrent);TokensCurrent=none),
    kleiofiles:kleio_source_dir(Sources),
    (getenv('KLEIO_DEBUG',DEBUG);DEBUG=false),
    ((getenv('KLEIO_ADMIN_TOKEN',ATOKEN), sub_atom(ATOKEN,0,5,_,AT5))
        -> true ;AT5='No token in environment'),
    clio_version(V),
    format('Version: ~t~w~n',[V]),
    format('Debug mode: ~t~w~n',[DEBUG]),
    format('Debug port: ~t~w~n',[SP]),
    format('REST port: ~t~w (check if mapped in docker)~n',[RP]),
    format('Workers: ~t~w~n',[Workers]),
    format('Timeout: ~t~w~n',[Timeout]),
    format('Cors: ~t~w~n',[Cors]),
    format('/kleio_home dir on host: ~t~w~n',[Home]),
    format('Kleio_home_dir: ~t~w~n',[Home]),
    format('Kleio_conf_dir: ~t~w~n',[Conf]),
    format('Kleio_stru_dir: ~t~w~n',[StruDir]),
    format('Kleio_default_stru: ~t~w~n',[Stru]),
    format('Kleio_admin_token: ~t~w~n',[AT5]),
    format('Kleio_token_db: ~t~w~n       current: ~w~n',[Tokens,TokensCurrent]),
    get_token_db_status(TDBS),
    format('Token db status: ~t~w~n',[TDBS]),
    format('kleio_source_dir: ~t~w~n',[Sources]),
    (get_shared_prop(log,file,Log)-> true; Log='*Logging not started'),
    format('~nLogging to: ~w~n',[Log]),
     !.

save_kleio_config:-
    (get_shared_prop(log,file,Log)-> true; Log='*Logging not started'),
    (tokens:get_kleio_admin(ATOKEN,_,_)
        -> true 
        ;
        ATOKEN=null), % null is JSON convention for null
    (getenv('KLEIO_HOME_LOCAL',HomeLocal)
        -> true 
        ;
        HomeLocal=null), % null is JSON convention for null
    restServer:default_value(rest_port,RP),
    clio_version_version(V),
    clio_version_build(B),
    clio_version_date(D),
    kleiofiles:kleio_home_dir(Home),
    kleiofiles:kleio_conf_dir(Conf),
    kleiofiles:kleio_admin_token_path(AdminTokenPath),
    get_token_db_status(TDBS),
    % get todays date
    get_time(T), 
    format_time(string(NowDateTime),'%FT%T%z',T),
    % output config info
    atom_concat('http://localhost:',RP,KURL),
    KleioSetup = ksetup{kleio_home_local:HomeLocal,
                        kleio_home:Home, 
                        kleio_admin_token:ATOKEN,
                        kleio_url:KURL,
                        kleio_log:Log,
                        kleio_version:V,
                        kleio_version_build:B,
                        keio_version_date:D,
                        kleio_conf_dir:Conf,
                        kleio_token_db_status:TDBS,
                        kleio_admin_token_path:AdminTokenPath,
                        kleio_setup_date:NowDateTime
                        },
    atom_concat(Home,'/.kleio.json',KleioSetupFile), 
    open(KleioSetupFile,write,KFI_Stream,[]),        
    json_write_dict(KFI_Stream,KleioSetup),
    close(KFI_Stream).


admin_token_exists:- getenv('KLEIO_ADMIN_TOKEN',_).
admin_token_ok :- getenv('KLEIO_ADMIN_TOKEN',ATOKEN), sub_atom(ATOKEN,0,5,_,_).
bootstrap_token_exists:- tokens:user_token(_,bootstrap,_).
bootstrap_token_ok :- tokens:user_token(T,bootstrap,_), is_api_allowed(T,generate_token).
tokens_exist :- tokens:ensure_db, tokens:user_token(_,U,_), U \= bootstrap,!.

get_token_db_status('Tokens exist') :- tokens_exist.
get_token_db_status('No tokens defined but KLEIO_ADMIN_TOKEN env has valid value') :- 
    \+ tokens_exist,
    admin_token_ok,!.
get_token_db_status('No tokens defined and bad KLEIO_ADMIN_TOKEN') :- 
    \+ tokens_exist,
    admin_token_exists,
    \+ admin_token_ok,!.
get_token_db_status('Waiting to generate first token') :- bootstrap_token_ok,!.
get_token_db_status('No tokens, bootstrap token expired. Set env KLEIO_ADMIN_TOKEN or delete token_db') :- 
    bootstrap_token_exists,
    \+ bootstrap_token_ok,!.
get_token_db_status('Token generation blocked. Set env KLEIO_ADMIN_TOKEN or delete token_db') :- 
    \+ tokens_exist,
    \+ admin_token_ok,
    \+ bootstrap_token_ok,!.
get_token_db_status('Could not determine token status').




% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dispatcher zone
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- http_handler(root('rest/files/'),handle_files_request,[prefix,methods([get,put,post,delete])]). %workaround could not make it work as normal rest
% :- http_handler(root('rest/structures/'),serve_structure_file,[prefix,method(get)]). % same workaround
% :- http_handler(root('rest/upload'),upload,[id(upload),prefix,method(post)]).
:- http_handler(root('rest/'), process_rest, [id(rest),time_limit(300),prefix,methods([get,delete,put,post,options])]).
:- http_handler(root('json/'), process_json_rpc, [id('json-rpc'),time_limit(300),methods([post,options])]). % time limit in seconds
:- http_handler(root(.), home_page, [methods([get,options])]).
:- http_handler(root('forms/upload/'),upload_form,[]). % generate a form for uploading files
:- http_handler(root(echo), echo_request, [prefix]).

%! start_debug_server is det.
%
% Starts a debugging server in port default_value(server_port,Port).
% if Port = 0 server not created
% Usefull to test and debug when no console available.
start_debug_server:-
     default_value(server_port,Port),
     Port \= 0,
    % options are set in serverStart.pl file
     (get_prop(prolog_server,options,Options);Options=[allow(ip(_,_,_,_))]),
     log_debug('Starting DEBUG server on port ~w with ~w options. ~n',[Port,Options]),
     prolog_server(Port,Options),
     log_debug('Debug server started in port ~w~n',[Port]).
start_debug_server:-
    log_debug('Debug server NOT started (port=0)~n',[]).

%% start_rest_server is det.
%
% Starts rest server
%
start_rest_server:-
    default_value(rest_port,Port),
    default_value(workers,Number),
    default_value(cors,Cors),
    log_debug('Starting REST server on port ~w with ~w worker threads. ~n',[Port,Number]),
    put_shared_value(pool_mode,message),
    set_shared_count(rest_request_count,1),
    set_shared_count(jsonrpc_request_count,1),
    http:set_setting(cors,Cors),
    create_workers(Number),
    check_token_database,
    server(Port),
    log_debug('REST JSON server listening on port ~w with ~w workers~n',[Port,Number]).

server(Port) :-
        default_value(timeout,Number),
        http_server(http_dispatch,
                    [ port(Port),timeout(Number) % 15 secs timeout on requests
                    ]),
        save_kleio_config.

%% server_idle(+Seconds) is det.
%
% The restServer is considered idle if after Seconds there are no queued nor processing jobs.
%
% This predicate is used to have auto-stopping servers
%
% @see serverStart.pl
server_idle(Seconds):-
    sleep(Seconds),
    get_queued(Q),
    get_processing(P),
    Q == [],
    P == [],
    set_shared_count(queued,1),
    set_shared_count(jobs,1).

server_idle(Seconds):-log_debug('Server busy. Waited for ~w seconds.',[Seconds]),fail.

%% ssa is det.
%
% Synomym for show_server_activity. 
%
ssa:-show_server_activity.

%% show_server_activity is det.
%
% Output current server status and running threads.
% This info is also available in the home page. See home_page/1
%
show_server_activity:-
    repeat,
    nl,
    show_processing_status,
    threads,
    nl,
    (server_idle(10) -> (writeln('SERVER IDLE'),fail)).


%% check_token_database is det.
%
% Checks if token database exists, if not it initializes it.
%
% 
check_token_database:- 
    check_token_database(exists),
    check_token_database(bootstrap),!.

check_token_database(exists):-
    tokens:token_db_attached(TokensCurrent),
    log_info('Serving starting with token database at: ~w~n',[TokensCurrent]),!.

check_token_database(exists):-
    kleiofiles:kleio_token_db(DB),
    (exists_file(DB)->true;log_info('Token db at ~w does not exist. Will create.',[DB])),
    tokens:attach_token_db(DB),!.

check_token_database(bootstrap):-
    getenv('KLEIO_ADMIN_TOKEN',_),!.

check_token_database(bootstrap):-
    % Generate a 'bootstrap' token with admin privileges
    A = kleio_admin,
    (tokens:invalidate_user(A);true),
    kleiofiles:kleio_admin_token_path(AdminTokenFile),
    tokens:get_admin_options(Opts),
    tokens:generate_token(A,Opts,Token),
    put_shared_value(bootstrap_admin_token,token(Token)),
    open(AdminTokenFile,write,F,[]),
    write(F,Token),
    close(F).


home_page(_):-
        format('Content-type: text/html~n~n', []),
        format('<html>~n', []),
        format('<head><meta http-equiv="refresh" content="60" ><link rel="icon" href="data:;base64,="></head>~n', []),
        format('<body>~n', []),
        format('<h3>Kleio JSON-RPC server ready.~n</h3>~n'),
        clio_version(V),
        format('<h4>~w</h4>~n',[V]),
        get_time(T),
        format_time(atom(Time),'%Y-%m-%d %H:%M:%S',T),
        get_shared_count(rest_request_count,RC),
        get_shared_count(jsonrpc_request_count,JC),
        format('~n<xmp>~n'),
        format('REST requests: ~w, JSONRPC requests: ~w~n',[RC,JC]),
        format('Current time: ~w~n',[Time]),
        persistence:get_shared_cons(more_attributes,Cons),
        length(Cons,NumberCached),
        format('Number of attribute files cached: ~w~n',[NumberCached]),
        nl,
        print_server_config,
        format('~n</xmp>~n'),
        format('<pre>~n~@~n</pre>',[show_processing_status]),
        format('</body>~n', []),
        format('</html>~n', []).

%% make_rest_url(+Path,+Entity,-URL) is det.
%
% builds a URL for a rest call with  Path on Entity (must finish with /)
%
make_rest_url('',_,''):-!.
make_rest_url(Path,Prefix,URL):-
    atom_concat(Prefix,Path,P1),
    http_link_to_id(process_rest,path_postfix(P1),URL). % this escapes characters as needed. 


%% mime_extension(+Ext,-Mime) is nondet.
%
% Maps the mime types of kleio files. 
% 
% See http://www.swi-prolog.org/pldoc/doc_for?object=mime%3Amime_extension/2
%
mime:mime_extension(Ext,Mime):-
    kleiofiles:kleio_mime_type(Ext,Mime).


%! process_rest(+RestRequest) is det.
%
% Processes a REST request, in a three step approach.
%
%  1. Decode de command with rest_decode_command/4 extracting the _entity_ (type of object), 
%   the _http_method_ and the _path_ to the object targetted. Since we deal 
%       with files the _path_ is basically an id to the object. 
%   For instance http://localhost:8088/rest/sources/baptisms/b1686.cli 
%   the entity is "sources" the http_method is "GET" and the path is "baptisms/b1686.cli". The
%   resulting operation to be performed will be _sources_get_ on the object "baptisms/b1686.cli" .
%   
%   The request can contain aditional parameters encoded in the http standard way.
%
%   The decoded request is then passed to rest_exec/4 which will execute the operation and return the result.
%
%   The result is then passed to return_sucess/5 to format the output.
%   In case of an exception process_rest_error/1 is called to output the error information.
%
%   Note that certain predicates in SWI Prolog libraries produce output by generating exceptions that
%   are caught by the server top level code and trigger the output. This seems to be used in cases where
%   SWI will take care of the whole output, like http_reply_file/3 and http_reply_from_files/3.
%
process_rest(Request) :-
    option(method(options), Request), !,
    cors_enable(Request,
                [ methods([get,post,delete,put])
                ]),
    format('~n'). 

process_rest(RestRequest):-
    cors_enable(RestRequest,[methods([get,post,delete,put])]),
    inc_shared_count(rest_request_count,_),
    http_public_host(RestRequest, Hostname, Port, []),
    option(path_info(PInfo),RestRequest,''),
    http_link_to_id(process_rest,path_postfix(PInfo),HREF),
    log_debug('>>>>>>>>>>>>>>>>>>>> ~n',[]),
    log_debug('~n~nREQUEST-REST received on host ~w:~w~w~n~@~n',[Hostname,Port,HREF,print_term(request(RestRequest),[output(logfile)])]),
    catch(process_rest_request(RestRequest),
        Error,
        process_rest_error(Error)
    ).

process_rest_request(Request):-
    rest_decode_command(Request,Id,method(Entity,Method,Object),Params),
    log_debug('~n~nREQUEST_DECODED ID: ~w Entity:~w Method: ~w Object:~w~n',[Id,Entity,Method,Object]),
    rest_exec(method(Entity,Method,Object),Id,Params),
    !.


%% json_out(Params) is det.
%
% True if the request required json output. There are several ways to request json output:
%
% * in the parameters include json=yes or json=true
% * in the Accept header of the HTTP request include application/json
%
json_out(Params):-
    option(json(yes),Params),!.
json_out(Params):-
    option(json(no),Params),
    !,fail.
json_out(Params):-
    option(json(true),Params),!.
json_out(Params):- 
    option(json(false),Params), 
    !,fail.
json_out(_):-
        httpd_wrapper:http_current_request(Request),
    option(accept(List),Request),
    member(media(text/plain,_,_,_),List),!,fail.
json_out(_):-
    httpd_wrapper:http_current_request(Request),
    option(accept(List),Request),
    member(media(application/json,_,_,_),List),!.

process_rest_error(Error):-
    return_error(rest,Error).

%% rest_decode_command(+Request,-Id,-Method,-Params) is det.
%
% Decodes a REST call extracting Id, Method (API Call), Parameters.
% 
% Tests if API token is registered and if Method is allowed for the user associated with the token.
%
rest_decode_command(Request,Id,method(Entity,HMethod,Object),
        [token_info(TokenInfo),token(Token),
        request(Request)|Params]):-
    option(method(HMethod),Request),
    option(path_info(PathInfo),Request,''),
    get_entity_object(PathInfo,Entity,Object),
    log_debug('REST request Entity: ~w Object:~w Method:~w',[Entity,Object,HMethod]),
    get_authorization_token(Request,Token),
    (Token \= null -> true; throw(http_reply(bad_request(token_missing)))),
    (tokens:decode_token(Token,_,TokenInfo)->true; throw(http_reply(bad_request(error(system_error,context(rest_decode_command/4,'could not decode token')))))),
    % Here we use http_read_data for multipart post requests and http_parameters for other requests
    % Multipart post requests are used to upload files and http_read_data will store
    % uploaded data in a temporary file. See http://www.swi-prolog.org/howto/http/FileUpload.html
    % See also apiSources:source_upload
    (multipart_post_request(Request)->
        (upload_allowed(Token,PathInfo),
         http_read_data(Request, Params,
            [ on_filename(save_file)
            ]),
        (memberchk(id=Id,Params);Id=null)
        )
    ;
        http_parameters(Request,[
            id(Id,[default(null)])
            ],[form_data(Params)])
    ),
    !.


% tests if Resquest is a multipart post request. See http://www.swi-prolog.org/howto/http/FileUpload.html
multipart_post_request(Request) :-
    (memberchk(method(post), Request);memberchk(method(put), Request)),
    memberchk(content_type(ContentType), Request),
    http_parse_header_value(
        content_type, ContentType,
        media(multipart/'form-data', _)).

%% upload_allowed(+Token) is det.
%
% True if Token has upload permission. If not throws forbidden exception
upload_allowed(Token,Context):-
    (is_api_allowed(Token,upload) -> 
        true
    ;
        (  
        throw(http_reply(forbidden(Context))) 
        ) 
    ).    

%! get_entity_object(+PathInfo,-Entity,-EntityId) is det.
% 
% Extracts the the entity and the Object (normally a path) from the pathinfo).
%
get_entity_object(PathInfo,Entity,EntityId):-
    (atomic_list_concat(['',Entity|EntityIdList], '/', PathInfo)
    ;
    atomic_list_concat([Entity|EntityIdList], '/', PathInfo)
    ),
    atomic_list_concat(EntityIdList,'/',EntityId),
    !.


%! get_authorization_token(+Request,-Token) is det.
%  
% Extract the autorization token from the request.
% Normally in a Autorization header with the format "Bearer TOKEN"
get_authorization_token(Request,Token):-
    option(authorization(T),Request),
    atom_concat('Bearer ',Token,T).
get_authorization_token(Request,Token):- % This allows debugging with forms
    option(search(SearchTerms),Request,[]),
    option(token(Token),SearchTerms,null).
%! is_collection(Entity,EntityId,TokenInfo) is det.
%
%  Determines if an entity of type Entity and id EntityId is a collection or a single entity.
%  In fact checks if EntityId is a path to a directory of a single file. 
%  Can be extended to handle patterns.
%
is_collection(sources,Path,TokenInfo):-
    kleio_resolve_source_file(Path,AbsFile,TokenInfo),
    exists_directory(AbsFile).

%! rest_exec(+Operation, +RequestId,+Params,-Results) is det.
%
% Switches the execution of a specific operation 
%
% In the new style Operation is a term: method(Entity,Object,Method).
%   and rest_exec calls the goal Entity(Method,Object,Mode,Id,Params) note that results must be handled by the goal.
% In the old style Operation was just an atom
%   and rest_exec call the goal Operation(Mode,Id,Param,Results)
%
% TODO: could have a final catchall to throw an error.
rest_exec(method(Entity,Method,Object),Id,Params):-
    (json_out(Params)->Mode=json;Mode=rest),
    Goal =.. [Entity,Method,Object,Mode,Id,Params],
    catch(call(Goal),error(existence_error(_,_),context(_,_)),fail).

% TODO: DEPRECATED
rest_exec(Operation,Id,Param,Results):-
    (json_out(Param)->Mode=json;Mode=rest),
    Goal =.. [Operation,Mode,Id,Param,Results],
    catch(call(Goal),error(existence_error(_,_),context(_,_)),fail).
    
%% process_json_rpc(+Request) is det.
% Process a json RPC request, according to the json-RPC specs 2.0
% Handles single requests and batch requests.
% Decodes the request into JSONPayload and passes it to either process_json_batch/1 or process_json_request/1 .
%
process_json_rpc(Request) :-
    option(method(options), Request), !,
    cors_enable(Request,
                [ methods([post])
                ]),
    format('~n'). 
process_json_rpc(Request):-
    %print_content_type(json),
    cors_enable(Request,[methods([post])]),
    inc_shared_count(jsonrpc_request_count,_),
    catch(process_json_rpc_(Request),
        Error,
        process_json_rpc_error(Error)
        ).

%% process_json_rpc_error(+Error) is det.
%  returns an Error according to the JSON-RPC specs
%
process_json_rpc_error(Error):-
    print_content_type(json),
    with_output_to(string(Message),write(Error)),
    log_error(Message,[]),
    return_error(json,Error),!.

process_json_rpc_(Request):-
    %http_read_json(Request, JSONRequest),
    catch(http_read_json(Request, JSONPayload),ParseError,
        throw(parse_error(null, ParseError))),
    % if JSONRequest is a list we have a batch.
    log_debug('>>>>>>>>>>>>>>>>>>>> ~n',[]),
    log_debug('JSON-RPC: ~w~n',[JSONPayload]),
    (is_list(JSONPayload) ->
        process_json_batch(JSONPayload);
        process_json_request(JSONPayload)
    ),
    !.

%% process_json_batch(+ListOfJsonRequests) is det.
%
% Processes a batch of JSON request by calling process_json_request/1 on each list member
%
process_json_batch([]):-!.
process_json_batch([JSONRequest]):-!,
    writeln('['),
    process_json_request(JSONRequest),
    writeln(']').
process_json_batch(JSONBatchRequest):-
    length(JSONBatchRequest,L),
    member_nth(JSONPayload,JSONBatchRequest,N),
    (N = 1 -> writeln('[') ; true),
    process_json_request(JSONPayload),
    (N < L -> writeln(', ');true),
    (N = L -> writeln(']');true),
    fail.

process_json_batch(_):-!.

%% process_json_request(+JSONRequest) is det.
%
% Processes a single json rpc request, and outputs the result. If an error is throw it outputs a error record.
%
process_json_request(JSONRequest):-
    catch(process_json_request_(JSONRequest),
        Error,
        process_json_rpc_error(Error)
        ).

%% process_json_request_(+JSON) is det.
%
% Processes a json-rpc request in three steps:
%
% * decodes the payload and extracts Method, Params and Id if any
% * hands over the execution to json_exec/4.
% * formats the result in json-rpc record format.
%
process_json_request_(JSONRequest):-
    json_decode_command(JSONRequest,Id,Method,Params),
    option(path(Path),Params,nopath),
    log_debug('~nREQUEST_DECODED_JSON_RPC ID: ~w Method: ~w Path:~w~nParams:~w~n',[Id,Method,Path,Params]),
    catch(json_exec(Method, Id, Params),
        error(Term,Code,Message),
        process_json_rpc_error(invalid_request(Id,error(Term,Code,Message)))).
        

process_json_request_(JSONRequest):- % TODO: Deprecated
    json_decode_command(JSONRequest,Id,Method,Params),
    json_exec(Method, Id, Params,Results),
    (select(token_info(_),Params,P);P=Params), % we remove the token info introduced by json_decode_command
    (select((token=_),P,ParamsClean);ParamsClean=P),% and also the original token in the request
    return_sucess(json,Method,Id,ParamsClean,Results),!.

%% json_decode_command(+JSONRequest,?Id,?Method,?Params) is det.
%
% Decodes a JSON-RPC call extracting Id, Method, Parameters.
% Tests if API token is registered and if Method is allowed for the user associated with the token.
%
json_decode_command(json(JSONRequest),Id,Method,[token_info(TokenParams)|Params]):-
    option(id(Id),JSONRequest,null),
    option(method(Method),JSONRequest),
    option(params(json(Params)),JSONRequest),
    (option(token(Token),Params) -> 
        true
        ;  
        throw(invalid_params(Id,'Missing parameter: token'))
    ),
    (tokens:decode_token(Token,_,TokenParams)->true; throw(invalid_params(Id,'Bad token'))).

json_decode_command(JSONRequest,_Id,_Method,_Params):-
    throw(invalid_request(null,JSONRequest)).


%% json_exec(+Method,+Id,+Params,+Results) is det.
%
% Switches the execution of a specific operation, trying to call operation(RequestId,Params,Results)
%
%
json_exec(Op,Id,Param):-
    Goal =.. [Op,json,Id,Param],
    catch(call(Goal),error(existence_error(_,_),context(_,_)),throw(method_not_found(Id,Op))).

%% return_sucess(+Apitype,+Method,+Id,+Params,+Results) is det.
%
% Outputs results of API call. This is called by json_exec and rest_exec upon successfull execution of a request
% 
%
% Params:
%
% @arg Apitype json or rest
% @arg Method request called
% @arg Id Id of the request as per json-rpc specs.
% @arg Params original params of the request.
% @arg Results results of the call
%
% Each combination of Apitype and Method requires a specific implementation a predicate 
% Method_results(+ApiType,+Id, +Params, +Results) which will be called bh this predicate in order
% to output `Results` in a format compatible with the `Apitype`.
%
% Example:
%
%
% Note that by JSON-RPC specs a request with no id should get no response.
%
% For convenience two default implementations are provided.
% * For REST requests, if Method is "default", this predicate will output the results assumed to be a list of term(Value).
% * For JSON-RPC, similarly, if the method is default it will output the results assumed to be a list or a dictionary.
%
return_sucess(json,_Method,null,_Params,_Results) :-!.
return_sucess(json,Method,Id,Params,Results) :-
    atom_concat(Method,'_results',Predicate),
    Goal =.. [Predicate,json,Id,Params,Results],
    catch(call(Goal),error(existence_error(_,_),context(_,_)),fail).

% if resuts is "ignore" do nothing. Output was handled elsewhere.
% default result is to prnt the parameters and then the list of results. Both assumed as lists of term(Value)
return_sucess(json,default,Id,Params,Results):-
    default_results(json,Id,Params,Results).

return_sucess(rest,_,_,_,ignore):-!.
return_sucess(rest,Method,Id,Params,Results) :-
    atom_concat(Method,'_results',Predicate),
    Goal =.. [Predicate,rest,Id,Params,Results],
    catch(call(Goal),error(existence_error(_,_),context(_,_)),fail).
return_sucess(ApiType,default,Id,Params,Results):-
    default_results(ApiType,Id,Params,Results).

%% default_results(+ApiType,+Id,+Params,+Results) is det.
%
% Provides a default output for results.
% If ApiTYpe is rest then Results is assumed to be a list of terms with arity one, e.g. file('sources/a.cli'),file('sources/b.cli')
%  which will be output in lines like
% ==
%  file:sources/a.cli
%  file:sources/b.cli
% ==
% If ApiType is json then Results is assumed to be a dict or a list and it will be converted to json and inserted in the proper
% place in a JSON-RPC reply.
%
default_results(rest,Id,_Params,Results):-
    print_content_type(rest,Id),
    format('~@~n',
        [print_option_list(Results)]),!.

default_results(rest,Id,_Params,Results):-
        is_dict(Results),
        print_content_type(rest,Id),
        current_output(Out),
        print_term(Results,[output(Out)]),!.

default_results(json,null,_,_):-
    format('Content-type: application/json~nStatus: 204~n~n',[]),!.

default_results(json,Id,_Params,Results):-
    Id \= null,!,
    Return = _{jsonrpc:'2.0',
        result:Results,
        id:Id},
    current_output(O),
    print_content_type(json),
    json_write(O,Return,[]).
            
print_content_type(rest):-
    format('Content-type: text/text~n~n', []).
print_content_type(json):-
    format('Content-type: application/json~n~n',[]).
print_content_type(rest,RequestId):-
    format('Content-type: text/text~n', []),
    format('Request-id: ~w~n~n', [RequestId]).

print_option_list([Option|Options]):-
    (Option =.. [O,V];Option = (O=V)),
    format('~w=~w~n',[O,V]),
    print_option_list(Options).
print_option_list([Dict|Options]):-
    is_dict(Dict),
    current_output(Out),
    print_term(Dict,[output(Out)]),
    print_option_list(Options).
print_option_list(Dict):-
    is_dict(Dict),
    current_output(Out),
    print_term(Dict,[output(Out)]),!.

print_option_list([Plain|Options]):-
    format('~w~n',[Plain]),
    print_option_list(Options).
print_option_list([]).

print_list(L):-
    member(A,L),
    writeln(A),
    fail.

print_list(_):-!.

return_sucess(rest,print,_Id,_Params,Results):-
    print_content_type(rest),
    return_rest_default(Results).

return_rest_default([Result|Results]):-
    call(Result),
    return_rest_default(Results).

return_rest_default([]):-
    nl.



:- public save_file/3.

save_file(In, file(FileName, File), Options) :-
        option(filename(FileName), Options),
        setup_call_cleanup(
            tmp_file_stream(octet, File, Out),
            copy_stream_data(In, Out),
            close(Out)).
  
return_sucess(json,upload,Id,_,Results):-
            Id \= null,
            [destination(File)] = Results,
            Return =json([
                jsonrpc='2.0',
                result=File,
                id=Id]),
            current_output(Out),json_write(Out,Return).
        
%% upload_form(+Request) is det.
% From: http://www.swi-prolog.org/howto/http/FileUpload.html
% Generates a form to allow uploading of a file to the rest server
%
upload_form(Request) :-
    option(method(options), Request), !,
    cors_enable(Request,
            [ methods([get,post,delete,put])
            ]),
    format('~n'). 
upload_form(Request) :-
    get_authorization_token(Request,Token),
    atomic_list_concat(['/rest/sources/uploads?token=',Token],URL),
    reply_html_page(
        title('Upload a file'),
        [ h1('Upload a file'),
            form([ method('POST'),
                    action(URL),
                    enctype('multipart/form-data')
                ],
                table([],
                        [ tr([td(input([type(file), name(file),multiple]))]),
                        tr([
                            td([ 
                            'Id:',input([type(number),name(id)]),
                            'path:',input([type(text),name(path_info)])
                            ])
                        ]),
                        tr([td(align(right),
                                input([type(submit), value('Upload!')]))])
                        ]))
        ]).


%%
% ### API CALL: structures
%
%  Download Structure files. Not available is JSON.
%

%% serve_structure_file(+Request) is det.
%
% returns the content of a structure file (.str) or report file (.srpt)
% Note: if it is desirable that special stru files are kept with sources, 
% configure user token so that sources and structure dirs overlap 
%
serve_structure_file(_Request):-!.
  
/* serve_structure_file(Request):-
    option(path_info(FilePath),Request),
    http_parameters(Request,[
            id(Id,[default(null)]),
            token(Token,[default(null)])
            ],[form_data(_)]),
    (Token \= null -> true; throw(http_reply(bad_request(token_missing),[id(Id)]))),
    file_name_extension(_Base,Ext,FilePath),
    (option(api(Allowed),TokenInfo) -> true; throw(http_reply(bad_request(bad_token),[id(Id)]))),
    (tokens:decode_token(Token,_,TokenInfo)->true
        ;
        throw(http_reply(bad_request(bad_token),[id(Id)]))
    ),
    (member(structures,Allowed) -> true; throw(forbidden(structures))),
    (memberchk(Ext, [str,rpt]) ->
        true
        ;
        (
            atomic_list_concat([FilePath,' ','Ilegal extension, only str,rpt.'],Message)
        ,
            throw(http_reply(not_acceptable(Message),[id(Id)]))
        )
    ),
    kleio_resolve_structure_file(FilePath,AbsFile,TokenInfo),
    (
            exists_file(AbsFile) -> true
        ;
            throw(http_reply(not_found(FilePath),[id(Id)]))
    ),
    kleio_user_structure_dir(DataDir,TokenInfo),
    http_reply_from_files(DataDir,[],Request).
 */

%%

%%
% ### API CALL: translations
% Translates a file or set of files. JSON and REST
%
 
%% json_exec(+Method,+Id,+Params,?Results) is det.
%
% Hands over the execution of each method to the relevant server_exec predicate,
% either directly or by posting job to the worker pool. Gets the result.
json_exec(translations,Id,Params,[job(JobId),result(Result)]):-
    kleiofiles:kleio_default_stru(DefaultStru),
    % get user base dirs from token info
    option(token_info(TokenInfo),Params), % token info was put into Params by json_decode_command
    % get path and (optional) stru file
    (option(path(DFile),Params) -> true; throw(error(Id,-32602,'Missing parameter: path'))),
    kleio_resolve_source_file(DFile,DatFile,TokenInfo),
    (option(structure(SFile),Params) ->
        (kleio_resolve_structure_file(SFile,StruFile,TokenInfo),
        (exists_file(StruFile) -> true; throw(error(Id,-32602,'Structure file does not exist'-SFile))))
        ;
        (StruFile=DefaultStru,
        (exists_file(StruFile) -> true; throw(error(Id,-32602,'Default structure file does not exist'-StruFile))))
     ),
    (exists_file(DatFile) -> true; throw(error(Id,-32602,'Source file does not exist '-DFile))),
    post_job(server_exec(translations,[stru_path(StruFile),dat_path(DatFile)|Params],Result),JobId),
    Result = "OK".

% Rest variant, it just branches to the json method.
rest_exec(translations,Id,Params,[job(JobId),result(Result)]):-
    catch(
        json_exec(translations,Id,Params,[job(JobId),result(Result)]),
        E,
        (E=error(Id,-32602,Message-Path),throw(http_reply(resource_error(resource_not_found(Path,Message)),[id(Id)],[translations/3,Message])))
    ).

%%  server_exec(+Method,+Parameters,-Results) is det.
%
% Executes Method given Parameters and returns Results. 
% Method and Parameters where extracted previously from the http request by either process_json_rpc or process_rest. 
% Results will be handled to return_sucess/5 to be returned in the format appropriate to the resuest type.
%



return_sucess(json,translations,Id,Params,Results):-
    current_output(O),
    option(path(Object),Params),
    option(job(JobId),Results),
    Return =json([
        jsonrpc='2.0',
        result="OK",
        "job"=json([
            method=translations,
            object=Object,
            job=JobId
            ]),
        id=Id]),
    json_write(O,Return).

return_sucess(rest,translations,Id,Params,Results):-
    option(path(Object),Params),
    option(job(JobId),Results),
    return_sucess(rest,default,Id,[method(translations),path(Object),job(JobId)],Results)
    ,
    !.

%%
% ### API CALL: update
% Translate new and edited files.
% This combines sources with tstatus=T and translations, to translate all the sources that need to be translated.
%
json_exec(update,Id,Params,Results):-
    server_exec(update,[id(Id)|Params],Results),
    !.

rest_exec(update,Id,Params,Results):-
    catch(
        json_exec(update,Id,Params,Results),
        E,
        (E=error(_,-32602,Message-Path),throw(http_reply(resource_error(resource_not_found(Path,Message)),[id(Id)],[rest_exec/4,Message])))
    ).

server_exec(update,Params,Result):-!,
    option(id(Id),Params,null),
    option(path(Dir),Params),
    option(echo(Echo),Params,no),
    option(token_info(TokenInfo),Params),
    kleiofiles:kleio_default_stru(DefaultStru),
    (option(structure(SFile),Params) ->
        (kleio_resolve_structure_file(SFile,StruFile,TokenInfo),
        (exists_file(StruFile) -> true; throw(error(Id,-32602,'Structure file does not exist'-SFile))),
        StruPar=SFile % this is used to output the parameter later in return_sucess
        )
        ;
        (StruFile=DefaultStru,
        (exists_file(StruFile) -> true; throw(error(Id,-32602,'Default structure file does not exist'-StruFile))),
        StruPar='*Default*')
     ),
    update(Dir,StruPar,StruFile,Echo,TokenInfo,Result).

%! update(+Dir,+Stru,+Excho,+TokenInfo, -Params,-Result) is det.
%
% Translates the files in Dir and all subdirs, in the case they need translation.
%
update(Dir,StruPar,Stru,Echo,TokenInfo,Results):-
    sources_in_dir(Dir,[token_info(TokenInfo),tstatus('T')],Sources),
    sources_to_absolute(Sources,AbsFiles,TokenInfo),
    post_translate_jobs(Echo,Stru,StruPar,Sources,AbsFiles,Results),
    !.

post_translate_jobs(_,_,_,[],[],[]):-!.
post_translate_jobs(Echo,Stru,StruPar, [(Source,_,_,_)|MoreSources], [File|MoreFiles],[Result|MoreResults]):-
    post_job(server_exec(translations,[echo(Echo),path(Source),stru_path(Stru),dat_path(File)],Result),JobId), 
    Result = translation([job(JobId),source(Source),structure(StruPar),echo(Echo)]),
    post_translate_jobs(Echo,Stru,StruPar,MoreSources,MoreFiles,MoreResults).

sources_to_absolute([(File,_,_,_)|More],[AbsFile|More2],TokenInfo):-
    kleio_resolve_source_file(File,AbsFile,TokenInfo),
    sources_to_absolute(More,More2,TokenInfo).

sources_to_absolute([],[],_):-!.

return_sucess(rest,update,Id,Params,Results):-
    (select(request(_),Params,P1);P1=Params),
    (select(http_method(_),P1,ParamsClean);ParamsClean=P1),
    print_content_type(rest),
    length(Results,L),
    format('~w~nId:~w~n~nParams:~n~@~nResults:~ntranslations=~w~n~n~@~n',
        ['OK',Id,
        print_option_list(ParamsClean),L,
        print_update_list(Results)])
    ,!.

return_sucess(json,update,Id,Params,Results):-
    Id \= null,
    option(path(Path),Params,null),
    length(Results,L),
    (Results = [] -> 
        JTranslations=[]
        ;
        bagof(json(T),member(translation(T),Results),JTranslations)
    ),
    Return =json([
        jsonrpc='2.0',
        result=json([translations=L,path=Path,sources=JTranslations]),
        id=Id]),
    current_output(Out),json_write(Out,Return).

print_update_list([S|Sources]):-
    print_translation(S),
    print_update_list(Sources).
print_update_list([]):-!.

print_translation(translation(Options)):-
    print_option_list(Options),nl.


%%
% ### API CALL: clean
% Clean result of translations (files with extension rpt, err, ids, xml).
% parameter: path directory to clean
% If file is queued or processing does not clean
%
json_exec(clean,_Id,Params,Results):-
    server_exec(clean,Params,Results),
    !.

rest_exec(clean,Id,Params,Results):-
    option(path(Path),Params,''),
    catch(
        json_exec(clean,Id,Params,Results),
        E,
        (E=error(Id,-32602,Message-Path),throw(http_reply(resource_error(resource_not_found(Path,Message)),[id(Id)],[rest_exec/4,Message])))
    ).

server_exec(clean,Params,Result):-
    option(path(Path),Params),
    sources_in_dir(Path,Params,Result),
    set_prop(kleio,clean,[]),
    option(token_info(TokenInfo),Params),
    member((File,TCode,PCode, QCode),Result),
    log_debug('clean file candidate:~w~n',[File]),
    TCode \= 'D', PCode \='P', QCode \='Q',
    log_debug('calling ~w~n',[kleio_file_clean(File)]),
    kleio_resolve_source_file(File,AbsFile,TokenInfo),
    kleio_file_clean(AbsFile),
    add_to_prop(kleio,clean,clean(File)),
    fail.

server_exec(clean, _,Result):-
    get_prop(kleio,clean,Result),!.

return_sucess(rest,clean,Id,Params,Result):-
    length(Result,L),
    print_content_type(rest),
    format('~w~nId:~w~n~nParams:~n~@~nResults:~ncleaned=~w~n~@~n',
        ['OK',Id,
        print_option_list(Params),L,
        print_option_list(Result)]).


return_sucess(json,clean,Id,_,Results):-
        Id \= null,
        (Results=[]->
            JsonList=[]
        ;
            bagof(File,(member(clean(File),Results)),JsonList)
        ),
        Return =json([
                jsonrpc='2.0',
                result=JsonList,
                id=Id]),
        current_output(O),
        json_write(O,Return).
%% 
% ### API CALL: kleioset
% Get information on kleio set of files.
%
json_exec(kleioset,Id,Params,Results):-
    option(path(File),Params),
    option(token_info(TokenInfo),Params),
    kleio_resolve_source_file(File,AbsFile,TokenInfo),
    (
        (exists_file(AbsFile); exists_directory(AbsFile)) -> true
    ;
        throw(error(Id,-32602,'File or dir does not exist in user directory'))
    ),
    server_exec(kleioset,Params,Results),
    !.

rest_exec(kleioset,Id,Params,Results):-
    catch(
        json_exec(kleioset,Id,Params,Results),
        E,
        (E=error(Id,-32602,Message-Path),
            throw(http_reply(resource_error(resource_not_found(Path,Message)),[id(Id)],[rest_exec/4,Message])))
    ).

server_exec(kleioset,Params,Result):-
    option(path(File),Params),
    option(token_info(TokenInfo),Params),
    kleio_resolve_source_file(File,AbsFile,TokenInfo),
    kleio_file_set_relative(AbsFile,Result,TokenInfo).

return_sucess(json,kleioset,Id,_,Results):-
    Id \= null,
    [kleio(K),rpt(R),err(E),xml(X),org(O),old(L),ids(I)] = Results,
    JsonObject=json([kleio=json(K),rpt=json(R),err=json(E),xml=json(X),org=json(O),old=json(L),ids=json(I)]),
    Return =json([
        jsonrpc='2.0',
        result=JsonObject,
        id=Id]),
    current_output(Out),json_write(Out,Return).

return_sucess(rest,kleioset,Id,_,Results):-
    [kleio(K),rpt(R),err(E),xml(X),org(O),old(L),ids(I)] = Results,
    print_content_type(rest),
    format('OK~n'),
    format('Id:~w~n',[Id]),
    format('~nkleio:~n'),
    print_option_list(K),
    format('~nrpt:~n'),
    print_option_list(R),
    format('~nerr:~n'),
    print_option_list(E),
    format('~nxml:~n'),
    print_option_list(X),
    format('~norg:~n'),
    print_option_list(O),
    format('~nold:~n'),
    print_option_list(L),
    format('~nids:~n'),
    print_option_list(I),!.



%%
% ### API CALL: rmdir remove dir
% Remove a directory. Also availabe as REST DELETE directories/PATH
%
json_exec(rmdir, Id, Params,Results):-
    server_exec(rmdir,[id(Id)|Params],Results),
    !.
  
server_exec(rmdir,[id(Id)|Params],Results):-
    rmdir([id(Id)|Params],Results).

rest_exec(directories,Id,Params,Results):-
    option(http_method(delete),Params),
    catch(
        rmdir([id(Id)|Params],Results),
        E,
        (
        (E=error(Id,-32602,Message-Path)->throw(http_reply(resource_error(resource_not_found(Path,Message)),[id(Id)],[rest_exec/4,Message])));
        (E=error(Id,-32003,Message-Path)->throw(http_reply(bad_request(directory_not_empty(Path)),[id(Id)],[rest_exec/4,Message])))
        )
        ).

%! rmdir(+Params,-Results) is det.
%
% Removes a directory. Used by Json-RPC `rmdir` and REST DELETE /directories/PATH
%
% Params:
% * id(Id): id of the request or null if any, is used for error generation and returned with results.
% * path(Path): path to the directory to be removed.
% * structure(yes/no): if `yes` the path refers to the user structure directory
% * contents(yes/no) : if `yes`the directory is removed even if not empty. if `no` removal will fail if directory not empty. Beware of invisible files
% like those created by the MacOs finder or git.
%
rmdir([id(Id)|Params],Results):-
    (option(path(Path),Params) -> true; throw(error(_,-32602,'Missing parameter: path'))),
    option(token_info(TokenInfo),Params), % token info was put into Params by json_decode_command
    option(structure(IsStructureDir), Params, no),
    option(contents(DeleteContents), Params, no),
    (IsStructureDir='yes' ->
        kleio_resolve_structure_file(Path,AbsPath,TokenInfo)
        ;
        kleio_resolve_source_file(Path,AbsPath,TokenInfo)
    ),
    (exists_directory(AbsPath) -> 
        true
        ; 
        throw(error(Id,-32602,'Directory does not exist '-Path))
    ),
    (DeleteContents = yes -> 
        delete_directory_and_contents(AbsPath)
        ; 
        catch(delete_directory(AbsPath),Error,
            (log_error('delete_directory error:~w',[Error]),
            throw(error(Id,-32003,'Delete failed. Directory not empty?'-Path))))
    ),
    Results=[id(Id),path(Path)].



return_sucess(json,rmdir,Id,_,Results):-
    Id \= null,
    [File] = Results,
    Return =json([
        jsonrpc='2.0',
        result=File,
        id=Id]),
    current_output(Out),json_write(Out,Return).

%%
% ### API CALL: mkdir create dir
% Create a directory. Also available as a POST request to rest/directories
%
json_exec(mkdir, Id, Params,Results):-
    server_exec(mkdir,[id(Id)|Params],Results),
    !.
  
server_exec(mkdir,[id(Id)|Params],Results):-
    mkdir([id(Id)|Params],Results).

rest_exec(directories,Id,Params,Results):-
    option(http_method(post),Params),
    catch(
        mkdir([id(Id)|Params],Results),
        E,
        (
        (E=error(Id,-32005,Message-Path)->throw(http_reply(bad_request(directory_exists(Message,Path)),[id(Id)],[mkdir/2,Message])))
        ;
        (E=error(Id,-32004,Message-Path)->throw(http_reply(bad_request(directory_create_fail(Message,Path)),[id(Id)],[mkdir/2,Message])))

        )
        ).


mkdir([id(Id)|Params],Results):-
    (option(path(Path),Params) -> true; throw(error(_,-32602,'Missing parameter: path'))),
    option(token_info(TokenInfo),Params), % token info was put into Params by json_decode_command
    option(structure(IsStructureDir), Params, no),
    (IsStructureDir='yes' ->
        kleio_resolve_structure_file(Path,AbsPath,TokenInfo)
        ;
        kleio_resolve_source_file(Path,AbsPath,TokenInfo)
    ),
    (exists_directory(AbsPath) -> 
        throw(error(Id,-32005,'Directory already exists '-Path))
        ; 
        true
    ),
    catch(make_directory_path(AbsPath),Error,
        (log_error('make_directory_path:~w',[Error]),
        throw(error(Id,-32004,'Could not create directory. Check error log.'-Path))))
    ,
    Results=[id(Id),path(Path)].

return_sucess(json,mkdir,Id,_,Results):-
    Id \= null,
    option(path(Path),Results,null),
    Return =json([
        jsonrpc='2.0',
        result=Path,
        id=Id]),
    current_output(Out),json_write(Out,Return).


%%
% ## Error processing for json and rest
%


%% return_error(+Format,Error) is det.
%
%  formats an error in REST-JSON-RPC and returns to the client. Format can be JSON or REST.
%
% _JSON-RPC Errors_
%  If Error is error(Id,Code,ErrorInfo) then error is output with Code, Id of the calling request and ErrorInfo
%  If not Code is set to -32000 and Id to null.
% _REST Errors_
%
% REST errors are generated using the builtin exception http_reply/1. 
% See http://www.swi-prolog.org/pldoc/man?section=httpserver
%
% For the standard Status codes see https://restfulapi.net/http-status-codes/
% For the way the builtin exception http_reply/1 generates them see http://www.swi-prolog.org/pldoc/man?predicate=http_status_reply/4
%
% Example:
%
%       throw(http_reply(not_found(FilePath)))
%
%   Status can be one of the following:
%      - authorise(Method) 401
%        Challenge authorization.  Method is one of
%        - basic(Realm)
%        - digest(Digest)
%      - authorise(basic,Realm)
%        Same as authorise(basic(Realm)).  Deprecated.
%      - bad_request(ErrorTerm) 400
%      - busy 202. Accepted, but it will take time to complete. Should include pointer to status
%      - created(Location) 201
%      - forbidden(Url) 403 Does not have necessary permissions
%      - moved(To) 301
%      - moved_temporary(To) 307
%      - no_content 204
%      - not_acceptable(WhyHtml) 406 cannot generate preferred mime type as in Accept header
%      - not_found(Path) 404
%      - method_not_allowed(Method, Path) 405 resource does not allow method (PUT to r/o)
%      - not_modified 304 resource not modified since requested headers If-Modified-Since or If-None-Match
%      - resource_error(ErrorTerm)
%      - see_other(To) 303
%      - switching_protocols(Goal,Options)
%      - server_error(ErrorTerm) 500
%      - unavailable(WhyHtml)
%
% Note: ErrorTerm above is a term like:
%   * error(MessageName) or error(MessageName,context(predicate/arity,ContextInfo)) where MessageName is an ISO Prolog Error (see http://fsl.cs.illinois.edu/images/9/9c/PrologStandard.pdf p.8) 
%   * A term associated with a message defined by prolog:message//1 see bellow.
%
% IMPORTANT note that swi uses throw(http_reply(....)) to generate output
% which is not an error. See code for http_reply_file for an example. So 
% we need to pass the exception if it matches http_reply(.....). Only if it does
% we have to hande it and wtap a http_reply(....) around for output
%
% The fact that throw(http_reply(..)) is used for output inside swipl httpd 
% is very confusing and caused great pain during developmente before it was figured out.
% See http://localhost:4040/pldoc/man?section=html-body
return_error(rest,HTTP_ERROR):-
    HTTP_ERROR =.. [http_reply|_], !,% if http_reply just re-throw, builtin server takes care
    %
    throw(HTTP_ERROR).

return_error(rest,E):- % this is a system error
    with_output_to(string(M),write(E)),
    log_error(M,[]),
    throw(http_reply(server_error(server_error(M)))).


%
% Codes in the Json-RPC spec:
%
% | _code_ | _message_        | _meaning_                                     |
% | -32700 | Parse error      | Invalid JSON was received by the server.      |
% | -32600 | Invalid Request  | The JSON sent is not a valid Request object.  |
% | -32601 | Method not found | The method does not exist / is not available. |
% | -32602 | Invalid params   | Invalid method parameter(s).                  |
% | -32603 | Internal error   | Internal JSON-RPC error.                      |
% | -32000 to -32099 | Server error | Reserved for implementation-defined server-errors. |
% | -32000 | Generic server error | Normally internal prolog error not expected |
% | -32001 | Invalid token Info | The token was recognized by the server but the associated information is invalid |
% | -32002 | Resources not found | Resource (file or directory) not found |
% | -32003 | Directory not empty | Attempt to delete a non empty directory |
% | -32004 | Could not create directory | Atempt to create directory failed |
% | -32005 | Directory exists | Atempt to create directory failed |
% | -32006 | Forbidden        | same as REST error 403 |
% | -32007 | Destination exists | resource already exists cannot be created by copy or move |
% | -32008 | Resource not found | same as REST error 404 |
% | -32009 | Directory does not exist  | Operation requires a directory that does not exist |
% | -32010 | Could not copy directory | Attempt to copy directory failed |
%
%

return_error(json,http_reply(forbidden(Url),Headers,[])):-
    option('Request-id'(Id),Headers,null),
    atomic_list_concat(['Forbidden - insufficient privilegis for ',Url],Message),
    json_error_output(-32006,Message,Id),!.

return_error(json,
                http_reply(bad_request(destination_file_exists(File)),
                Headers,_Context)):-
    option('Request-id'(Id),Headers,null),                   
    atomic_list_concat(['Destination of copy or move exists: ',File],Message),
    json_error_output(-32007,Message,Id),!.


return_error(json,http_reply(not_found(Path), Headers,_Context)):- 
    option('Request-id'(Id),Headers,null),                   
    atomic_list_concat(['Resource not found: ',Path],Message),
    json_error_output(-32008,Message,Id),!.   

return_error(json,http_reply(
                    bad_request(directory_not_exists(Directory)),
                    Headers,_Context)):- 
                    option('Request-id'(Id),Headers,null),                   
                    atomic_list_concat(['Directory does not exist: ',Directory],Message),
                    json_error_output(-32009,Message,Id),!.   

return_error(json,http_reply(Error)):-
    return_error(json,http_reply(Error,[],[])).

return_error(json,http_reply(Error,Headers)):-
    return_error(json,http_reply(Error,Headers,[])).


return_error(json,parse_error(Id,ErrorMessage)):-!,
    with_output_to(string(M),write(ErrorMessage)),    
    atomic_list_concat(['Parse Error: ',M], Message),
    json_error_output(-32700,Message,Id).

return_error(json,invalid_request(Id,ErrorMessage)):-!,
    with_output_to(string(M),write(ErrorMessage)),    
    atomic_list_concat(['Invalid request: ',M], Message),
    json_error_output(-32600,Message,Id).

return_error(json,method_not_found(Id,ErrorMessage)):-!,
    with_output_to(string(M),write(ErrorMessage)),    
    atomic_list_concat(['Method not found: ',M], Message),
    json_error_output(-32601,Message,Id).

return_error(json,invalid_params(Id,ErrorMessage)):-!,
    with_output_to(string(M),write(ErrorMessage)),    
    atomic_list_concat(['Invalid params: ',M], Message),
    json_error_output(-32602,Message,Id).

return_error(json,internal_error(Id,ErrorMessage)):-!,
    with_output_to(string(M),write(ErrorMessage)),    
    atomic_list_concat(['Internal error: ',M], Message),
    json_error_output(-32603,Message,Id).

return_error(json,server_error(Id,Code,ErrorMessage)):-!,
    with_output_to(string(M),write(ErrorMessage)),    
    atomic_list_concat(['Server error: ',M], Message),
    json_error_output(Code,Message,Id).

return_error(json,error(Id,param_error(ErrorMessage))):-!,
    with_output_to(string(Message),write(ErrorMessage)),
    json_error_output(-32602,Message,Id).

return_error(json,error(Id,Code,Error)):-!,
    with_output_to(string(Message),write(Error)),
    json_error_output(Code,Message,Id).

return_error(json,Error):-!,
    with_output_to(string(Message),write(Error)),
    json_error_output(-32000,Message,null).

json_error_output(Code,Message,Id):-
    current_output(O),
    Error=
        json([
            jsonrpc='2.0',
            error=json([
                code=Code,
                message=Message]),
            id=Id]),
    json_write(O,Error).

%% message templates for errors
%
% example: throw(http_reply(bad_request(bad_token))) 
% whenever ErrorTerm is used in the http_reply error descriptions above
%
prolog:message(token_missing) -->
    [ 'Request does not contain token',nl,'Use rest/generate_token to obtain a valid token'].

prolog:message(bad_token) -->
    [ 'Bad token in request', nl,
        'Use rest/generate_token to obtain a valid token'
    ].
prolog:message(bad_method(M,P)) -->
    [ 'Bad method in request' , P, ': ', M].


prolog:message(wrong_file_extension(F)) -->
    [ 'Wrong filename: ',F,' Only files with extensions "str","cli" and "kleio" are allowed for upload. ',F, nl,
        'Other file formats must be produced by the translation process.'
    ].

prolog:message(resource_not_found(F,M)) -->
    [ 'Resource not found ',F, nl,
        'Context:',M
    ].

prolog:message(directory_not_exists(P)) -->
    [ 'Bad path, directory does not exists: ',P
    ].

prolog:message(directory_not_empty(P)) -->
    [ 'Delete failed, directory empty?: ',P
    ].

prolog:message(directory_exists(M,P)) -->
    [ 'Directory exists ',P, nl,
        'Context:',M
    ].

prolog:message(directory_create_fail(M,P)) -->
    [ 'Could not create directory ',P, nl,
        'Context:',M
    ].

prolog:message(bad_file_upload) -->
    [ 'A file upload must be submitted as multipart/form-data using ',
        'name=file and providing a file-name ','and path to upload directory (relative to user sources or structure dir'
    ].

prolog:message(upload_file_exists(F)) -->
    [ 'Upload would overwrite existing file ',F,
        ' Use method PUT or delete existing file first.'
    ].

prolog:message(destination_file_exists(F)) -->
    [ 'Destination file exists ',F,
        ' Delete destination first.'
    ].

prolog_message(server_error(M)) -->
    ['Unexpected server error: ',M].

% UTILTIES FOR REST

%% convert_to_options(+List,-OptionList) is det.
%
% Converts any term of type option=value in List to option(value) in OptionList.
% If list contains option(value) terms they are copied to OptionList.
% 
% This is useful when dealing with different formats of options or parameters in
% rest requests, because swi builtin functions produce different results from 
% forms, get parameters, and json requests.
%
% Example:
%
%       convert_to_options([option1=value1, options(value2)],Options).
%       Options = [option1(value1), options(value2)] 
%
convert_to_options([A=B|Rest],[T|Rest2]):-
    T =.. [A,B],
    convert_to_options(Rest,Rest2).

convert_to_options([A|Rest],[A|Rest2]):-
    convert_to_options(Rest,Rest2).

convert_to_options([],[]):-!.

%%
% ## Reporting on the processing queue.
%

%% ps is det.
%
% Synonym show_processing status.
% @see sa,
%
ps:-show_processing_status.


%% show_processing_status is det.
%
% Show processing queue.
% @see ps,
%
show_processing_status:-
        get_time(T),
        format_time(atom(Time),'%Y-%m-%d %H:%M:%S',T),
        format('RUNNING    : ~w~n',[Time]),
        get_queued(Queue),
        length(Queue,Q),
        format('QUEUED JOBS: ~w~n',[Q]),
        log_debug('QUEUED JOBS:~w',[Q]), 
        member(Proc,Queue),
        show_proc(Proc),
        fail.
show_processing_status:-
        get_processing(Processing),
        length(Processing,P),
        format('IN PROCESS: ~w~n',[P]),
        log_debug('IN PROCESS: ~w',[P]),
        member(Proc,Processing),
        show_proc(Proc),
        fail.
show_processing_status:-get_shared_value(pool_mode,pool),show_pool,!.
show_processing_status.

show_proc(queued(Q,G)):-
        option(time(T),G),
        format_time(atom(Time),'%y-%m-%d %H:%M:%S',T),
        option(job(Job),G),
        show_job('QUEUED',Q,Time,Job,''),
        !.
show_proc(processing(Q,G)):-
        option(time(T),G),
        format_time(atom(Time),'%y-%m-%d %H:%M:%S',T),
        option(job(Job),G),
        option(job_number(N),G),
        show_job('PROCESSING',Q,Time,Job,number(N)),
        !.

show_job(Status,Q,Time,server_exec(translations,Options,_Result),number(N)):-
        option(path(Path),Options),
        format('~w: ~w ~w translating ~w [~w]~n',[Status,Q,Time,Path,N]),!.
show_job(_,_,_,translate([],_Stru,_Echo),_):-!.
show_job(Status,Q,Time,translate([Source|Sources],_Stru,_Echo),_):-   
        show_job(Status,Q,Time,translate(Source,_,_),_),
        show_job(Status,Q,Time,translate(Sources,_,_),_),!.
show_job(Status,Q,Time,translate(Source,_Stru,_Echo),_):-
        atomic(Source),
        kleiofiles:kleio_home_dir(H),
        relative_file_name(Source,H,P),
        format('~w: ~w ~w ~w~n',[Status,Q,Time,P]),!.
show_job(Status,Q,Time,Job,Extra):-
        format('~w: ~w ~w ~@ ~w~n',[Status,Q,Time,print_term(Job,[]),Extra]),!.



%%
% ## Testing
%

rest_test(Request):-
    format('Content-type: text/html~n~n', []),
    format('<html>~n', []),
    prepare_report('/Users/jrc/mhk-tests/swi_report.txt'),
    report(([nl,write('Processing data file:'),write('filename'),nl,
                  writeln('-------------------------------------------')])),
    telling(S),
    tell('/Users/jrc/mhk-tests/swi_report.txt'),
    nl,
    echo_request(Request),
    told,
    tell(S),
    writeln('<hr>'),
    format('<h2>Current stream</h2>', []),
    writeln(S),
    writeln('<hr>'),
    format('</html>~n').

echo_request(Request) :-
    option(method(options), Request), !,
    cors_enable(Request,
            [ methods([get,post,delete,put])
            ]),
    format('~n'). 

echo_request(Request) :-
        format('Content-type: text/html~n~n', []),
        format('<html><body>Echoing request~n', []),
        format('<table border=1>~n'),
        print_request(Request),
        format('~n</table>~n'),
        member(path_info(P),Request),
        format('<h2>~w</h2>',[P]),
        format('<table border=1>~n'),
        print_streams;
        format('~n</table>~n'),
        format('</body></html>~n', []).

print_request([]).
print_request([H|T]) :-
        H =.. [Name, Value],
        format('<tr><td>~w<td>~w~n', [Name, Value]),
        print_request(T).

print_streams:-
        format('Streams~n', []),
        current_stream(O,M,S),
        format('~w~10|~w~36|~w~n', [M,S,O]),
        fail.
print_streams:-!,
        format('~n</table>~n').

rest_api_call(In,In):-!.

