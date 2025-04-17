:-use_module(restServer).
:-use_module(utilities).
:-use_module(persistence).
:-use_module(logging).
% for tests
:-use_module(library(http/http_client)).
:-use_module(library(http/http_open)).
:-use_module(library(http/json)).
:-use_module(library(http/json_convert)).
% to run tests do run_tests(server).
:- set_test_options([run(manual)]).

%% run_debug_server is det.
%
% Activate a rest server and a debug server.
% Sets environment variables for the local tests directory
% This should be run from clio top level directory
%
run_debug_server:-
    set_log_level(debug),
    log_debug("serverStart:run_debug_server starting server with log level debug.",[]),
    print_server_config,
    log_debug("serverStart:run_debug_server server config printed",[]),
    set_prop(prolog_server,options,[allow(ip(_,_,_,_))]), % TODO: limit by subnet (first two numbers of host'sIP)
    restServer:start_debug_server, % we keep this for the semantic tests scripts until we find a better way
    restServer:start_rest_server,!.

%% run_from_mhk_home(?MHK_HOME, ?PORT) is det.
%
% runs a server in a given MHK_HOME and PORT
% This is best way to set up a server for testing with
% with mhk sever running in paralel and sharing a mhk-home
% if MHK_HOME is unbound it defaults to $HOME/mhk-home
% if PORT is unbound it defaultls to 8088
% if mhk-home/system/conf/mhk_system_properties set
% mhk.kleio.service=http://host.docker.internal:8088
% if in docker or
% mhk.kleio.service=http://localhost:8088
% if mhk is running outside docker.

run_from_mhk_home:-
    run_from_mhk_home(_,_).

run_from_mhk_home(MH,P):-
    mhk_home(MH),
    (var(P)->P=8088;true),
    writeln('Starting server in '-MH-P),
    run_server.

%% run_server is det.
% Activate a rest server.
run_server:-
    restServer:start_rest_server,
    print_server_config,!.

%% run_server_forever is det.
%
% Activate a rest server and hold the thread.
run_server_forever:-
    %logging:open_log(current_output), Currently not working
    (getenv('KLEIO_DEBUG',true)->
        (run_debug_server,sleep_forever)
        ;
        (run_server, sleep_forever)
    ).

sleep_forever:-repeat, sleep(300),fail.


%% debug_server_until_idle is det.
%
% Activates the server; quit if idle for 60 seconds.
%
debug_server_until_idle:-
    restServer:default_value(timeout,Seconds),
    print_server_config,
    run_debug_server,
    wait_for_idle(Seconds).

%% wait_for_idle(+Secs) is det.
%
% This predicate will wait for the server to be idle for Secs seconds.
% Idle means no running jobs and no queued jobs.
%
% Can be used to auto quit the server application if not used for a period of time.
% Used by testing shell scripts:
%
%      swipl -f dev/serverStart.pl -g run_server_until_idle -t halt
%
wait_for_idle(Secs) :- repeat,format('Waiting ~w seconds for server idle',[Secs]),restServer:server_idle(Secs).
wait_for_idle(Secs) :- log_info('Rest Server idle for ~w seconds',[Secs]).

%% run_test_server is det.
%
% Sets up a test server with the environment set for running the test suites in `tests`.
% It is useful to simulate the test environment.
%
% This assumes that the server is started from a tests directory with the following internal layout
%
%
% $ KLEIO_HOME_DIR        : working dir.
% $ KLEIO_SOURCE_DIR      : test_sources
% $ KLEIO_CONF_DIR        : conf/kleio
% $ KLEIO_STRU_DIR        : conf/kleio/stru/
% $ KLEIO_TOKEN_DB        : conf/kleio/token_db
% $ KLEIO_DEBUGGER_PORT   : 4000
% $ KLEIO_SERVER_PORT     : 8088
% $ KLEIO_WORKERS         : 6
% $ KLEIO_IDLE_TIMEOUT    : 60
%
run_test_server:-
    writeln('RUN FROM THE ./tests DIRECTORY'),
    working_directory(CurrentDir,CurrentDir),
    (atom_concat(_,'tests/',CurrentDir)->  % check if it is tests
        true
    ;
        (exists_directory('tests') ->  % if not in test move to tests
            working_directory(_,'tests')
        ;
            throw(bad_directory('run from tests directory'))
        )
    ),
    (exists_directory('kleio-home')->true
        ;
        throw(error(bad_directory(WD),context(run_test_server/0,'no kleio-home directoy')))),
    working_directory(WD,WD),
    log(debug,'serverStart:run_test_server starting with kleio home: ~w ~n',[WD]),
    setenv_dir('KLEIO_HOME_DIR',WD,'kleio-home'),
    setenv_dir('KLEIO_SOURCE_DIR',WD,'kleio-home/sources'),
    setenv_dir('KLEIO_CONF_DIR',WD,'kleio-home/system/conf/kleio'),
    setenv_dir('KLEIO_STRU_DIR',WD,'kleio-home/system/conf/kleio/stru'),
    setenv_dir('KLEIO_TOKEN_DB',WD,'kleio-home/system/conf/kleio/token_db'),
    setenv_dir('KLEIO_DEFAULT_STRU',WD,'kleio-home/system/conf/kleio/stru/gacto2.str'),
    % setenv('KLEIO_DEBUGGER_PORT',4000),
    setenv('KLEIO_SERVER_PORT', 8088),
    setenv('KLEIO_WORKERS',3),
    setenv('KLEIO_IDLE_TIMEOUT',360),
    setenv('KLEIO_ADMIN_TOKEN',mytoken),
    run_debug_server.

setenv_dir(Var,Dir,Value):-
    atomic_list_concat([Dir,Value], V),
    setenv(Var,V).

%% setup_and_run_server(ServerPredicate+,SetupInfo) is det.
%
% ServerPredicate is the name of the predicate to run the server, e.g.
%  run_server/0, run_server_forever/0, run_test_server/0, debug_server_until_idle/0,
%  etc.
%
% SetupInfo is a list of setup instructions. Currently recognized setup instructions are:
%
% $ env(variable,value) : set environment variable before lunch
% $ home(path) : path to kleio_home_dir
% $ sources(path): path to kleio_sources_dir
% $ strus(path) : path to kleio_stru_dir
% $ conf(path)  : path to kleio_conf_dir
% $ tokens(Path): path to token_db
% $ dstru(S) : path to default stru
% $ dport(Number) : debug port (not all server run predicates activate the debugger)
% $ port(Number): main serer port.
% $ workers(Number): number of workers
%
%
setup_and_run_server(RunCommand,Setup):-
    setup_each(Setup),
    call(RunCommand).

setup_each(Setup):-
    member(S,Setup),
    do_setup(S),
    fail.

setup_each(_):-!.


do_setup(env(Var,Val)):-setenv(Var,Val),!.
do_setup(home(H)):-setenv('KLEIO_HOME_DIR',H),!.
do_setup(source(P)):-setenv('KLEIO_SOURCE_DIR',P),!.
do_setup(conf(V)):-setenv('KLEIO_CONF_DIR',V),!.
do_setup(strus(V)):-setenv('KLEIO_STRU_DIR',V),!.
do_setup(tokens(V)):-setenv('KLEIO_TOKEN_DB',V),!.
do_setup(kleio_admin_token(V)):-setenv('KLEIO_ADMIN_TOKEN',V),!.
do_setup(dstru(V)):-setenv('KLEIO_DEFAULT_STRU',V),!.
% do_setup(dport(V)):-setenv('KLEIO_DEBUGGER_PORT',V),!.
do_setup(port(V)):-setenv('KLEIO_SERVER_PORT',V),!.
do_setup(workers(V)):-setenv('KLEIO_WORKERS',V),!.

%% stop_server is det.
%
% Stops currently running server.
stop_server:-
    restServer:default_value(rest_port,Port),
    thread_httpd:http_stop_server(Port,[]),!.

%% stop_debug_server is det.
%
% Stops currently running debug server on port 4000.
stop_debug_server:-
    restServer:default_value(server_port,Port),
    thread_httpd:http_stop_server(Port,[]),!.

%% mhk_home(?Path) is det.
%
% Set or infer mhk-home path and
% change working directory to it.
% if Path is not und then it will
% be bound 'mhk-home' dir user_home (getenv('HOME'))
% If bound change working dir to Path
%
mhk_home(MH):-
    var(MH),
    getenv('HOME',H),
    atom_concat(H,'/mhk-home',MH),
    working_directory(_,MH),
    ls.
mhk_home(MH):-
    \+ var(MH),
    working_directory(_,MH),
    ls.


% Test zone

% testing processing of "escritura" which is taking a very long time.

test_escritura:-
    set_log_level(debug),
    restServer:translate(
            './tests/kleio-home/sources/api/varia/auc_cartulario18.cli',
            './tests/kleio-home/system/conf/kleio/stru/gacto2.str',
            yes).
test_error_out:-
    set_log_level(debug),
    restServer:translate(
            './tests/kleio-home/sources/api/notariais/docsregiospontepisc.cli',
            './tests/kleio-home/system/conf/kleio/stru/gacto2.str',
            yes).
% testing other tricky c
test_ucalumni:-
    set_log_level(debug),
    restServer:translate(
            './tests/kleio-home/sources/api/varia/auc-alunos-264605-A-140337-140771.cli',
            './tests/kleio-home/system/conf/kleio/stru/gacto2.str',
            yes).
test_ivcc:-
    set_log_level(debug),
    restServer:translate(
            './tests/kleio-home/sources/api/varia/ivcc.cli',
            './tests/kleio-home/system/conf/kleio/stru/gacto2.str',
            yes).

test_quotes:-
    set_log_level(debug),
    restServer:translate(
            './tests/kleio-home/sources/api/varia/quotes.cli',
            './tests/kleio-home/system/conf/kleio/stru/gacto2.str',
            yes).



show_prolog_stack:-
    member(StackType,[local,global,trail]),
    prolog_stack_property(StackType,min_free(MF)),
    prolog_stack_property(StackType,low(Low)),
    prolog_stack_property(StackType,factor(Factor)),
    prolog_stack_property(StackType,spare(Spare)),
    format('Stack ~12w ~22t min_free: ~w low: ~w factor: ~w spare: ~w~n',[StackType, MF,Low,Factor,Spare]),
    fail.
show_prolog_stack:-!.


%%%%%%%%%%%%%%%%%%%%%%%
%  tests of restServer
%%%%%%%%%%%%%%%%%%%%%%%
% To run tests do:
%    run_tests(server).
%
% assumes test sources in tests/kleio-home/reference_sources
%
test_setup(EndPoint,Token):-
    working_directory(CD,CD),
    put_value(dir_before_tests,CD),
    source_file(run_test_server,ThisFile),
    file_directory_name(ThisFile,SourceDir),
    file_directory_name(SourceDir,ClioDir),
    concat(ClioDir,'/tests/',TestPath),
    format('Test dir: ~w~n',[TestPath]),
    working_directory(_,TestPath),
    PORT=8090,
    Token = 'mytoken',
    setenv('KLEIO_SERVER_PORT', PORT),
    setenv('KLEIO_ADMIN_TOKEN',Token),
    setenv('KLEIO_DEBUGGER_PORT',0),
    catch(run_debug_server,E,writeln(E)),
    concat('http://localhost:',PORT,EndPoint),
    (delete_test_sources(EndPoint,Token);true),
    (copy_test_sources(EndPoint,Token);true).

test_cleanup:-
    get_value(dir_before_tests,CD),
    catch(stop_server,_,true),
    working_directory(_,CD).

rest_call(Protocol,Host,Method, Function,Path, Params,Token, Accept,Response,Status):-
    make_uri(Protocol,Host,Function,Path,Params,Uri),
    http_get(Uri, Response,
        [   method(Method),
            authorization(bearer(Token)),
            request_header(accept=Accept),
            status_code(Status)]).

%
make_uri(S,H,F,P,Par,Uri):-
    params_to_query(Par,Qs),
    atomic_list_concat(['/rest/',F,'/',P],Path),
    uri_components(Uri,uri_components(S,H,Path,Qs,_)).
params_to_query(ParamList,QueryString):-
    uri_query_components(QueryString, ParamList).

server_response(json([id=Id,jsonrpc=Version,result=Results]),Id,Version,Result):-
    server_results(Results,Result),!.
server_response(Response,id,version,Response).

server_results([R|Rs],[D|Ds]):-
    R=json(Data),
    dict_create(D,result,Data),
    server_results(Rs,Ds).
server_results([],[]).

test_case(translations,File,Stru):-
    Stru = 'system/conf/kleio/stru/gacto2.str',
    translate_file(File,Flag),
    Flag = true,
    format('TESTING ~wn',[File]).

translate_file('sources/api/linked_data/dehergne-a.cli',false).
translate_file('sources/api/linked_data/multiplelinks.cli',false).
translate_file('sources/api/paroquiais/obitos/ob1688.cli',false).
translate_file('sources/api/bugs/bugs.cli',false).
translate_file('sources/api/linked_data/dehergne-locations-1644.cli',false).
translate_file('sources/api/linked_data/linked-datanw.cli',false).
translate_file('sources/api/varia',false).
translate_file('sources/api/issues/issue21.cli',false).
% the next two go together
translate_file('sources/api/issues/issue34.cli',false).
translate_file('sources/api/issues/issue34b.cli',false).
translate_file('sources/api/issues/issue38/issue38.cli',false).
translate_file('sources/api/issues/issue1/issue1.cli',true).
translate_file('sources/api/issues/issue10/issue10.cli',false).

translate_file('sources/api/varia/lrazao516pe.cli',false).


translate_file('sources/api/paroquiais/baptismos/bap-com-celebrantes.cli',false).
translate_file('sources/api/varia/cartas.cli',false).
translate_file('sources/api/notariais/docsregiospontepisc.cli',false).
translate_file('sources/api/paroquiais/baptismos/bapteirasproblem1.cli',false).
translate_file('sources/api/paroquiais/baptismos/bapt1714.cli',false).
translate_file('sources/api/paroquiais/baptismos/',false).
translate_file('sources/api/notariais/docsregiospontepisc.cli',false).
translate_file('sources/api/varia/lrazao516pe.cli',false).
translate_file('sources/api/varia/ivcc.cli',false).
translate_file('sources/api/notariais/docsregiospontepisc.cli',false).
translate_file('identifications/mhk_identification_toliveira.cli',false).
translate_file('sources/api/varia/test-atr-date.cli',false).

delete_test_sources(EndPoint,Token):-
    uri_components(EndPoint,UComponents),
    uri_data(scheme,UComponents,Scheme),
    uri_data(authority,UComponents,Host),
    rest_call(Scheme,Host,'DELETE',directories,'sources/api',[id=1212,force=yes],Token,'application/json',Response,Status),
    server_response(Response,Id,Version,Results),
    writeln('OK got answer'-Id-Version-Status),
    print_term(Results,[]),!.
copy_test_sources(EndPoint,Token):-
    uri_components(EndPoint,UComponents),
    uri_data(scheme,UComponents,Scheme),
    uri_data(authority,UComponents,Host),
    rest_call(Scheme,Host,'POST',directories,'sources/api',[id=1212,origin='sources/reference_sources'],Token,'application/json',Response,Status),
    server_response(Response,Id,Version,Results),
    writeln('OK got answer'-Id-Version-Status),
    print_term(Results,[]),!.


:-begin_tests(server).

test(translations,[
            setup(test_setup(EndPoint,Token)),
            forall(test_case(translations,File,Stru)),
            cleanup(test_cleanup)]):-
    uri_components(EndPoint,UComponents),
    writeln(UComponents),
    uri_data(scheme,UComponents,Scheme),
    writeln(Scheme),
    uri_data(authority,UComponents,Host),
    writeln(Host),
    rest_call(Scheme,Host,'POST',translations,File,[id=1212,str=Stru],Token,'application/json',Response,Status),
    server_response(Response,Id,Version,Results),
    writeln('OK got answer'-Id-Version-Status),
    print_term(Results,[]),!.
    %dict_create(Dict,response,Data),
    %format('~n~nid:~w~n,~k',[Dict.id,Dict]).
:- end_tests(server).