:-use_module(restServer).
:-use_module(utilities).
:-use_module(persistence).
:-use_module(logging).

%% run_debug_server is det.
%
% Activate a rest server and a debug server.
% Sets environment variables for the local tests directory
% This should be run from clio top level directory
% 
run_debug_server:-
    set_log_level(debug),
    
    print_server_config,
    set_prop(prolog_server,options,[allow(ip(_,_,_,_))]), % TODO: limit by subnet (first two numbers of host'sIP)
    %restServer:start_debug_server, 
    restServer:start_rest_server,!.

%% run_server is det.
% Activate a rest server.
run_server:-
    set_prop(prolog_server,options,[allow(ip(_,_,_,_))]), % TODO: limit by subnet (first two numbers of host's IP)
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
% Sets up a test server with the environment set for running the test suites in `clio/test`. 
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
    writeln('RUN FROM THE clio/tests DIRECTORY'),
    working_directory(WD,WD),
    (atom_concat(_,'tests/',WD)->true;throw(bad_directory('run from tests directory'))),
    (exists_directory('kleio-home')->true
        ;
        throw(error(bad_directory(WD),context(run_test_server/0,'no kleio-home directoy')))),
    
    setenv_dir('KLEIO_HOME_DIR',WD,'kleio-home'),
    setenv_dir('KLEIO_SOURCE_DIR',WD,'kleio-home/sources'),
    setenv_dir('KLEIO_CONF_DIR',WD,'kleio-home/system/conf/kleio'),
    setenv_dir('KLEIO_STRU_DIR',WD,'kleio-home/system/conf/kleio/stru'),
    setenv_dir('KLEIO_TOKEN_DB',WD,'kleio-home/system/conf/kleio/token_db'),
    setenv_dir('KLEIO_DEFAULT_STRU',WD,'kleio-home/system/conf/kleio/stru/gacto2.str'),
    setenv('KLEIO_DEBUGGER_PORT',4000),
    setenv('KLEIO_SERVER_PORT', 8088),
    setenv('KLEIO_WORKERS',3),
    setenv('KLEIO_IDLE_TIMEOUT',360), 
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
do_setup(dstru(V)):-setenv('KLEIO_DEFAULT_STRU',V),!.
do_setup(dport(V)):-setenv('KLEIO_DEBUGGER_PORT',V),!.
do_setup(port(V)):-setenv('KLEIO_SERVER_PORT',V),!.
do_setup(workers(V)):-setenv('KLEIO_WORKERS',V),!.

%% stop_server is det.
%
% Stops currently running server.
stop_server:-
    restServer:default_value(rest_port,Port),
    thread_httpd:http_stop_server(Port,[]),!.