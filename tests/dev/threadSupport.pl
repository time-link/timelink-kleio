
:- module(threadSupport,
        [ 
        create_workers/1,
        post_job/2,
        show_threads/0,
        show_pool/0,
        get_queued/1,
        get_processing/1
        ]). 


/** <module> threadSupport
* 
*Creates a work pool and dispatch jobs to workers.
*
*@author Joaquim Carvalho
*@license MIT
*/
:-use_module(library(pprint)).
:-use_module(counters).
:-use_module(logging).
:-use_module(restServer). % maybe not needed in the V2
:-use_module(apiCommon).
:-use_module(persistence).

:-dynamic(processing/2).
:-dynamic(queued/2).


:-put_shared_value(pool_mode,message). % "pool" to use thread_pool_create, "message" to use message queue, "debug" to execute directly

%%      create_workers(+N) is det.
%
%       Create a pool of workers.
%       After the pool is created, post_job/1 can be used to
%       send jobs to the pool.
% 
%  @see http://localhost:4444/pldoc/man?section=msgqueue
%
create_workers(N):-
        set_shared_count(jobs,1),
        set_shared_count(queued,1),
        set_shared_count(processing,1),
        (get_shared_value(pool_mode,Mode) -> true ; Mode = pool),
        put_shared_value(pool_mode,Mode),
        create_workers(Mode,N).

create_workers(message,N):-
        message_queue_create(_,[alias(jobs)]),
        forall(between(1, N, _),
               thread_create(do_work, _, [])).

create_workers(pool,N):-
        thread_pool_create(translators,N,[local(60000), global(2000000), trail(80000),
        backlog(10)]).

create_workers(debug,_):-
        log_debug('DEBUG Mode, not creating any workers',[]).

do_work:-
        repeat,
                thread_get_message(jobs, exec_goal(Goal)),
                exec_goal(Goal),
        fail .

exec_goal(queued(Q,Goal)):-
        (inc_shared_count(jobs,N) -> true ; (N=1,set_shared_count(jobs,N))),
        % remove from queued
        thread_self(ThreadId),
        get_time(Time),
        (retract(queued(Q,_)) 
        -> true 
        ;  log_debug('>> [Queue job: ~w] Problems removing queued register ~w in thread ~w ~n',[Q,ThreadId])
        ),
        log_info('=================================================',[]),
        log_debug('>> [JOB ~w] exec goal :- ~w.~n',[Q,Goal]),
        log_debug('=================================================',[]),
        working_directory(DD,DD),
        %log_debug('>> [JOB ~w] current dir in Thread ~w dir:~w~n',[Q,ThreadId,DD]),
        % push info that this job is being processed
        %(inccount(processing,P) -> true ; (P=1,setcount(processing,P))),
        assert(processing(Q,[thread(ThreadId),time(Time),job_number(Q),job(Goal)])),
        %log_debug('>> [JOB ~w] asserted: ~w~n',[Q,processing(Q,[thread(ThreadId),time(Time),job_number(Q),job(Goal)])]),                
        %sleep(5),
        (   catch(Goal, E, (
                log_debug('>> [exec_goal] ~@',[print_term(Goal,[])]),
                log_error('>> [JOB ~w] ERROR ~w',[Q,E]))
                )
        ->  (true)
        ;  log_error('>> [JOB ~w] Thread: ~w, Goal failed:~w',[Q,ThreadId, Goal])
        ),
        % pop info of processing status.
        (retract(processing(Q,_)) 
        -> true 
        ;  log_debug('>> [JOB ~w] Problems removing processing register ~w in thread ~w ~n',[Q,N,ThreadId])
        ),
        log_debug('>> [JOB ~w] done with message: ~w~n',[Q,print_term(job(N,Goal))]).


%%      post_job(+Id, +Goal) is det.
%
%   Post a job to be executed by one of the pool's workers.
%   @see http://localhost:4444/pldoc/man?section=msgqueue
%
post_job(Goal,Q):-
        inc_shared_count(queued,Q),
        thread_self(ThreadId),
        get_time(Time),
        assert(queued(Q,[thread(ThreadId),time(Time),job(Goal)])),
        get_shared_value(pool_mode,Mode),
        post_job_(Mode,queued(Q,Goal)),!.

post_job_(debug,Goal):-
        exec_goal(Goal),!.

post_job_(message,Goal):-
        thread_send_message(jobs,exec_goal(Goal)),!.

post_job_(pool,Goal):-
        thread_create_in_pool(translators,exec_goal(Goal),_ThreadId,[wait(true)]),!.

%% show_threads is det.
% show running threads in a loop.
%
show_threads:-repeat,threads,sleep(5),fail.

%% show_pool is det.
%  Show info on pool of threads running.
%
show_pool:-thread_pool_property(Name, Property),writeln(Name-Property),fail.
show_pool:-!.

%% get_queued(-Jobs) is det.
% Show list of queued jobs. Queued jobs are waiting for a free thread to execute
%
get_queued(L):-
        bagof(queued(Q,G),clause(queued(Q,G),true),L),!.
get_queued([]).

%% get_processing(-Jobs) is det.
% Show list of jobs currently executing.
%
get_processing(L):-
        bagof(processing(P,JobData),clause(processing(P,JobData),true),L),!.
get_processing([]).



