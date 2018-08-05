%*****************************************************
% Threading support.
% when acessing prolog via  prolog_server/2
% it is necessary to synchronize access because
% all threads share the same database and so only one
% translations can occur at the same time.
%
% clients request translations with tkleio(File|Filelist)
% for each file to be translated tkleio wait either 300 secs
% or get_value(thread_max_wait_secs,S) S seconds
% If the server is not available after that time tkleio fails.
%
%*****************************************************

% we save the server_thread so only it can change busy/idle/status
set_server_thread :- thread_self(Trd),put_value(server_thread,Trd),
                     writeln('Warning: only thread creating server should use this predicate.').


set_busy :- thread_self(Trd),
            get_time(T),
            with_mutex(busy,
                put_value(busy,(true,T,Trd))),!.

set_idle :- with_mutex(busy,put_value(busy,false)),!.


busy_for(S,Trd) :- is_busy,get_time(T2),with_mutex(busy,get_value(busy,(true,T1,Trd))), S is T2-T1,!.
busy_for(0,_) :- is_idle.

is_busy :- with_mutex(busy,get_value(busy,(true,_,_))).
is_idle :- with_mutex(busy,get_value(busy,false)).
grab_if_idle :- with_mutex(busy,(is_idle,set_busy)).

server_status(busy) :- is_busy.
server_status(idle) :- is_idle.

% waits for N seconds, otherwise fails.
call_when_idle(_,G) :- grab_if_idle,exec_and_free(G),!.
call_when_idle(N,G) :- is_busy, get_time(WaitSince),thread_self(Trd),put_value(Trd,WaitSince),wfidle(N,G).

exec_and_free(G) :- catch(G,T,(write('ERROR: '),writeln(T),set_idle)),set_idle,sleep(6),!.
exec_and_free(_) :- set_idle,random(2,6,S),sleep(S),!.

wfidle(_,G) :-  grab_if_idle,exec_and_free(G),!.
wfidle(N,G) :-  thread_self(Trd),
                get_value(Trd,Since),
                get_time(Now),
                Seconds is round(Now-Since),
                Seconds @< N, sleep(2), writeln('waiting'-Seconds-less-N-to-G),wfidle(N,G),!.
wfield(N,G):-
        write('ERROR: could execute '),
        write(G),
        write(after),
        write(N),
        writeln(' seconds.'),
        !,
        fail.

% interface to translations
tkleio([F|R]):-
    tkleio(F),
    tkleio(R).
tkleio([]).
tkleio(F):-
    atomic(F),
    (get_value(thread_max_wait_secs,S);S=300),
    call_when_idle(S,trad_dat_only(F)).
