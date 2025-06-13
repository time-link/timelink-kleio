:- module(counters,
    [
        setcount/2,
        getcount/2,
        inccount/2,
        deccount/2,
        set_shared_count/2,
        get_shared_count/2,
        inc_shared_count/2,
        dec_shared_count/2
    ]).
:-use_module('utilities').
:-use_module('persistence').

/** <module> Counters

Utility predicates to deal with counters.
Counters are local to threads.
Shared counters are shared between all threads.

*/

%% setcount(+COUNTER,+VALUE) is det.
%
%  Initializes a thread local counter, VALUE must be an integer.
%
setcount(COUNTER,VALUE):-
    atom_concat(COUNTER,'_counter_',C_NAME),
    put_value(C_NAME,VALUE),!.

 %% getcount(+COUNTER,?VALUE) is det.
 %
 %  Get current value of a counter.
 %
 getcount(COUNTER,VALUE):-
    atom_concat(COUNTER,'_counter_',C_NAME),
    get_value(C_NAME,VALUE).

 %% inccount(+COUNTER,?VALUE) is det.
 %
 %  Increment counter, return previous value.
 %
 inccount(COUNTER,VALUE):-
    getcount(COUNTER,VALUE),
    V is VALUE+1,
   setcount(COUNTER,V),!.

inccount(COUNTER,VALUE,0):-
    \+ getcount(COUNTER,VALUE),
   setcount(COUNTER,0),!.

 %% deccount(+COUNTER,?VALUE) is det.
 %
 %   Decrement value, return previous value.
 %
 deccount(COUNTER,VALUE):-
    getcount(COUNTER,VALUE),
    V is VALUE-1,
    setcount(COUNTER,V),!.

%% set_shared_count(+COUNTER,+VALUE) is det.
%
%  Initializes a thread local counter, VALUE must be an integer.
%
set_shared_count(COUNTER,VALUE):-
    atom_concat(COUNTER,'_counter_',C_NAME),
    put_shared_value(C_NAME,VALUE),!.

 %% get_shared_count(+COUNTER,?VALUE) is det.
 %
 %  Get current value of a counter.
 %
 get_shared_count(COUNTER,VALUE):-
    atom_concat(COUNTER,'_counter_',C_NAME),
    get_shared_value(C_NAME,VALUE).

 %% inc_shared_count(+COUNTER,?VALUE) is det.
 %
 %  Increment counter, return previous value.
 %
 inc_shared_count(COUNTER,VALUE):-
    get_shared_count(COUNTER,VALUE),
    V is VALUE+1,
   set_shared_count(COUNTER,V),!.

 %% dec_shared_count(+COUNTER,?VALUE) is det.
 %
 %   Decrement value, return previous value.
 %
 dec_shared_count(COUNTER,VALUE):-
    get_shared_count(COUNTER,VALUE),
    V is VALUE-1,
    set_shared_count(COUNTER,V),!.
