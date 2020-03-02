:-module(logging,
    [
        log/3,
        log_emergency/2,
        log_alert/2,
        log_critical/2,
        log_error/2,
        log_warning/2,
        log_notice/2,
        log_debug/2,
        log_info/2,
        set_log_level/1,
        start_log/1,
        stop_log/0,
        log_levels/1,
        get_log_level/1
    ]).

:-use_module(persistence).
:-use_module(utilities).
:-use_module(kleioFiles).
:-use_module(library(prolog_stack)).
:-use_module(library(pprint)).

log_levels([emerg, alert, crit, err, warning, notice, info, debug]).

start_log(Destination):- 
    open_log(Destination).

stop_log:-close_log.

log_emergency(Format,Args) :- log(emerg,Format,Args).
log_alert(Format,Args) :- log(alert,Format,Args).
log_critical(Format,Args) :- log(crit,Format,Args).
log_error(Format,Args) :- 
    log(err,Format,Args).
    %,
    %get_prolog_backtrace(6,BT),
    %log(err,'Backtrace: ~@~n',[print_term(BT,[])]).

%% get_caller(?Predicate) is det.
% 
% get the name of the predicate that called this one
% Same as get_caller(1,Predicate).
%
get_caller(MP):-
    get_caller(1,MP),!.

%% get_caller(?Up,?Predicate) is det.
%
% get a predicate up in the calling sequence.
% if Up = 1 then the precicate that called get_caller is returned.
% TODO: this does not work reliably
%
get_caller(LevelAbove,Predicate):-
    get_prolog_backtrace(20,B),
    print_term(B,[]),nl,
    print_prolog_backtrace(user, B),nl,
    member_nth(MyFrame,B,N),
    frame(_N,_MyClause,logging:get_caller(_,_)) = MyFrame,
    member_nth(Frame,B,N2),
    N2 is N + LevelAbove,
    (frame(_N2,_Clause,_Module:Caller) = Frame;
    frame(_N2,_Clause,Caller) = Frame),
    Caller =.. [Predicate|_].
    
%% get caller tests

first:-
    log_info('primeiro',[]),
    second.

second:-
    log_info('segundo',[]),
    third.

third:-
    log_info('terceiro',[]),
    quarto.

quarto:-
    log_info('quarto',[]).

log_warning(Format,Args) :- log(warning,Format,Args).
log_notice(Format,Args) :- log(notice,Format,Args).
log_debug(Format,Args) :- log(debug,Format,Args).
log_info(Format,Args) :- log(info,Format,Args).

set_log_level(Level) :-
    log_levels(Levels),
    member_nth(Level,Levels,Priority),
    put_shared_value(log_level,Priority),
    !.

set_log_level(Level):-
    throw(error(ilegal_log_level,context(set_log_level/1,Level))).
    
log(Level,Format,Args):-
    check_openlog,
    log_levels(Levels), member_nth(Level,Levels,Priority),
    get_log_level(Current),
    (Priority @=< Current 
        -> (
        get_time(T),
        format_time(atom(Time),'%Y-%m-%d %H:%M:%S',T),
        % get_caller(3,Caller), % only works in debug mode
        Caller='',
        atomic_list_concat(['~w [~w] ~w:',Format,'~N'], Format2),       
        logging(Format2, [Time,Level,Caller|Args]))
        ;
        %writeln(log-lowPriority-Priority-Current),
    true),!.
log(_,Format,Args):-log(notice,Format,Args),!.   

logging(Format,Args):-
    get_shared_prop(log,alias,Alias),
    format(Alias,Format,Args),
    flush_output(Alias),
    !.
    % syslog(Level, Format2, Args)) % this seems not to work reliably



get_log_level(Level):-
    get_shared_value(log_level,Level),!.
get_log_level(5):-!. % default is notice

check_openlog:-
    get_shared_prop(log,open,yes),!.
check_openlog:-open_log(_).

open_log(Destination):-
    (var(Destination) -> 
        (kleio_log_dir(L),
        make_directory_path(L),
        atom_concat(L,'/kleio_service.log',Destination))),
    Alias = logfile,
    open(Destination,append,_AStream,[alias(Alias)]),
    set_shared_prop(log,open,yes),
    set_shared_prop(log,alias,Alias),
    set_shared_prop(log,file,Destination),!.

close_log:-
    get_shared_prop(log,open,yes),
    get_shared_prop(log,file,File),
    close(File),
    get_shared_prop(log,open,no).
