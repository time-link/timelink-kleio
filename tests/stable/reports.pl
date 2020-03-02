:-module(reports,
    [
        prepare_report/1,
        prepare_report/2,
        report/1,
        report_status/1,
        set_report/1,
        close_report_file/0
    ]).

:- meta_predicate rep_call(0).

:-use_module(persistence).
:-use_module(utilities).
:-use_module(topLevel). 
:-use_module(errors).
:-use_module(logging).

/** <module> Reports
 
 # Reporting

 Sending text to a disk file and optionally to the console
 at the same time.
*/
%% prepare_report(+ReportFile) is det.
% Prepare reporting system to send output to file and console.
% Output to ReportFile is done with report/1 predicate.
% It is the same as prepare_report(ReportFile,[type(console)]).
% This is kept for compatibility since originally all output of 
% 
% @see: prepare_report/2 for full documentation
%
prepare_report(R):-prepare_report(R,[type(console)]).

%% prepare_report(+ReportFile,+Options) is det.
% Prepare reporting system to send output to file ReportFile.
%
% Output to ReportFile is done with the report/1 predicate.
% If options includes type(console) the output is also sent to the current output.
% When reporting is done ReportFile must be closed with close_report_file/0
%
% @see report/1
% @see set_report/1
% @see close_report_file/0
% @param    Options A list that includes
%           * type(console) if present output is also sent to the current output stream.
%           * type(noconsole) for legibility can be used to signal that output to the console is not desired. In reality it is the presence of the type(console) that regulates output to the console.
%
prepare_report(R,Options):-
    option(type(Type),Options,console),
    put_value(report_type,Type),
    set_report_file(R),
    set_report(on),
    report_header(Type),
    !.

report_header(_):-
    report([pclio_version]),!.

set_report_file(FILENAME):-
    open_file_write(FILENAME),
    put_value(report,FILENAME),
    set_report(off),!.

%% set_report(+OnOFF) is det.
%  Turn reporting on and off.
%  
% @param OnOFF if on, subsequent calls to report/1 will output to file set in prepare_report/2.
%
set_report(on):-put_value(rep_stat,on),!.
set_report(off):-put_value(rep_stat,off),!.
set_report(X):- \+ var(X), X \= on, X \=off, writeln('** ERROR: Bad value for set_report'-X),!.

%% report_status(?OnOFF) is det.
%  returns current status for report status set by set_report/1
%
report_status(X):- var(X),get_value(rep_stat,X),!.
report_status(X):- var(X), X=off,!.
report_status(X):- get_value(rep_stat,X),!.

%%   report(+PREDICATE_LIST) is det.
%    Executes the predicates in PREDICATE_LIST directing output to the
%    file set in prepare_report/2. If type(console) was included as an option
%    in prepare_report/2 then the output is also sent to the console.
%   
% @see prepare_report/2
%
report(L):-report_status(X), X=off,repexec(L),!.
report(L):-
    report_status(on),
    %log_debug('report 1 call  : ~w~n',[L]),
    with_output_to(string(S),repexec(L)),
    %log_debug('[report] ~s~n',[S]),
    get_value(report,FILENAME),
    %log_debug('report 3 tofile:~w~n',[FILENAME]),
    write(FILENAME,S),
    %current_output(Stream),
    %current_stream(Sid,Smode,Stream),
    %log_debug('report 5 touser: stream ~w ~w ~w',[Sid,Smode,Stream]),
    (get_value(report_type,console) -> format('~s',S);true),% also output to console
    %log_debug('report 6       : done!',[]),
    flush_output(FILENAME),
    !.
report(L):-
    log_error('ERROR: Problems in "report". Predicate list ~w~n ',[L]),
    % TODO: how to deal with error contions
    format('ERROR: Problems in "report". Predicate list ~w~n ',[L]),!.
repexec([]):-!.
repexec([A|B]):-!,
                rep_call(A),
                repexec(B).
repexec(A):- !, rep_call(A).

%% close_report_file is det.
% Closes current report file.
% 
% @see prepare_report/2
% @see report/1
%
close_report_file:-
    get_value(report, FILENAME),
    debug(kleio(report),'Closing file ~w~n',FILENAME), close_file(FILENAME),!.

%% rep_call(+Predicate) is nondet.
%
%  Wrapper for call meta predicate for modular use 
% see http://www.swi-prolog.org/pldoc/man?section=metapred
%
rep_call(M:P):-
    %log_debug('Report meta call. Module: ~w, Predicate: ~w',[M,P]),
    catch(M:P,Error,(write('** ERROR IN REPORT ARGUMENT'-Error))).

