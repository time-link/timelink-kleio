% vim: filetype=prolog
% ClioInput quick and dirty shell para o SWI prolog
%
% Joaquim Carvalho Lousa Novembro de 99
%
%
:-op(211,xfy,estrutura).
:-op(230,fx,traduzir).
%
% trailing "/" or "\" on dir names please.
%
:-ensure_loaded(utilities).
:-ensure_loaded(compatibilitySWI).


default_value(stru_dir,SD):- getenv(kleio_stru_dir,SD),!.
default_value(stru_dir,SD):- working_directory(SD,SD),!.
default_value(data_dir,DD):- getenv(kleio_data_dir,DD),!.
default_value(data_dir,DD):- working_directory(DD,DD),!.


set_defaults :-
        default_value(stru_dir,SD),put_value(stru_dir,SD),
        default_value(data_dir,DD),put_value(data_dir,DD),
        put_value(echo,yes),
        put_value(max_errors,1000).

check_command_line :-
    writeln('************************************'),
	check_arg(strufile,SF),
	check_arg(datafile,DF),
	check_arg(echo,Echo),
    put_value(echo,Echo),
    format('Kleio commando line parameters, stru ~w, file ~w, echo ~w~n~n',[SF,DF,Echo]),
	pwd,
	trad1(DF,SF,Echo),
	halt.

check_command_line:-!.
%	writeln('usage: swipl -f clioStart.pl -- [-sf StruFile] -df DataFile [-echo (yes|no)]').


check_arg(strufile,SF) :-
	current_prolog_flag(argv,Args),
	append(_,['-sf',SF | _],Args).

check_arg(strufile,'gacto2.str').

check_arg(datafile,DF) :-
	current_prolog_flag(argv,Args),
	append(_,['-df',DF | _],Args).

check_arg(echo,E) :-
	current_prolog_flag(argv,Args),
	append(_,['-echo',E | _],Args).

check_arg(echo,yes).

traduzir D :-
    get_value(echo,Echo),
     trad1(D,'gacto2.str',Echo).


traduzir D estrutura E :-
    get_value(echo,Echo),
	trad1(D,E,Echo).

traduzir D estrutura S :-
	writeln('Translating '-D-' with '-S-'failed.').

% for debugging in VS Code
start:-
    clio_init,
    trad1('../test_translations/bapteirasproblem2.cli','gacto2.str').

trad1(DFile,SFile,Echo):-
    clio_init,
    put_value(echo,Echo), % clio_init sets echo to no. Should it?
    writeln(structure-SFile),
    default_value(stru_dir,SDIR), 
    default_value(data_dir,DDIR), 
    atom_concat(SDIR,SFile,StruFile),
    atom_concat(DDIR,DFile,DatFile),
    break_fname(StruFile,SPath,_SFile,SBase,_),
    path_sep(Sep),
    list_to_a0([SPath,Sep,SBase,'.','rpt'],Report),
    put_value(stru_dir,SPath),
    prepare_report(Report),
    stru(StruFile),
    close_report_file,
    writeln(data-DatFile),
    put_value(data_file,DatFile),
    break_fname(DatFile,PD,_FileD,BaseD,_),
	 put_value(data_dir,PD),
    % chdir(PD),
    list_to_a0([PD,Sep,BaseD,'.','rpt'],ReportD),
    prepare_report(ReportD),
    dat(DatFile),
    list_to_a0([PD,Sep,BaseD,'.','err'],ReportErrors),
    prepare_report(ReportErrors),
    report([perror_count]),
    close_report_file,!.

trad_stru_only(SFile):-
    clio_init,
    writeln(structure-StruFile),
    default_value(stru_dir,SDIR), 
    atom_concat(SDIR,SFile,StruFile),
    break_fname(StruFile,P,_File,Base,_),
    put_value(stru_dir,P),
	% chdir(P),
	path_sep(Sep),
    list_to_a0([P,Sep,Base,'.','rpt'],Report),
    prepare_report(Report),
    stru(StruFile),
    close_report_file,!.

trad_dat_only(DFile):-
    clio_init,
    writeln(data-DatFile),
    default_value(data_dir,DDIR), 
    atom_concat(DDIR,DFile,DatFile),
    put_value(data_file,DatFile),
    break_fname(DatFile,PD,_FileD,BaseD,_),
	 put_value(data_dir,PD),
    % chdir(PD),
	 path_sep(Sep),
    list_to_a0([PD,Sep,BaseD,'.','rpt'],ReportD),
    prepare_report(ReportD),
    dat(DatFile),
    close_report_file,
    list_to_a0([PD,Sep,BaseD,'.','err'],ReportErrors),
    prepare_report(ReportErrors),
    report([perror_count]),
    close_report_file,!.

trad(D,E):-
    clio_init,
    get_value(stru_dir,Sdir),
    concat(Sdir,E,StruFile),
    writeln(structure-StruFile),
    break_fname(StruFile,P,File,Base,_),writeln('changing dir to '-P),
    chdir(P),writeln('Dir changed.'),
    list_to_a0([Base,'.','rpt'],Report),
    writeln('Preparing to report to file '-Report),
    prepare_report(Report),
    writeln('structure file is '-File),
    writelist(['Processing structure file',File]),
    stru(StruFile),
    close_report_file,
    get_value(data_dir,Ddir),
    concat(Ddir,D,DatFile),
    writeln(data-DatFile),
    put_value(data_file,DatFile),
    break_fname(DatFile,PD,FileD,BaseD,_),
    chdir(PD),
    list_to_a0([BaseD,'.','rpt'],ReportD),
    prepare_report(ReportD),
    writelist(['Processing data file',FileD]),
    dat(DatFile),
     path_sep(Sep),
    list_to_a0([PD,Sep,BaseD,'.','err'],ReportErrors),
    prepare_report(ReportErrors),
    report([perror_count]),   close_report_file,!.


do_stru(E) :-
    pclio_version,
    get_value(stru_dir,Sdir),
    concat(Sdir,E,StruFile),
    writeln(structure-StruFile),
    break_fname(StruFile,P,File,Base,_),writeln('changing dir to '-P),
    chdir(P),writeln('Dir changed.'),
    list_to_a0([Base,'.','rpt'],Report),
    writeln('Preparing to report to file '-Report),
    prepare_report(Report),
    writeln('structure file is '-File),
    writelist(['Processing structure file',File]),
    stru(StruFile),
    close_report_file.


prepare_report(R):-
   set_report_file(R),
   set_report(on),
   report([pclio_version]),!.


%*****************************************************
% threading support.
% when acessing prolog via  prolog_server/2
% it is necessary to synchronize access because
% all threads share the same database and so only two
% different translations cannot occur at the same time
% TODO reimplement this using queues see http://www.swi-prolog.org/pldoc/man?section=threadcom
%*****************************************************

% we save the server_thread so only it can change busy/idle/status
set_server_thread :- thread_self(Trd),put_value(server_thread,Trd),
                     writeln('Warning: only thread creating server should use this predicate.').
set_busy :- get_time(T),with_mutex(busy,put_value(busy,(true,T))),!.

set_idle :- with_mutex(busy,put_value(busy,false)),!.


busy_for(S) :- is_busy,get_time(T2),with_mutex(busy,get_value(busy,(true,T1))), S is T2-T1,!.
busy_for(0) :- is_idle.

is_busy :- with_mutex(busy,get_value(busy,(true,_))).
is_idle :- with_mutex(busy,get_value(busy,false)).
grab_if_idle :- with_mutex(busy,(is_idle,set_busy)).

server_status(busy) :- is_busy.
server_status(idle) :- is_idle.

% waits for N seconds, otherwise fails.
call_when_idle(_,G) :- grab_if_idle,exec_and_free(G),!.
call_when_idle(N,G) :- is_busy, get_time(WaitSince),thread_self(Trd),put_value(Trd,WaitSince),wfidle(N,G).

exec_and_free(G) :- catch(G,T,(write('ERROR: '),writeln(T),set_idle)),set_idle,!.
exec_and_free(_) :- set_idle,!.

wfidle(_,G) :-  grab_if_idle,exec_and_free(G).
wfidle(N,G) :-  thread_self(Trd),
                get_value(Trd,Since),
                get_time(Now),
                Seconds is Now-Since ,
                Seconds @< N, sleep(2), writeln('waiting'-Seconds-less-N-to-G),wfidle(N,G),!.



%******************************************************
%  pclio_version prints version,
%    compiler version, date and time
%******************************************************
%  %
pclio_version:-
		  clio_version(P),writeln(P),
		  get_time(T),
		  convert_time(T,Year,Month,Day,Hour,Min,_,_), D = Day-Month-Year,
		  write(D),
		  Time = Hour-Min,
		  tab(1),write(Time),
		  nl,
		  !.

layout(voyager):-
		put_value(data_dir,'C:\\WINDOWS\\Profiles\\joaquim\\My Documents\\develop\\mhk.users\\'),
		put_value(stru_dir,'C:\\WINDOWS\\Profiles\\joaquim\\My Documents\\develop\\rch\\clio\\src\\').

layout(linuxppc) :-
		put_value(data_dir,'/home/jrc/develop/mhk/mhk_users/'),
		put_value(stru_dir,'/home/jrc/develop/mhk/trans/rch/clio/src/').

layout(macosx) :-
		put_value(data_dir,'/Users/jrc/develop/mhk_users/testes/sources/'),
		put_value(stru_dir,'/Users/jrc/develop/mhk-git/clio/src/').
layout(windows) :-
        		  put_value(data_dir,'../mhk_users/'),
        		  put_value(stru_dir,'../clio/src/').
cdweb:-
    cd('/Users/jrc/develop/apache-tomcat/webapps/mhk/WEB-INF/clio/src'),pwd.
cddev:-
    get_value(stru_dir,S),
    cd(S),pwd.





%:-pclio_version.

%:-set_defaults.
:-check_command_line.

