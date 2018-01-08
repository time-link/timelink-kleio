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
:- put_value(stru_dir,'/Users/jrc/develop/mhk-git/clio/src/').
:- put_value(data_dir,'/Users/jrc/develop/mhk-git/mhk_users/').
:- put_value(echo,yes).
:- put_value(max_errors,1000).

check_command_line :-
	check_arg(strufile,SF),
	check_arg(datafile,DF),
	check_arg(echo,Echo),
	put_value(echo,Echo),
	pwd,
	trad1(DF,SF),
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
     trad(D,'gacto2.str').


traduzir D estrutura E :-
	trad(D,E).

traduzir D estrutura S :-
	writeln('Translating '-D-' with '-S-'failed.').

trad1(DatFile,StruFile):-
    writeln(structure-StruFile),
    break_fname(StruFile,P,_File,Base,_),
    put_value(stru_dir,P),
	% chdir(P),
    list_to_a0([Base,'.','rpt'],Report),
    prepare_report(Report),
    stru(StruFile),
    close_report_file,
    writeln(data-DatFile),
    put_value(data_file,DatFile),
    break_fname(DatFile,PD,_FileD,BaseD,_),
	 put_value(data_dir,PD),
    % chdir(PD),
	 path_sep(Sep),
    list_to_a0([PD,Sep,BaseD,'.','rpt'],ReportD),
    prepare_report(ReportD),
    dat(DatFile),
    list_to_a0([PD,Sep,BaseD,'.','err'],ReportErrors),
    prepare_report(ReportErrors),
    report([perror_count]),
    close_report_file,!.

trad_stru_only(StruFile):-
    writeln(structure-StruFile),
    break_fname(StruFile,P,_File,Base,_),
    put_value(stru_dir,P),
	% chdir(P),
	path_sep(Sep),
    list_to_a0([P,Sep,Base,'.','rpt'],Report),
    prepare_report(Report),
    stru(StruFile),
    close_report_file,!.

trad_dat_only(DatFile):-
    writeln(data-DatFile),
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

:-check_command_line.

