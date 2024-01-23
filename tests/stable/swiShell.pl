:- module(swishell,
    [
        trad_stru_only/1,
        make_kleio_doc/2,
        trad_dat_only/1,
        trad/2,trad1/2

    ]).
% vim: filetype=prolog
% ClioInput quick and dirty shell para o SWI prolog
%
% Joaquim Carvalho Lousa Novembro de 99
%
%
%
% trailing "/" or "\" on dir names please.
%
:-use_module('swiCompatibility').
:-use_module('utilities').
:-use_module('topLevel').
:-use_module(persistence).
:-use_module(reports).
:-use_module(dataDictionary). % needed to make_html_doc

default_value(stru_dir,SD):- getenv(kleio_stru_dir,SD),!.
default_value(stru_dir,'~/clio/src/'):-!.
default_value(data_dir,SD):- getenv(kleio_data_dir,SD),!.
default_value(data_dir,'~/mhk-home/sources/'):-!.


set_defaults :-
        default_value(stru_dir,SD),put_value(stru_dir,SD),
        default_value(data_dir,DD),put_value(data_dir,DD),
        put_value(echo,yes),
        put_value(max_errors,1000).

check_command_line :-
	check_arg(strufile,SF),
	check_arg(datafile,DF),
	check_arg(echo,Echo),
    put_value(echo,Echo),
    format('Kleio commando line paramenters, stru ~w, file ~w, echo ~w~n~n',[SF,DF,Echo]),
	pwd,
	trad1(DF,SF),
	halt.

check_command_line:-!.
%	writeln('usage: swipl -f swiShell.pl -- [-sf StruFile] -df DataFile [-echo (yes|no)]').


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEBUGGER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! trad1(+DatFile,+StruFile) is det.
%  translates DatFile using strucutre StuFile
%
trad1(DatFile,StruFile):-
    writeln(structure-StruFile),
    break_fname(StruFile,P,_File,Base,_),
    put_value(stru_dir,P),
	% chdir(P),
    list_to_a0([Base,'.','srpt'],Report),
    prepare_report(Report),
    stru(StruFile),
    close_report_file,
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
    break_fname(StruFile,P,_File,Base,_),
    put_value(stru_dir,P),
	% chdir(P),
	path_sep(Sep),
    list_to_a0([P,Sep,Base,'.','srpt'],Report),
    prepare_report(Report),
    stru(StruFile),
    close_report_file,!.

make_kleio_doc(StruFile,DestDir):-
    break_fname(StruFile,P,_File,Base,_),
    put_value(stru_dir,P),
	% chdir(P),
	path_sep(Sep),
    list_to_a0([P,Sep,Base,'.','srpt'],Report),
    prepare_report(Report),
    topLevel:doc(StruFile,DestDir),
    close_report_file,!.

trad_dat_only(DatFile):-
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
    break_fname(StruFile,P,__File,Base,_),
    chdir(P),
    list_to_a0([Base,'.','srpt'],Report),% TODO: file_name_extension(Base,'srpt',Report).
    prepare_report(Report),
    stru(StruFile),
    close_report_file,
    get_value(data_dir,Ddir),
    concat(Ddir,D,DatFile),
    put_value(data_file,DatFile),
    break_fname(DatFile,PD,__FileD,BaseD,_),
    chdir(PD),
    list_to_a0([BaseD,'.','rpt'],ReportD),% TODO: file_name_extension(BaseD,'rpt',ReportD).
    prepare_report(ReportD),
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
    list_to_a0([Base,'.','srpt'],Report),
    writeln('Preparing to report to file '-Report),
    prepare_report(Report),
    writeln('structure file is '-File),
    writelist(['Processing structure file',File]),
    stru(StruFile),
    close_report_file.


layout(voyager):-
		put_value(data_dir,'C:\\WINDOWS\\Profiles\\joaquim\\My Documents\\develop\\mhk.users\\'),
		put_value(stru_dir,'C:\\WINDOWS\\Profiles\\joaquim\\My Documents\\develop\\rch\\clio\\src\\').

layout(linuxppc) :-
		put_value(data_dir,'/home/jrc/develop/mhk/mhk_users/'),
		put_value(stru_dir,'/home/jrc/develop/mhk/trans/rch/clio/src/').

layout(macosx) :-
		put_value(data_dir,'/Users/jrc/develop/timelink-kleio/tests/kleio-home/sources/api/linked_data/'),
		put_value(stru_dir,'/Users/jrc/develop/timelink-kleio/tests/kleio-home/structures/').
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

