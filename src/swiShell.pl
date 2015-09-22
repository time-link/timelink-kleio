% vim: filetype=prolog
% ClioInput quick and dirty shell para o SWI prolog
% $Id$
% $Date$
% $Author$
% $Log: swiShell.pl,v $
% Revision 1.2  2006/05/16 10:52:51  Joaquim
% update dos ficheiros prolog
%
% Revision 1.2  2006/03/27 20:38:02  jrc
% Default value for command line argument for the str is now gacto2.str
%
% Revision 1.1  2004/04/08 14:45:24  ltiago
% Source code has passed from Joaquim to Tiago.
% Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
% File build.xml, web.xml and velocity.properties were changed
%
% Revision 1.2  2002/02/10 19:52:33  jrc
% Added the production of a .err file with just the number of errors detected to allow the MHK web application to easily check the result of a translation. Added separate predicates to process the strutucture and the data files, so that the structure is processed once for several datafiles.
%
% Revision 1.1.1.1  2001/04/19 23:25:43  jrc
% New Repository.
%
% Revision 1.1.1.1  2001/04/18 23:34:39  jrc
% CVS repository moved from llinux to MacOSX.
%
% Revision 1.14  2001/03/25 19:27:46  jrc
% fixed report file path problem. Now it stis next to the data file.
%
% Revision 1.13  2001/03/06 06:37:29  jrc
% Laypout for MacOSX
%
% Revision 1.12  2001/03/06 06:31:06  jrc
% Changed the way the command line execution of
% translations (trad1/2) delat with directories. Behaviour now is more robust in what regards relative path names passed as arguments.
%
% Revision 1.11  2001/02/14 04:47:01  jrc
% Minor correction on variables names.
%
% Revision 1.10  2001/02/13 15:01:29  jrc
% Corrected argument fetching from command line. Tested from command line.
%
% Revision 1.9  2001/02/13 05:02:28  jrc
% It can now be launched from the command line.
%
% Revision 1.8  2001/01/27 13:49:55  jrc
% Modified default path for structure file so that it works by default
% on the development system.
%
% Revision 1.7  2001/01/27 13:13:07  jrc
% Corrected default value for data_dir so that it works on the development
% system without calling the layout predicate to set up the dirs.
%
% Revision 1.6  2001/01/26 14:06:46  jrc
% Interim version with element and group mapping, and overall xml structure
% as specific.
%
% Still missing: infered relations, same_as treatment, object mapping (no
% RCH class defined for it) and consolidation listings. Output of group to class mappings not yet done.
%
% Known issues: iso chars in xml code must cleaned or encoding declared
%  (kleio group?), bad implementation of relation-type pseudo-groups.
%
% Revision 1.5  2001/01/18 21:22:56  jrc
% Added layout clause for linuxppc (jrc).
%
% Revision 1.4  2001/01/14 23:09:52  jrc
% Layout predicate added.
%
% Revision 1.3  2001/01/14 00:45:26  jrc
% Cleaned version can handle casamentos. More files under source control.
% Minor bugs cleaned in various prolog files.
%
% Still missing: new xml format not implemented. No default relations.
% No vocabulary reports.
%
% Revision 1.2  2000/12/20 06:26:16  jrc
% Default data path updated for new project layout. CVS keywords added.
%
%
% Joaquim Carvalho Lousa Novembro de 99
%
%
:-op(211,xfy,estrutura).
:-op(230,fx,traduzir).
%
% trailing "/" or "\" on dir names please.
%
:- put_value(stru_dir,'/Users/jrc/develop/rch/clio/src/').
:- put_value(data_dir,'/Users/jrc/develop/rch/mhk_users/').
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

check_command_line:-
	writeln('usage: pl -f clioStart.pl -- [-sf StruFile] -df DataFile [-echo (yes|no)]').


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
    list_to_a0([Base,'.','rpt'],Report),
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
		put_value(stru_dir,'/Users/jrc/develop/mhk_users/testes/stru/').


layout(windows) :-
		  put_value(data_dir,'../mhk_users/'),
		  put_value(stru_dir,'../clio/src/').
:-pclio_version.

:-check_command_line.

