%% vim: filetype=prolog ts=3
/*
 =========================================================
  errors.pl
  =========
  Error reporting for kleio predicates

 $Id$

 $Date$
 $Revision$
 $Author$
 $Log: errors.pl,v $
 Revision 1.2  2006/05/16 10:52:44  Joaquim
 update dos ficheiros prolog

 Revision 1.1  2004/04/08 14:45:24  ltiago
 Source code has passed from Joaquim to Tiago.
 Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
 File build.xml, web.xml and velocity.properties were changed

 Revision 1.2  2002/06/23 10:27:42  jrc
 Outputs data file name and line number (for eventual processing by build systems).

 Revision 1.1.1.1  2001/04/19 23:25:43  jrc
 New Repository.

 Revision 1.1.1.1  2001/04/18 23:34:39  jrc
 CVS repository moved from llinux to MacOSX.

 Revision 1.8  2001/02/11 21:39:20  jrc
 Minor change in code organization.

 Revision 1.7  2001/01/28 16:21:26  jrc
 Corrected error count.

 Revision 1.6  2001/01/28 02:04:33  jrc
 Prints two lines in error reports for better context.

 Revision 1.5  2001/01/28 00:06:06  jrc
 Echoes the current line.

 Revision 1.4  2001/01/27 23:56:48  jrc
 Simplified error reporting (skiped showCDS).

 Revision 1.3  2001/01/15 18:27:15  jrc
 added p_errorcount. Cleaned line ends and added
 cvs keywords.

*/

initErrorCount:-
    setcount(errors,1),
    setcount(warnings,1).

error_out(Mess):- %not sure this is a good idea, so changed the name to disable
   get_prop(line,number,N),
   get_prop(line,previous,number,N),
   !. % skipping repeated error in the same line.

error_out(Mess):-
    p_error_warn_context('ERROR',Mess).

warning_out(Mess):-
    p_error_warn_context('WARNING',Mess).


p_error_warn_context(TYPE,Mess):-
   get_value(data_file,DataFile),
   break_fname(DataFile,_,Source_file,_,_),
   get_prop(line,number,N),
   get_prop(line,last,Last),
   get_prop(line,text,Line),
   set_prop(line,previous,N),
   N2 is N-1,
   report([nl,writelist0([DataFile,':',N,':']),perr(Mess),nl,true]),
  report([nl,write(TYPE),write(': '),write(Source_file),tab(1),write(line),tab(1), write(N2),tab(1),perr(Mess), % we always get at least a line later
           write('Near line: '),write(N2),tab(1),write(Last),
           write('Near line: '),write(N) ,tab(1),write(Line),
           nl,
       true]),
   check_continuation(TYPE),!.

error_out(Mess,noline):-
   report([nl,write('ERROR: '),perr(Mess),nl]),
   check_continuation('ERROR'),!.

perr(X):-
  atomic(X),
  writeln(X),!.
perr([A|B]):-
  writelist0ln([A|B]),!.
perr(X):-
   functor(X,_,_),
   writeln(X),!.

check_continuation('ERROR'):-
   inccount(errors,N),
   get_value(max_errors,M),
   N =< M,!.
check_continuation('ERROR'):-
   nl,report(
        [writeln('*** ERROR: MAXIMUM NUMBER OF ERRORS REACHED. TRANSLATION ABORTED.')]),
   fail, !.
check_continuation('ERROR'):- % don't think this is ever reached
   inccount(errors,N).
check_continuation('WARNING'):-
   inccount(warnings,N),!.



perror_count :-
		  error_count(N2),
		  warning_count(N3),
		  writelistln([N2,' errors. ',N3,' warnings.']),!.

error_count(E) :- getcount(errors,N),E is N-1.
warning_count(E) :- getcount(warnings,N),E is N-1.
