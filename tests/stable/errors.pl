:- module(errors,[
        initErrorCount/0,
        error_out/1,error_out/2,
        warning_out/1,warning_out/2,
        error_count/1,
        warning_count/1,
        perror_count/0,
        perr/1,
        check_continuation/0
    ]).

/** <module> Error reporting for kleio predicates

## Error reporting



## Pre-git history

*  Revision 1.2  2006/05/16 10:52:44  Joaquim
 update dos ficheiros prolog

*  Revision 1.1  2004/04/08 14:45:24  ltiago
 Source code has passed from Joaquim to Tiago.
 Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
 File build.xml, web.xml and velocity.properties were changed

 * Revision 1.2  2002/06/23 10:27:42  jrc
 Outputs data file name and line number (for eventual processing by build systems).

 * Revision 1.1.1.1  2001/04/19 23:25:43  jrc
 New Repository.

 * Revision 1.1.1.1  2001/04/18 23:34:39  jrc
 CVS repository moved from llinux to MacOSX.

 * Revision 1.8  2001/02/11 21:39:20  jrc
 Minor change in code organization.

 * Revision 1.7  2001/01/28 16:21:26  jrc
 Corrected error count.

 * Revision 1.6  2001/01/28 02:04:33  jrc
 Prints two lines in error reports for better context.

 * Revision 1.5  2001/01/28 00:06:06  jrc
 Echoes the current line.

 * Revision 1.4  2001/01/27 23:56:48  jrc
 Simplified error reporting (skiped showCDS).

 * Revision 1.3  2001/01/15 18:27:15  jrc
 added p_errorcount. Cleaned line ends and added
 cvs keywords.

*/
:-use_module(utilities).
:-use_module(counters).
:-use_module(persistence).
:-use_module(reports).

%% initErrorCount is det.
%
initErrorCount:-
    setcount(errors,1),
    setcount(warnings,1).

% COMMNENTED OUT
% not sure this is a good idea, it means that no more than one message regarding the same line
% so if two or more different errors occur in the same line only the first would be reported.
% comment out because it does not look a good idea.
%error_out(Mess):-
%   get_prop(line,number,N),
%   get_prop(line,previous,N),
%   !. % skipping repeated error in the same line.

%% error_out(+Mess) is det.
%  Outputs an error message, using context information.
%  Used in conjunction with report/1 to insert error
%  messages in translation reportds.
%
%  Also updates error count and aborts translation
%  if maximum number of errors is reached.
%
error_out(Mess):-
   inccount(errors,_),
    p_error_warn_context('ERROR',Mess).

%% error_out(+Mess,+Context) is det.
%
% Context is an option list with the following options:
% $file(Atom) : the source file where the error/warning was found
% $line_number(Number): the line number of the error
% $line_text(String)  : the text of the line
% $last_line_text(Last) : the texto the previous line (errors are normally detected on the line after the error)
%
error_out(Mess,Context):-
   inccount(errors,_),
   p_error_warn_context('ERROR',Mess,Context),!.

%% warning_out(+Mess) is det.
% Similar to error_out/1 but for warnings.
%
warning_out(Mess):-
   inccount(warnings,_),
    p_error_warn_context('WARNING',Mess).

%% warning_out(+Mess,+Context) is det.
% Similar to error_out/2 but for warnings.
%
warning_out(Mess,Context):-
   inccount(warnings,_),
    p_error_warn_context('WARNING',Mess,Context).


%% p_error_warn_context(+Type,+Message) is det.
%
% Outputs a warning or error message using the current report file.
%
% Fetches the current and previous line numbers and text.
%
p_error_warn_context(TYPE,Mess):-
   p_error_warn_context(TYPE,Mess,[]),!.

%% p_error_warn_context(+Type,+Message,Context) is det.
%
% Outputs a warning or error message using the current report file.
%
% Uses the context information in Context. Context is an option list with the following options:
% $file(Atom) : the source file where the error/warning was found
% $line_number(Number): the line number of the error
% $line_text(String)  : the text of the line
% $last_line_text(Last) : the texto the previous line (errors are normally detected on the line after the error)
%
p_error_warn_context(TYPE,Mess,Context):-
   (option(file(Source_file),Context)->
      true
      ;
      (
         get_value(data_file,DataFile),
         break_fname(DataFile,_,Source_file,_,_)
      )
   ),
   (option(line_number(N),Context)-> true; get_prop(line,number,N)),
   (option(line_text(Line),Context)->true;get_prop(line,text,Line)),
   (option(last_line_text(Last),Context)->true;get_prop(line,last,Last)),
   set_prop(line,previous,N), % we note the error line
   N2 is N-1, % we used to miss the line by one but now we we take the line number as good
   %report([nl,writelist0([DataFile,':',N,':']),perr(Mess),nl,true]),
  report([nl,write(TYPE),write(': '),write(Source_file),tab(1),write(line),tab(1), write(N2),tab(1),perr(Mess),
           write('Near lines: '),write(N2),tab(1),write(Last),
           write('Near lines: '),write(N) ,tab(1),write(Line),
           nl,
       true]),!.

perr(X):-
  atomic(X),
  writeln(X),!.
perr([A|B]):-
  writelist0ln([A|B]),!.
perr(X):-
   functor(X,_,_),
   writeln(X),!.
perr(X):-
   with_output_to(string(W),write(X)),
   writeln(W),!.

%% check_continuation is det.
%
% True if the current number of errors is less or equal to max_errors.
% Can be used to abort a translation if many errors are being generated.
%
check_continuation:-
   getcount(errors,N),
   (get_value(max_errors,M);M=100),
   N =< M,!.
check_continuation:-
   nl,report(
        [writeln('*** ERROR: MAXIMUM NUMBER OF ERRORS REACHED. TRANSLATION ABORTED.')]),
   !,fail.

%% perror_count is det.
%
%  Prints numbers of errors and warnings.
%
perror_count :-
		  error_count(N2),
		  warning_count(N3),!,
		  writelistln([N2,' errors. ']),
		  writelistln([N3,' warnings.']),!.

%% error_count(-E) is det.
% Current error count.
%

error_count(E) :- getcount(errors,N),E is N-1.

%% warning_count(-W) is det.
% Current warning count.
%
warning_count(E) :- getcount(warnings,N),E is N-1.
