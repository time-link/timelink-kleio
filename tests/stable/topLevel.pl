% vim: filetype=prolog
% $Id$
% $Date$
% $Author$
% $Log: topLevel.pl,v $
% Revision 1.2  2006/05/16 10:52:51  Joaquim
% update dos ficheiros prolog
%
% Revision 1.3  2006/03/27 20:36:23  jrc
% cleans information produced by the new documentation facility of kleio str files.
%
% Revision 1.2  2005/03/10 14:42:27  joaquim
% snapshot commit for purpose of moving the cvs directory.
%
% Revision 1.1  2004/04/08 14:45:24  ltiago
% Source code has passed from Joaquim to Tiago.
% Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
% File build.xml, web.xml and velocity.properties were changed
%
% Revision 1.1.1.1  2001/04/19 23:25:44  jrc
% New Repository.
%
% Revision 1.1.1.1  2001/04/18 23:34:39  jrc
% CVS repository moved from llinux to MacOSX.
%
% Revision 1.6  2001/01/28 01:35:05  jrc
% Keeps last line processed: this helps error reporting.
%
% Revision 1.5  2001/01/15 18:46:55  jrc
% Adds error count at the end of stru processing and datafile translation
%
% Revision 1.4  2001/01/14 23:02:01  jrc
% The name of the stru file processed is now kept and made available
% by clio_stru_file in external.pl
%
% Revision 1.3  2001/01/14 00:45:26  jrc
% Cleaned version can handle casamentos. More files under source control.
% Minor bugs cleaned in various prolog files.
%
% Still missing: new xml format not implemented. No default relations.
% No vocabulary reports.
%
% Revision 1.2  2001/01/11 04:42:30  jrc
% CVS keywords and vim filetype command. Version and Date in Clio Version derived from CVS keywords
% (experimental).
%
%******************************************************
%  Clio input Top level
%  ==================== 
%    
%    General description
%    ===================
%    
%    Clio input is a set of Prolog programs that can
%     translate Kleio source data files into a PROLOG 
%      database.
%    
% 
%  This window contains top level predicates for the
%    clio input program. These predicates can be called as they are,
%    but normally a shell with the user interface is defined that calls
%    the predicates here.
%   
%  clio_version/1: returns a string with the version of clio input,
%   clio_init:- initialization procedures. 
%    
%  stru(Filename): processes a file with a 
%        Kleio struture definition
%  dat(Filename):  processes of kleio data file
%        (requires a previously processed structure 
%        definition through a stru run)
%  
%  readlines(Filetype): reads a file (either a structure
%       definition or a data file) on a line by line
%       basis and calls the apropriate
%       predicates to process each line.
%
%    processLine(FileType,Tokens):- passes a line of tokens to the
%         syntactic analysers. 
%    
%
%    History
%    Extended comments added 27-AUG-90
%    InitData and CloseData added 17-Sept-90
%    clio_shell added 13 OCT 90
%    clio_shell removed to a separate window in 23 OCT 90
%    stru and dat changed to be system independent 13 OCT 90.
%    processLine moved here from a different window 13 OCT 90
%    version 1.0.1 correspondes to optimized storeCore in the DataCode
%     file.
%    version 1.1. corresponds to a new CDS data structure to speed DataCode
%    version 1.2  corresponds to the new clioShell in LPA with multiple data files
%    version 1.2.b is a test for efficiency with storeEls modified
%    version 1.3  Keeps 1.2.b and adds: 
%         "clio externals prol" code to standardize the interface with export modules.
%    version 1.4 Nov 96 Ported to LPA32
%    version 1.5 Ago 97 Make_id changed to allow longer ids (up to 42 chars)
%                       fons parameter stored in group definitions
%                       acess to group definition given to export modules thru clio_group_param
%    version 1.6 Nov 99 SWI port on windows
%    version 1.7 Feb 2000 XSB Port on windows
%    version 1.8 Sept 2000 SWI 3.39 on linux ppc.
% %
?-dynamic(cmd/2).
?-dynamic(cmd/2).
?-dynamic(cmd/2).

%******************************************************
%  clio_version(V) where V is the version
%******************************************************
%  %
clio_version('ClioInput - version 2.3 - build 1590 21/06/2018 12:00 ').

%******************************************************
%  clio_init :- initialization procedures
%                    Must be called before everything else
%******************************************************
%  %
clio_init:-
    put_value(echo,no),  % echo initialy off %
    initErrorCount, % error count %
    put_value(max_errors,1000), % maximum number of errors %
    set_report(off),
    !.  
%******************************************************
%  stru: starts the processing of a file with a 
%        Kleio struture definition
%******************************************************
%  
% %
stru(Filename):-
      open_file_read(Filename),
      set_input(Filename),
		put_value(stru_file,Filename),
      report([nl,write('Processing structure:'),write(Filename),nl]),
      initStru(Filename),
      retractall(gDoc(_,_)),
      retractall(eDoc(_,_,_)),
      ptime(readlines(cmd)),
	  report([perror_count]),
      closeStru(Filename),
      close(Filename).

%******************************************************
%  dat:  starts the processing of kleio data file
%        (requires a previously processed structure 
%        definition through a stru run)
%******************************************************
% %

dat(Filename):-
      open_file_read(Filename),
      set_input(Filename),
      put_value(data_file,Filename),
      report([nl,write('Processing data file:'),write(Filename),nl,
      writeln('-------------------------------------------')]),
      initData(Filename),
      initCompiler,
      ptime(readlines(dat)),
	  % report([perror_count]), % must be called after final checks in gactoxml.pl
      closeData(Filename),
      close(Filename). % SWI generates an error when closing after a seen


%******************************************************
%  
%  readlines(Filetype) reads a file (either a structure
%       definition or a data file) on a line by line
%       basis and calls the apropriate
%       predicates to process each line.
%
%       The FileType
%       variable is passed down to the line processing
%       predicates. It can either be 'cmd' for a structure
%       definition or 'dat' for a kleio data file.
%       A line counter is kept.
%             
% 'Readlines/1' processes lines in three steps: 
%      first the line is read as a list of chars; 
%      then 'getTokens/3' is called to transform the 
%       list of chars into a list of tokens
%       (lexical analysis);
%     the Token list is passed to 'processLine/2' where
%      the tokens are parsed and executed
%      (if the file is a command file) or converted
%      and added to the internal database
%      (if the file is a Clio source data file).
%    
% 'Readlines' consistes of a failure driven loop built
%    with a 'repeat'predicate (Built-in in LPA PROLOG)
%    The exit condition for the loop is that the last 
%    character read is a end of file character (Next=eof).
%    Inside the loop the 'readline2' predicate is called to
%    read a list of characters from the input file.
%    
%    The actual input is done by 'readlines2/2.'in the 
%    'basic i/0' window. 
%    'Readlines2/2' reads an input line as a list of chars. 
%    Each char is represented by a structure (Char, Type),
%    where 'Char' is the Ascii code for the char and 
%    'Type' is a constant that gives the type of the char,
%    (digit, letter, eof, etc...). 
%    Char types are defined by 'chartype' predicates in lexical.pl. 
%
%    The input line is echoed to the current output stream.
%    
% History
%    Extended comments 27 AUG 1990
%    line count implemented with the counters predicates
%      in 'rhc.utilities' 27 AUG 1990
%    line echo moved to before processLine call. 10 Sept 1990
%    
%******************************************************
%  %
readlines(FileType):-
     setcount(line,1),
     repeat,
      readline2(List,Last), % get the character List %
      inccount(line,L),
      get_tokens(FileType,List,Tokens),% lexical analysis %
      get_string(List,Line), % make a string %
      set_prop(line,number,L),
      set_prop(line,text,Line),
      processLine(FileType,Tokens), % parsing and execution%
      echo_line(L,Line),
		set_prop(line,last,Line),
     Last = eof, % fail until end of file %
     processLine(FileType,eof),!. % end of file reached %

readlines(_):- 
    error_out('Unexpected failure in readlines'),abort,!.

echo_line(L,Line):-
      get_value(echo,yes),
      report([write(L),write(': '),write(Line)]),!.
echo_line(_,_).
%******************************************************
%  
% processLine(FileType,Tokens):- do syntactic analysis 
%    Called by readlines
%    Takes the token lists created from current line by get_tokens/3
%    and 'feeds' them to the syntatic analysers.
%    
%    If FileType is 'cmd' processLine stores the command tokens until
%       the command is complete and then passes it to the syntatic
%       analyser (compile_command). A command is complete when a new
%       command appears in the input line or when the eof is reached.
%       The command tokens are stored in a cmd(C,T) clause where
%       C is the command name and T the list of tokens.
%    
%    If FileType is 'dat' lines are passed to the syntactic analyser
%    as they are read.
%       
%    
% History
%    Comments and simplification of structure (by elimination of
%      intermediate predicates) 27 AUG 1990.
%    Moved to clio input to level at 13 OCT 1990  
%        
%
%**********************************************
%/
%******************************************************
%  
% Source data files
% =================
%******************************************************
% %
processLine(dat,Tokens):-
   compile_data(Tokens),!.
%******************************************************
%  
% command files
% =============
%******************************************************
% empty lines - do nothing %

processLine(cmd,[]):-!.
processLine(cmd,[(return,_)]):-!.

%******************************************************
% lines beginning with space continue the last command 
%    get the command as it is and add the new tokens.
%    %

processLine(cmd,[(fill,_)|Tokens]):-
     retract(cmd(OldCmd,OldTokens)),!,
     append(OldTokens,Tokens,NewTokens),
     assert(cmd(OldCmd,NewTokens)),!.

% continuation without a previous command, Error!%
processLine(cmd,[(fill,_)|_]):-
     \+ retract(cmd(_,_)),
     error_out('Error, line begining with blank without previous command'),!.

%******************************************************
% lines not beginning with space are new commands %
% if there is a command stored process it and store the new one
%    
% %

processLine(cmd,[(Tok,Command)|Tokens]):-
     Tok \= fill,
     retract(cmd(OldCmd,OldTokens)),!,
     compile_command(OldCmd,OldTokens),
     assert(cmd(Command,Tokens)),!.
% if no command is stored then store the new one %
processLine(cmd,[(Tok,Command)|Tokens]):-
     Tok \= fill,
     \+ retract(cmd(_,_)),
     assert(cmd(Command,Tokens)),!.

%******************************************************
% eof of file token , process current command %

processLine(cmd,eof):-
   report([write('End of File'),nl]),
   (retract(cmd(OldCmd,OldTokens)) -> (!,compile_command(OldCmd,OldTokens))
        ;
        true).


%  clean_commands - erases any temporary command information %
clean_commands:-
    retractall(cmd(_,_)).

