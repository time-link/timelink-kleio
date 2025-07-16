/** <module> Clio input Top level

 # General description

 Clio input is a set of Prolog programs that can
 translate Kleio source data files into a PROLOG
 database.


This window contains top level predicates for the
clio input program. These predicates can be called as they are,
but normally a shell with the user interface is defined that calls
the predicates here.

clio_version/1: returns a string with the version of clio input,
clio_init:- initialization procedures.

stru(Filename): processes a file with a
Kleio struture definition
dat(Filename):  processes of kleio data file
(requires a previously processed structure
definition through a stru run)

readlines(Filetype): reads a file (either a structure
    definition or a data file) on a line by line
    basis and calls the apropriate
    predicates to process each line.

processLine(FileType,Tokens):- passes a line of tokens to the
syntactic analysers.


*/
:-module(topLevel,[
                    clio_version/1,
                    clio_version_version/1,
                    clio_version_build/1,
                    clio_version_date/1,
                    pclio_version/0,
                    clio_init/0,
                    stru/1,
                    dat/1]).
:-use_module(dataCode).
:-use_module(dataSyntax).
:-use_module(dataDictionary).
:-use_module(struSyntax).
:-use_module(struCode).
:-use_module(lexical).
:-use_module(reports).
:-use_module(counters).
:-use_module(errors).
:-use_module(persistence).
:-use_module(basicio).
:-use_module(utilities).
:-use_module(logging).
:-use_module(linkedData).
:-use_module(yamlSupport).

%%  clio_version(?V) is det.
%  Returns the version of the translator.
%  Version is set at build time by replacing tokens in the code.
% The tokens are replaced by the build script.
% Use clio_version_version/1 to get the version number only.
% Use clio_version_build/1 to get the build number only.
% Use clio_version_date/1 to get the build date only.
%
clio_version('KleioTranslator - server version @@VERSION@@ - build @@BUILD@@ @@DATE@@').
clio_version_version('@@VERSION@@').
clio_version_build('@@BUILD@@').
clio_version_date('@@DATE@@').


%%  pclio_version is det.
% prints version, compiler version, date and time
%
pclio_version:-
    clio_version(P),writeln(P),
    get_time(T),
    convert_time(T,Year,Month,Day,Hour,Min,_,_), D = Day-Month-Year,
    write(D),
    Time = Hour-Min,
    tab(1),write(Time),
    nl,
    !.
%******************************************************
%  clio_init :- initialization procedures
%                    Must be called before everything else
%******************************************************
%  %
clio_init:-
    (get_value(echo,E);E=no),%  if not defined echo initialy off %
    put_value(echo,E),
    initErrorCount, % error count %
    put_value(max_errors,10), % maximum number of errors %
    set_report(off),
    !.
%******************************************************
%  stru: starts the processing of a file with a
%        Kleio struture definition
%******************************************************
%
% %
stru(F):-
    file_name_extension(_, Ext, F),
    stru(F, Ext).

stru(F, yaml):-
    stru_yaml(F).% normalize name as atom

stru(F,str):-
      atom_string(Filename,F), % normalize name as atom
      open_file_read(Filename),
      set_input(Filename),
      put_value(stru_file,Filename),
      file_base_name(Filename,Basename),
      report([nl,write('Processing structure:'),write(Basename),nl]),
      initStru(Filename),
      retractall(struSyntax:gdoc(_,_)),
      retractall(struSyntax:edoc(_,_,_)),
      readlines(cmd),
	  report([perror_count]),
      closeStru(Filename),
      close(Filename),
      directory_file_path(Directory, _, Filename),
      concat(Basename,'.json',JsonFile),
      directory_file_path(Directory,JsonFile,JPath),
      concat(Basename,'.yaml',Yamlfile),
      directory_file_path(Directory,Yamlfile,YPath),
      make_json_yaml_str(Filename,JPath,YPath).

%******************************************************
%  dat:  starts the processing of kleio data file
%        (requires a previously processed structure
%        definition through a stru run)
%******************************************************
% %

%! dat(+Filename) is det.
%
dat(F):-
      atom_string(Filename,F), % normalize name as atom
      open_file_read(Filename),
      set_input(Filename),
      put_value(data_file,Filename),
      log_debug('[dat]: starting ~w ~n',[Filename]),
      file_base_name(Filename,Basename),
      report([nl,write('Processing data file '),write(Basename),nl,
      writeln('-------------------------------------------')]),
      initData(Filename),
      initCompiler,
      debug(kleio(dat),'dat: initData done',[]),
      % TODO PROFILING should go here
      % profile(readlines(dat))
      % or time(readlines(dat))
      readlines(dat),
	  % report([perror_count]), % must be called after final checks in gactoxml.pl
      closeData(Filename),
      close(Filename). % SWI generates an error when closing after a seen

doc(StruFile,DestDir):-
    stru(StruFile),
    make_html_doc(DestDir).

%% processLine(+FileType,+Tokens) is det.
%
%  Process line doing syntactic analysis
%    Called by readlines.
%    Takes the token lists created from current line by get_tokens/3
%    and 'feeds' them to the syntatic analysers.
%
%    If FileType is 'cmd' processLine stores the command tokens until
%       the command is complete and then passes it to the syntatic
%       analyser (compile_command). A command is complete when a new
%       command appears in the input line or when the eof is reached.

%
%    If FileType is 'dat' lines are passed to the syntactic analyser
%    as they are read.
%
%   @tbd Avoid using dynamic predicate cmd/2 to store cache for multi line commands.
%
processLine(dat,Tokens):-
    compile_data(Tokens),!.

  processLine(cmd,[]):-!.
  processLine(cmd,[(return,_)]):-!.

  processLine(cmd,[(fill,_)|Tokens]):-
      fetch_command(cmd(OldCmd,OldTokens)),!,
      append(OldTokens,Tokens,NewTokens),
      cache_command(cmd(OldCmd,NewTokens)),!.

  processLine(cmd,[(fill,_)|_]):-
      \+ clean_cached_command,
      error_out('Error, line begining with blank without previous command'),!.

  processLine(cmd,[(Tok,Command)|Tokens]):-
      Tok \= fill,
      fetch_command(cmd(OldCmd,OldTokens)),!,
      compile_command(OldCmd,OldTokens),
      cache_command(cmd(Command,Tokens)),!.

  processLine(cmd,[(Tok,Command)|Tokens]):-
      Tok \= fill,
      \+ clean_cached_command,
      cache_command(cmd(Command,Tokens)),!.

  processLine(cmd,eof):-
    report([write('End of File'),nl]),
    (fetch_command(cmd(OldCmd,OldTokens)) -> (!,compile_command(OldCmd,OldTokens))
         ;
         true).

%% echo_line(+L,+Line) is det.
%
% Echoes a line, prefixed by the line number,
% if global parameter (thread local) `echo`is "yes".
%
echo_line(L,Line):-
    get_value(echo,yes),
    report([write(L),write(': '),write(Line)]),!.
echo_line(_,_).


%% readlines(+Filetype) is det.
%
% @param Filetype Is passed down to the line processing
%       predicates. It can either be 'cmd' for a structure
%       definition or 'dat' for a kleio data file.
%       A line counter is kept.
%  Reads a file (either a structure
%       definition or a data file) on a line by line
%       basis and calls the apropriate
%       predicates to process each line.
%
% _readlines_ processes lines in three steps:
%
% #    first the line is read as a list of chars;
% #    then 'getTokens/3' is called to transform the
%       list of chars into a list of tokens
%       (lexical analysis);
% #   the Token list is passed to 'processLine/2' where
%      the tokens are parsed and executed
%      (if the file is a command file) or converted
%      and added to the internal database
%      (if the file is a Clio source data file).
%
% _readlines_  consists of a failure driven loop built
%    with a _repeat_ predicate (Built-in in LPA PROLOG).
%
%    The exit condition for the loop is that the last
%    character read is a end of file character (Next=eof).
%    Inside the loop the 'readline2' predicate is called to
%    read a list of characters from the input file.
%
%    The actual input is done by readlines2/2
%    which reads an input line as a list of chars.
%
%    Each char is represented by a structure (Char, Type),
%    where 'Char' is the Ascii code for the char and
%    'Type' is a constant that gives the type of the char,
%    (digit, letter, eof, etc...).
%    Char types are defined by 'chartype' predicates in lexical.pl
%
%    The input line is echoed to the current output stream.
%
%
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
    (Last = eof;not(errors:check_continuation)), % fail until end of file, or max errors reached %
    processLine(FileType,eof),!. % end of file reached %

readlines(_):-
   error_out('Unexpected failure in readlines'),abort,!.
