
:- module(swicompatibility,
          [ on/2,
            writelist0/1,
            writelist0ln/1,
            writelist/1,
            writelistln/1,
            ptime/1,
            now/6,
            break_fname/5,
            break_fname2/5,
            open_file_read/1,
            open_file_write/1,
            close_file/1,
            consult_file/1,
            chdir/1,
            set_path/1,
            stringof/2,
            concat/2,
            remember/2,
            forget/1,
            recall/2,
            remember2/2,
            forget2/1,
            recall2/2,
            path_sep/1
          ]).
/** <module> swiCompatibilty (included in "utilities.pl")
 
 Contains predicate s to extend or modify SWI prolog on windows/linux,
   for compatibility with other Prologs, namely Salford PROLOG
   for the PRIME, and LPA for the Mac.

  All these here are implementation dependent. Even if written
   in standard Prolog, they are peculiar
   to this Prolog version in the sense that in others they might
   be built in.

 _This Module is not meant for direct import. It is rexported in module "utilities"_

 Importing module utilities (use_module(utilities) gives access to the predicates 
 published by this module.


 ### Revision history (pre-git):
    * created 17 May 1988 for LPA prolog
    * modified for LPA version 2.0W in August 90
    * modified for LPA version 3.01w in September 1990.
    * bug in break_fname that prevented handling of files with no extensions corrected in AUG 91
    * modified for LPA MacProlog32 version 1.06 in September 95
    * more LPA MacProlog32 compatibility predicates added Sept 96, Nov 96
    * map predicate added March 98
    * Ported to SWI prolog on Windows Oct-Nov 99
      *  remember and recall
      *  propriety handling
      *  concat/2 as concat_atom/2
    * Cleaned up in SWI linux ppc Aug 2000
    * Put under source control January 2001
       * Revision 1.2  2006/05/16 10:52:42  Joaquim
           *  update dos ficheiros prolog
       *  Revision 1.2  2005/03/10 14:42:04  joaquim
           *  snapshot commit for purpose of moving the cvs directory.
       *  Revision 1.1  2004/04/08 14:45:24  ltiago
           * Source code has passed from Joaquim to Tiago. Since that, source was 
               reconfigured to work on a windows platform under Apache Tomcat 5.0.18
           * File build.xml, web.xml and velocity.properties were changed

       * Revision 1.2  2004/01/03 10:11:40  jrc
            * Removed the writeln predicate that became builtin in swi prolog 5.0.10
       *  Revision 1.1.1.1  2001/04/19 23:25:43  jrc
           *  New Repository.
       *  Revision 1.1.1.1  2001/04/18 23:34:39  jrc
           *  CVS repository moved from llinux to MacOSX.
       * Revision 1.4  2001/03/06 06:29:51  jrc
           * Removed the backslash as the default path separator in windows.
       * SWI in fact deals with windows file names converting them to
           * slash separator like in unix.
       * Revision 1.3  2001/02/11 21:38:25  jrc
           * Don't know. Diff shows file globally modified so probably line ends fixed.
       * Revision 1.2  2001/01/27 14:45:45  jrc
           * now predicate added. CVS keywords.
*/

:- use_module(library(lists)).

%*******************************************************************
% these dynamic predicates are shared among threads.
%*******************************************************************
?-dynamic(remember_/2).
?-dynamic(prop_/2).

%*******************************************************************
% A. General purpose predicates %
% Here you will find common primitives of Prolog that where not
% implemented as built-ins in this version.
%**********************************************************

%% on(?X:atom,+L:list) is det.
%  another name for member/2 (from LPA prolog)
%
%  @tbd Deprecated
%

on(M,L):-member(M,L).

%% writelist0(+L) is det.
% writes the elements of a list 
%
% @see writelist0ln(L) same but with a return at the end 
%

writelist0(L):-
    flatten(L,FL),findall(T,(member(M,FL),make_textual(M,T)),FL2),
    atomic_list_concat(FL2,A),write(A).

make_textual(A,A):-error:text(A),!.
make_textual(A,T):-format(string(T),"~p",[A]).

%% writelist0ln(+L) is det.
% writes the elements of a list with new line at the end.
%
% @see writelist0(L) same but without a newline at the end 
%
writelist0ln(L):-writelist0(L),nl.

%% writelist(+L) is det.
%  write the members of list L with a space in between 
% 
% @see writelistln(L) same but with return at the end
%
writelist(L):-atomic_list_concat(L,' ',A),write(A).

%% writelistln(+L) is det.
%  write the members of list L with a space in between 
% and end with new line,.
% 
% @see writelist(L) same but with no new line at end
%
writelistln(L):-writelist(L),nl.

%% ptime(+CALL) is det.
% execute CALL and print the time it takes to execute
%
ptime(Call):-time(Call).

%% now(-Year,-Month,-Day,-Hour,-Minute,-Second) is det.
% Get current time in _components_
%

now(Year,Month,Day,Hour,Minute,Second):-
	get_time(B),
	convert_time(B,Year,Month,Day,Hour,Minute,Second,_).

%% break_fname(+F,?P,?N,?B,?E) is det.
%  Decomposes filename F into path, name of file, base name and
%  extensions.
%
% @param F  filename including path and extension
% @param P  path
% @param N  name of file
% @param B  base of name of file (without extensions)
% @param E  extensions
%
% ### Example:
% ?- break_fname('/Users/jrc/Sour/SoureCliofonte/casamentos/cas1695.cli',P,N,B,E).
%
%       P = "/Users/jrc/Sour/SoureCliofonte/casamentos",
%       N = "cas1695.cli",
%       B = "cas1695",
%       E = "cli".
%
% @tbd  TODO: reimplement using the native filename predicates of SWI.
break_fname2(FullName,Path,FileName,Base,Extension):-
    path_sep(Sep),
    stringof(Chars_Full_Name,FullName),reverse(Chars_Full_Name,RCFN),
    (append(RFileName,[Sep|RPATH],RCFN)->true;(RFileName=RCFN,RPATH=['.'])),
    reverse(RFileName,CharsFileName),reverse(RPATH,CPATH),
    stringof(CharsFileName,FileName),stringof(CPATH,Path),
    (append(CharsBase,['.'|Cext],CharsFileName)->
         stringof(CharsBase,Base);
         Base =FileName,Cext=[]),
    stringof(Cext,Extension),!.


break_fname(FullName,Path,FileName,Base,Extension):-
    file_directory_name(FullName, Path),
    file_base_name(FullName,FileName),
    file_name_extension(Base,Extension,FileName),
    !.

%path_sep('/'):-current_prolog_flag(unix,true).
%path_sep('\\'):-current_prolog_flag(windows,true).

%% path_sep(?Sep) is det.
% True if Sep in the separator of directories in paths in the current os
% Currently always returns "/" but this is consistent with swi os independent way of handling files
%
path_sep('/').
%******************************************************************
%  B. file handling predicates %
%******************************************************************


%% open_file_read(+F) is det.
%  Opens file  F for reading.
% 
open_file_read(F):-
    current_stream(F,read,_),
    close(F,[force(true)]),
    make_alias(F,Alias),
    open(F,read,Alias,[alias(Alias)]).
open_file_read(F):-
    make_alias(F,Alias),
    open(F,read,__stream,[alias(Alias)]).

% currently not useful, we have to ensure that fileammes are atomic, no
% strings.
make_alias(File,File):-atom(File).
make_alias(File,Alias):-string(File),atom_string(Alias,File).

%% open_file_write(+O) is det.
% Creates or erases and opens file for writing.
%
open_file_write(O):-
    current_stream(O,write,_),
    close(O,[force(true)]),
    open(O,write,__stream,[lock(write),wait(true),alias(O)]).
open_file_write(O):-
    open(O,write,__stream,[lock(write),wait(true),alias(O)]).

%% close_file(+F) is det.
%  Closes file F. F must be instantiated with the
%  name of a currently open file.
%
close_file(F):-close(F).

%% consult_file(F) is det.
%  Adds predicates from file F
%
% @tbd deprecated.
%
consult_file(F):-
   consult(F).

%% chdir(?Dir) is det.
% Returns the current directory or changes the current directory.
%
% *Important* 
%    * chdir/1 is not thread safe. Current directory will change in every running thread
%
% @see working_directory/2
% 

chdir(D):-var(D),working_directory(D,D),!.
chdir(D):-working_directory(_,D),!.

%% set_path(+Path) is det.
% Same as chdir/2.
%
set_path( Path ):-
   writeln(Path),
   working_directory( _,Path ).


%**************************
% Generalized map program
%map/[2,3,4,5]
% TODO: use maplist instead
% Think not used at the moment.
%***************************%

map(Rel, Input):-
    map1(Rel, Input).

map(Rel, Input, Output):-
    map2(Rel, Input, Output).

map(Rel, Input, Start, Finish):-
    map3(Rel, Input, Start, Finish).

map(Rel, Input, Output, Start, Finish):-
    map4(Rel, Input, Output, Start, Finish).


% Each kind of map is handled separately %

% Type check
map1(__Rel, L):-
    var( L ),
    !,
    fail.
map1(__Rel, []):-
    !.
map1(Rel, [Val| List]):-
    make_map_goal(Rel, Call, [Val]),
    Call,
    map1(Rel, List).

% List conversion
map2(__Rel, [], []):-
    !.
map2(Rel, [El1| List1], [El2| List2]):-
    make_map_goal(Rel, Call, [El1, El2]),
    Call,
    map2(Rel, List1, List2).

% Accumulator map
map3(__Rel, [], Value, Value):-
    !.
map3(Rel, [El| List], Acc, Value):-
    make_map_goal(Rel, Call, [El, Acc, Int]),
    Call,
    map3(Rel, List, Int, Value).

% Produce and accumulate
map4(__Rel, [], [], Value, Value):-
    !.
map4(Rel, [El1| List1], [El2| List2], Acc, Value):-
    make_map_goal(Rel, Call, [El1, El2, Acc, Int]),
    Call,
    map4(Rel, List1, List2, Int, Value).

% Make each map goal
make_map_goal( Goal, Call, Rest ):-
    Goal =.. [Rel,Fixed],
    Call =.. [Rel, Fixed| Rest],
    !.
make_map_goal( Rel, Call,  Args ):-
    Call =.. [Rel| Args].

%***************************************
% Strings and atoms
%****************************************

%% stringof(?ListOfChars,?String) is det.
%  Equivalent to string_chars/2
%
stringof( ListOfChars, String ):-
  string_chars(String,ListOfChars).

%% concat(+List,-Atom) is det.
%  concatenate a list into a atom.
% Equivalente to concat_atom/2
%
concat(X,Y) :- concat_atom(X,Y).

%%  remember(+Name,+Value) is det.
%  stores a value associated with a name.
%  the value is shared among all threads.
%
% @see recall/2
%
remember(Name,Value):-
    retractall(remember_(Name,_)),!,
    assert(remember_(Name,Value)).
remember(Name,Value):-
    assert(remember_(Name,Value)),!.

%% recall(+Name,-Value) is det.
% Recall a stored valued, shared among threads.
%
% @see remember/2
recall(Name,Value) :- remember_(Name,Value).

%% forget(+Name) is det.
% Erase stored value under name. Shared among threads.
%
% @see recall2/2
% @see remember2/2
forget(Name):-
    retractall(remember_(Name,_)),!.

%%  remember2(+Name,+Value) is det.
%  stores a value associated with a name.
%  The value is thread local.
%
% @see recall2/2
%
remember2(Name,Value):-
    nb_setval(Name,Value).

%% recall2(+Name,-Value) is det.
% Recall a value stored. The value is thread local.
%
% @see remember2/2
% @see forget2/2
%
recall2(Name,Value):-
    catch(nb_getval(Name,Value),_,fail),!.

%% forget2(+Name) is det.
% Erase stored value under name. Thread local.
%
% @see recall2/2
% @see remember2/2
forget2(Name):-
    nb_delete(Name).







