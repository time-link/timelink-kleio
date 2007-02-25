%% vim: filetype=prolog ts=3
% ****************************************************
% $Id: compatibilitySWI.pl,v 1.2 2006/05/16 10:52:42 Joaquim Exp $
% $Date: 2006/05/16 10:52:42 $
% $Author: Joaquim $
% $Log: compatibilitySWI.pl,v $
% Revision 1.2  2006/05/16 10:52:42  Joaquim
% update dos ficheiros prolog
%
% Revision 1.2  2005/03/10 14:42:04  joaquim
% snapshot commit for purpose of moving the cvs directory.
%
% Revision 1.1  2004/04/08 14:45:24  ltiago
% Source code has passed from Joaquim to Tiago.
% Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
% File build.xml, web.xml and velocity.properties were changed
%
% Revision 1.2  2004/01/03 10:11:40  jrc
% Removed the writeln predicate that became builtin in swi prolog 5.0.10
%
% Revision 1.1.1.1  2001/04/19 23:25:43  jrc
% New Repository.
%
% Revision 1.1.1.1  2001/04/18 23:34:39  jrc
% CVS repository moved from llinux to MacOSX.
%
% Revision 1.4  2001/03/06 06:29:51  jrc
% Removed the backslash as the default path separator in windows.
% SWI in fact deals with windows file names converting them to
% slash separator like in unix.
%
% Revision 1.3  2001/02/11 21:38:25  jrc
% Don't know. Diff shows file globally modified so probably line ends fixed.
%
% Revision 1.2  2001/01/27 14:45:45  jrc
% now predicate added. CVS keywords.
%
%*******************************************************************
% text of window:  compatibilityswi.pl
% Contains predicate s to extend or modify SWI prolog on windows/linux,
%   for compatibility with other Prologs, namely Salford PROLOG
%   for the PRIME, and LPA for the Mac.
%  All these here are implementation dependent. Even if written
%   in standard Prolog, they are peculiar
%   to this Prolog version in the sense that in others they might
%   be built in.
%
%   Contents:
%     A. general purpose predicates
%       on(X,L) -> X is a member of list L
%       writeln (X) writes X and adds a carriage return
%       writelist0(L). writes on the atoms of a list
%       writelist0ln(L) same but with a return at the end
%       writelist(L) wirte the members of list L with a space in between
%       writelistln(L) same but with return at the end
%       ptime(C) show the time it takes to execute a predicate
%
%     B. file handling predicates
%       break_fname(F,P,N,B,E): decomposes a filename
%       open_file_read(F) -> Opens file for reading.
%       open_file_write(O) -> Creates or erases and opens file for writing.
%       close_file(F) -> closes file F.
%       consult_file(F) - adds predicates from file F
%
% 
% Joaquim Carvalho, Papanata November 99
%
% Revision history:
%    created 17 May 1988 for LPA prolog
%    modified for LPA version 2.0W in August 90
%    modified for LPA version 3.01w in September 1990.
%    bug in break_fname that prevented handling of files with
%    no extensions corrected in AUG 91
%    modified for LPA MacProlog32 version 1.06 in September 95
%    more LPA MacProlog32 compatibility predicates added Sept 96, Nov 96
%    map predicate added March 98
%    Ported to SWI prolog on Windows Oct-Nov 99
%        rember and recall
%        propriety handling
%        concat/2 as concat_atom/2
%    Cleaned up in SWI linux ppc Aug 2000
%    Put under source control January 2001
%*******************************************************************
% %
%*******************************************************************
?-dynamic(remember_/2).
?-dynamic(prop_/2).

%*******************************************************************
% A. General purpose predicates %
% Here you will find common primitives of Prolog that where not
% implemented as built-ins in this version.
%
%
%**********************************************************
% on - another name for member/2 (from LPA prolog)
%**********************************************************
%%
on(M,L):-member(M,L).

%**********************************************************
% writeln (X) writes X and adds a carriage return
% Removed because it became builtin in swipl 5.0.10
%**********************************************************
%%
%writeln(X):-write_ln(X).
%**********************************************************
% writelist0(L). writes on the atoms of a list %
% writelist0ln(L) same but with a return at the end %
%%
writelist0(L):-
    member(X,L),
    write(X),
    fail,!.
writelist0(_):-!.
writelist0ln(L):-writelist0(L),nl.

%**********************************************************
% writelist(L) wirte the members of list L with a space in between %
% writelistln(L) same but with return at the end
%%
writelist([X|[Y|Z]]):-write(X),tab(1),writelist([Y|Z]),!.
writelist([X|[]]):-write(X),!.
writelistln(L):-writelist(L),nl.

%**********************************************************
% ptime(CALL) execute CALL and print the time it takes to execute
%%
ptime(Call):-time(Call).

%**********************************************************
% now(Year,Month,Day,Hour,Minute,Second).

now(Year,Month,Day,Hour,Minute,Second):-
	get_time(B),
	convert_time(B,Year,Month,Day,Hour,Minute,Second,_).

%******************************************************************
% break_fname(F,P,N,B,E): decomposes filename F into
%                         P - path
%                         N - name of file
%                         B - base of name of file (without extensions)
%                         E - extensions
%  Example:
% break_fname('QuimCi/Sour/:SoureCliofonte/casamentos/cas1695.cli',
%               P,N,B,E)
% returns
%     P = 'QuimCi/Soure/SoureCliofontecasamentos'
%     N = 'cas1695.cli'
%     B = 'cas1695'
%     E = 'cli'
% adpated for windows paths 17:41 06-11-1999
%******************************************************************
%%

break_fname(FullName,Path,FileName,Base,Extension):-
    path_sep(Sep),
    stringof(Chars_Full_Name,FullName),reverse(Chars_Full_Name,RCFN),
    (append(RFileName,[Sep|RPATH],RCFN)->true;(RFileName=RCFN,RPATH=[])),
    reverse(RFileName,CharsFileName),reverse(RPATH,CPATH),
    stringof(CharsFileName,FileName),stringof(CPATH,Path),
    (append(CharsBase,['.'|Cext],CharsFileName)->
         stringof(CharsBase,Base); 
         Base =FileName,Cext=[]),
    stringof(Cext,Extension),!.

%path_sep('/'):-current_prolog_flag(unix,true).
%path_sep('\\'):-current_prolog_flag(windows,true).
path_sep('/').
%******************************************************************
%  B. file handling predicates %
%
%******************************************************************
%       open_file_read(F) -> Opens file for reading.
%*******************************************************************
% %
open_file_read(F):-open(F,read,_stream,[alias(F)]).

%******************************************************************* 
%       open_file_write(O) -> Creates or erases and opens file for writing.
%*******************************************************************
% % 
open_file_write(O):-open(O,write,_stream,[alias(O)]).
%*******************************************************************
%
%   close_file(F). Closes file F. F must be instantiated with the
%                    name of a currently open file.
%*******************************************************************
% %
close_file(F):-close(F).
%******************************************
% consult_file(F) - adds predicates from file F
%*******************************************
% %
consult_file(F):-
   open_file_read(F),
   see(F),
   load_data.
load_data:- read(X),
                   assert(X),
                   write(X),nl,
                   load_data.
load_data:-seeing(F),close(F),seen.

%*******************************************
% sort PATH in sort not implemented
%*******************************************
%%
% sort/4
sort( List, Sorted, _ , 1 ):-
  !,
  sort( List, Sorted ).
sort( List, Sorted, _, -1 ):-
  sort( List, SortedAsc),
  reverse( SortedAsc, Sorted ).

sort(List,Sorted,_):-sort(List,Sorted),!.

% ****************************************
% change dir.
%*****************************************%

% set_path/1
set_path( Path ):-
   writeln(Path),
   cdir( Path ).


%**************************
% Generalized map program
%map/[2,3,4,5]
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
map1(_Rel, L):-
    var( L ),
    !,
    fail.
map1(_Rel, []):-
    !.
map1(Rel, [Val| List]):-
    make_map_goal(Rel, Call, [Val]),
    Call,
    map1(Rel, List).

% List conversion
map2(_Rel, [], []):-
    !.
map2(Rel, [El1| List1], [El2| List2]):-
    make_map_goal(Rel, Call, [El1, El2]),
    Call,
    map2(Rel, List1, List2).

% Accumulator map
map3(_Rel, [], Value, Value):-
    !.
map3(Rel, [El| List], Acc, Value):-
    make_map_goal(Rel, Call, [El, Acc, Int]),
    Call,
    map3(Rel, List, Int, Value).

% Produce and accumulate
map4(_Rel, [], [], Value, Value):-
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

% stringof/2
stringof( ListOfChars, Atom ):-
  atom( Atom ),
  !,
  name( Atom, ListOfBytes ),
  bytes_to_chars( ListOfBytes, ListOfChars ).
stringof( ListOfChars, Atom ):-
  bytes_to_chars( ListOfBytes, ListOfChars ),
  name( Atom0, ListOfBytes ),
  Atom = Atom0.

bytes_to_chars( [], [] ).
bytes_to_chars( [Byte|Bytes], [Char|Chars] ):-
  atom_char( Char, Byte ),
  bytes_to_chars( Bytes, Chars ).

%
% concat/2 concatenate an atom list
%
concat(X,Y) :- concat_atom(X,Y).

%*****************************************
% Remember and recall
%*****************************************
remember(Name,Value):-
    retractall(remember_(Name,_)),!,
    assert(remember_(Name,Value)).
remember(Name,Value):-
    assert(remember_(Name,Value)),!.
recall(Name,Value) :- remember_(Name,Value).

%*****************************************
% properties
%*****************************************
set_prop(Atom,Prop,Value):-
   atom(Atom),atom(Prop),
   retractall(prop_(Atom,Prop,_)),
   !,
   assert(prop_(Atom,Prop,Value)).


set_prop(Atom,Prop,Value):-
   atom(Atom),atom(Prop),
   !,
   assert(prop_(Atom,Prop,Value)).

get_prop(Atom,Prop,Value):-prop_(Atom,Prop,Value).

del_prop(Atom,Prop) :-
   retractall(prop_(Atom,Prop,_)),!.

del_props(Atom) :-
   retractall(prop_(Atom,_,_)),!.

get_props(Atom,Props):-
	setof(P,V^prop_(Atom,P,V),Props).

get_cons(Prop,Atoms) :-
   setof(A,V^prop_(A,Prop,V),Atoms).


% ********************************************************
% init_gensym
% Can't do in swi without reimplementing the all gensym stuff
% ********************************************************
init_gensym(_).





