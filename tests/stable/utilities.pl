% vim: filetype=prolog ts=3
% ****************************************************
% $Id$
% $Date$
% $Revision$
% $Author$
% $Log: utilities.pl,v $
% Revision 1.2  2006/05/16 10:52:51  Joaquim
% update dos ficheiros prolog
%
% Revision 1.3  2005/03/10 14:42:28  joaquim
% snapshot commit for purpose of moving the cvs directory.
%
% Revision 1.2  2004/05/10 06:23:39  joaquim
% No change except noting that the semantics of last
% in swi-prolog changed again. It now is last(LIST,Element)
% as in ?- last([a,c,d,f,v,g],A).
% A = g
%
% Revision 1.1  2004/04/08 14:45:24  ltiago
% Source code has passed from Joaquim to Tiago.
% Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
% File build.xml, web.xml and velocity.properties were changed
%
% Revision 1.2  2004/01/03 10:19:02  jrc
% Removed the last predicate because it became builtin in swi prolog 5.0.10. Note that the semantics of "last" in swi is different from what it was here: last(Element,List) in swi instead of last(List,Element) as defined previously here. Only dataCDS.pl used the last predicate
%
% Revision 1.1.1.1  2001/04/19 23:25:44  jrc
% New Repository.
%
% Revision 1.1.1.1  2001/04/18 23:34:39  jrc
% CVS repository moved from llinux to MacOSX.
%
% Revision 1.3  2001/01/15 18:47:39  jrc
% Line ends corrected
%
%
% Contains usefull predicates or programs im plemented on
% standard (Edinburgh) PROLOG. System dependent predicates,
% even if they would qualify as "utilities", are in a special
% file, called compatibility.xxxx, where xxxx identifies the
% system to which they apply.
%
% Contents:
%    A.general purpose utilities
%       findatrib(X,A,V):  gets the value V of atribute A of object X
%
%       uo_pair(A,B),L):   gives an unordered pair with the elements of a list.
%
%       all_uo_pairs(A,L): all unordered, no repetition pairs of a list
%
%       member_check(X,L): non backtrackable version of member.
%       member_nth(E,L,N): E is the Nth element of L
%       insert(A,L,N) inserts A at the biggining of list L giving N
%       gensymbol(S,X): generates new atoms from root S
%       setgensymbol(S,N): resets gensymbol counter to N
%
%       put_value(Variable,Value) stores a value for latter recall with get_value
%
%       ok:   a predicate that is always true.
%
%       nth_arg(Arg,Table,N):- return in N the Arg position in Table structure
%
%       set_nth(List,Value,N,NewList)-set the nth element of a list
%    
%       first_n(L,N,M) M is the list of the first N elements of L
%
%       export(F,_format,_fields,_predicate): outputs results of
%           a query to a file in a format usable by other programes.
%
%       list_to_a0(LIST,STRING):- transforms a list of atoms into a string
%       list_to_a(LIST,STRING):- same, but puts a space between each atom
%                                on list.
%       l_dif(A,B,C) :- C is the list of the elements of A not in B
%      
%       begins_with(Small,Long) :- Long list begins with small list
%
%  B. Counters
%
%      setcount(COUNTER,VALUE):-initializes a counter
%      getcount(COUNTER,VALUE):-get counter value
%      inccount(COUNTER,VALUE):-increment value.
%      deccount(COUNTER,VALUE):-decrement value.
%      eracount(COUNTER,VALUE):-erase counter.
%
% C. Reporting (sending text to a disk file and to the console at the same time)
% 
%    set_report_file(FILENAME) :- sets and opens FILENAME for output
%    set_report(X) :- if X is 'on' output is send to FILENAME and the console
%                     if X is 'off' output is send only to the console.
%    report_status(X) - returns current status for report status
%                       set by set_report
%
%    report(PREDICATE_LIST):- executes the predicates in PREDICATE LIST twice.
%          The second time the output is redirected to FILENAME
%    close_report_file:- closes current report file.
%
% Joaquim Carvalho, Florence, May 88
% Revision history:
%    created 17 May 1988
%    Modified 27 August 90
%    'reverse' deleted and moved to the 'compatibility file'
%    since it is built-in in version 2.0w of LPA
%    nth_arg added 17 Sept 1990
%    Reporting predicates added in November 1990
%    writeln(X) moved to compatibility file FEB 1991
%    Split facts moved out to a separated file in FEB 1991
%    Several new predicates moved here from other programs
%      list_to_a, list_to_a0, first_n, beggins_with,l_dif,
%      last... FEB 1991.
%    report(Predicate list) can now be called without calling
%      set_report before (Out 93).
%    removed a dependency on the lst (is_a_list) predicate (Nov 2000).
%***********************************************************
% %
?-dynamic(current_num/2).
?-dynamic(count/2).

%***********************************************************
%***********************************************************
%    A.general purpose utilities
%
%***********************************************************
% findatrib(X,A,V): gets the value V of atribute A of object X
%  returns "missing"  if a value wasn't found (no A(X,V) fact
%        in the database, or if called with A unistantiated
%***********************************************************
% %
findatrib(X,A,V):-
      not( var(A)),
      P =.. [A,X,V],
		call(P),!.
findatrib(_,_,missing).

%***********************************************************
% uo_pair((A,B),L): gives an unordered pair (A,B) with the elements
%                   of a list L,without repetition of the element in a pair.
%                   On backtracking produces all the unordered
%                   pairs of elements of the list (see all_uo_pairs).
%***********************************************************
% %
uo_pair((A,B),L):-
     member(A,L),
     append(_L1,[A|L2],L),
     member(B,L2).

%***********************************************************
% all_uo_pairs: all unordered, no repetition pairs of a list
%***********************************************************
% %
all_uo_pairs(A,L):-findall(C,uo_pair(C,L),A).
%***********************************************************
% ord_pair((A,B),L) ordered pair of elements of list L.
%                            gives (A,B), but not (B,A), nor (A,A) pairs
%***********************************************************
% %
ord_pair((A,B),L):-
      append(_X,[A|Y],L),
      member(B,Y).

%***********************************************************
% member_check(X,L): non backtrackable version of member.
%                   to be used just to see if X is member of L.
%***********************************************************
% %
member_check(X,[X|_Xs]):-!.
member_check(X,[Y|Ys]):-not( X = Y),
            member_check(X,Ys),!.
%******************************************************
%  memeber_nth(E,L,N) E is the Nth element of L
%******************************************************
%  %
member_nth(A,L,N):-member_nth(A,L,1,N).
member_nth(A,[A|_B],N,N).
member_nth(A,[_B|C],N,N1):-
    N2 is N+1,
    member_nth(A,C,N2,N1).

%***********************************************************
% insert(A,L,N) inserts A at the biggining of list L giving N
%***********************************************************
% %
insert(A,L,N):-append(L,[A],N).

%***********************************************************
% ok: predicate that is always true.
% This originally served as a way to get around a bug in LPA PROLOG
% That ignores a cut when it is the only body of a clause. So instead
%     of writing "rule:-!." we write "rule:-ok,!."
% It is still around because those original LPA programs have to be kept
%  until a newer compiler is found for the Mac.
%***********************************************************
% %
ok.
%***********************************************************
% gensymbol(ROOT,SYMBOL):- generates a symbol from ROOT.
%  Ex. gensym(root,X) would return with X=root1,
%  successif calls will give root2, root3, etc...
% See Cocklish and Mellish, p.153 ff. 
%***********************************************************
% %
gensymbol(Root,Atom):-
   get_num(Root,Num),
   name(Root,Name1),
   name(Num,Name2),
   append(Name1,Name2,Name),
   name(Atom,Name),!.

get_num(Root,Num):-
   retract(current_num(Root,Num1)),!,
   Num is Num1+1,
   asserta(current_num(Root,Num)).
get_num(Root,1):-asserta(current_num(Root,1)),!.

%**********************************************************
%       setgensymbol(S,N): resets gensymbol counter to N
%**********************************************************
% %
setgensymbol(S,_N):- retract(current_num(S,_)),fail.
setgensymbol(S,N):- assert(current_num(S,N)),!.

%******************************************************
%  put_value(Var,VAL) stores VAL associated with Var
%         Val can be retrieved latter on by calling
%         get_value(Var,VAL)
%******************************************************
%  %

put_value(NAME,VALUE):-
    remember(NAME,VALUE),!.

% get_value(NAME,VALUE) %
get_value(NAME,VALUE):-recall(NAME,VALUE),!.
%******************************************************
%   nth_arg(Arg,Table,N):- return in N the Arg position
%     in the Table structure
%    Example:  : 
%    nth_arg(ls, visitas(pessoas, caso, ls), N)
%    N =  3
%******************************************************
%  %

nth_arg(Field,Table,Pos):-
    functor(Table,_,Nfields),
    nth_arg(Field,Table,Pos,1,Nfields).
nth_arg(Field,Table,Nfields,Nfields,Nfields):-
    !,
    arg(Nfields,Table,Field).
nth_arg(Field,Table,Pos,Pos,_NFields):-
   arg(Pos,Table,Field),!.
nth_arg(Field,Table,Pos,N,Nfields):-
   N1 is N+1,
   nth_arg(Field,Table,Pos,N1,Nfields).

%******************************************************
%  set_nth(List,Value,N,NewList)-set the nth element of a list
%    fails if N=0 or N>length(List)
%    Example:
%    : set_nth([a, b, s, d], c, 3, L)
%    L =  [a, b, c, d]
%******************************************************
%  %
set_nth(List,Value,N,NewList):-
   N1 is N-1,
   append(A,[_B|C],List),
   length(A,N1),
   append(A,[Value|C],NewList),!.
%****************************************************
% first_n(L,N,M) M is the list of the first N elements
%       of L. If length of L < N then M=N
%%
first_n(L,N,L):-
    length(L,I),
    I @=< N,!.
first_n(L,N,M):-
    append(M,_,L),
    length(M,N),!.
%****************************************************
% begins_with(Small,Long) Long list begins with small list
%%
begins_with(SmallList,BigList):-
     append(SmallList,_,BigList),!.

%***********************************************************
% export(F,_format,_fields,_predicate) outputs results of a query to a file
%     in a format usable by other programes.
%    F name of the file to which export
%    _format - choses the output format:
%            'tab' will separate field values by tabs
%            'del' will separate field values with commas
%   _fields - specifies the variables in the query that are to
%            be output.
%    _predicate - is the query.
%***********************************************************
%  %
export(F,_format,_fields,_predicate):-
      open_file_write(F),
       tell(F),
       exp(_format,_fields,_predicate).
exp(_format,_fields,_predicate):-
          call(_predicate),
          write_fields(_format,_fields),
           fail.
exp(_,_,_):-telling(F),close_file(F),told,!.
write_fields(del,[_f1|[_f2|_others]]):-
             write(_f1),put(44),
             write_fields(del,[_f2|_others]),!.
write_fields(tab,[_f1|[_f2|_others]]):-
             write(_f1),put(9),
             write_fields(tab,[_f2|_others]),!.
write_fields(_,[_f1|[]]):-
              write(_f1),nl,!.
write_fields(_,[]):-nl,!.
%*****************************************************************************
% list_to_a0(LIST,STRING) :- concatenates a list of atoms into a single one %
%%
list_to_a0([],'').
list_to_a0([A|R],S):-
    list_to_a0(R,S1),
    concat(A,S1,S),!.
%*****************************************************************
% list_to_a(LIST,STRING) :- 
%          takes a list of atoms and returns a single string
%          but puts spaces between the atoms.
%%

list_to_a([],'').
list_to_a([A],A).
list_to_a([A|R],S):-
    list_to_a(R,S1),
    concat(A,' ',A1),
    concat(A1,S1,S),!.
%**************************************************************
% l_dif(A,B,C)- C is the list of elements of A not in B %
l_dif([],_,[]):-!.
l_dif([A|B],M,X):-
    member_check(A,M),!,
    l_dif(B,M,X). 
l_dif([A|B],M,[A|X]):-
    l_dif(B,M,X),!.     

%*********************************************************
% B. Counters (from Prolog programming..., Claudia Marcus)
%*********************************************************
%      setcount(COUNTER,VALUE):-initializes a counter
%*********************************************************
% %
setcount(COUNTER,VALUE):-
   retract(count(COUNTER,_)),
   asserta(count(COUNTER,VALUE)),!.
setcount(COUNTER,VALUE):-
   asserta(count(COUNTER,VALUE)),!.

%*********************************************************
%      getcount(COUNTER,VALUE):-get counter value
%*********************************************************
% %
getcount(COUNTER,VALUE):-count(COUNTER,VALUE).
%*********************************************************
%      inccount(COUNTER,VALUE):-increment counter, return previous value.
%*********************************************************
% %
inccount(COUNTER,VALUE):-
   retract(count(COUNTER,VALUE)),
   V is VALUE+1,
  asserta(count(COUNTER,V)),!.
%*********************************************************
%      deccount(COUNTER,VALUE):-decrement value, retrun previous value.
%*********************************************************
% %
deccount(COUNTER,VALUE):-
   retract(count(COUNTER,VALUE)),
   V is VALUE-1,
  asserta(count(COUNTER,V)).
%*********************************************************
%      eracount(COUNTER,VALUE):-erase counter, returns current value.
%*********************************************************
% %
eracount(COUNTER,VALUE):-
   retract(count(COUNTER,VALUE)),!.

%*********************************************************************************
% C. Reporting (sending text to a disk file and to the console at the same time)
%*********************************************************************************
%    set_report_file(FILENAME) :- sets and opens FILENAME for output
%*********************************************************************************
%%
set_report_file(FILENAME):-
    open_file_write(FILENAME),
    put_value(report,FILENAME),
    set_report(off),!.
%**********************************************************************************
%    set_report(X) :- if X is 'on' output is send to FILENAME and the console
%                     if X is 'off' output is send only to the console.
%**********************************************************************************
%%
report_status(X):- var(X),get_value(rep_stat,X),!.
report_status(X):- var(X), X=off,!.
report_status(X):-get_value(rep_stat,X),!.

set_report(on):-put_value(rep_stat,on),!.
set_report(off):-put_value(rep_stat,off),!.
set_report(X):- \+ var(X), X \= on, X \=off, writeln('** ERROR: Bad value for set_report'-X),!.

% report status - returns current status for report status set by set_report %

%**********************************************************************************
%    report(PREDICATE_LIST):- executes the predicates in PREDICATE LIST twice.
%                              The second time the output is redirected to FILENAME
%**********************************************************************************
%%
report(L):-report_status(X), X=off,repexec(L),!.
report(L):-
    report_status(on),
    repexec(L),
    get_value(report,FILENAME),
    telling(X),
    tell(FILENAME),
    repexec(L),
				tell(X),!.
report(L):-
    writeln('** ERROR: Problems in "report". Predicate list '-L),!.
repexec([]):-!.
repexec([A|B]):-!,call(A),repexec(B).
repexec(A):- !,call(A).
%***********************************************************************************
%    close_report_file:- closes current report file.
%***********************************************************************************
%%
close_report_file:-
    get_value(report, FILENAME),
    close_file(FILENAME),!.
