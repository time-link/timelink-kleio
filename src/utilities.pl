:- module(utilities,
        [
            member_check/2,
            member_nth/3,
            nth_arg/3,
            set_nth/4,
            first_n/3,
            export/4,
            list_to_a0/2,
            list_to_a/2,
            l_dif/3,
            begins_with/2,
            gensymbol/2,
            gensymbol_local/2,
            init_gensym/1,
            setgensymbol/2,
            setgensymbol_local/2,
            a_upper_to_lower/2,
            upper_to_lower/2,
            afirst_n/3,
            print_with_newlines/1
        ]).

:-reexport('swiCompatibility').
:-use_module(persistence).

/** <module> Utilities

 # Utilities.pl

Contains usefull predicates or programs im plemented on
standard (Edinburgh) PROLOG. System dependent predicates,
even if they would qualify as "utilities", are in a special
file, called xxxx.compatibility.pl, where xxxx identifies the
system to which they apply.

*/

% this is shared by all the threads and used in counters.
:-dynamic(current_num/2).

%***********************************************************
%    A.general purpose utilities
%
%***********************************************************

%!  member_check(+X,+L:list) is det.
%
%   Check if X is a member of list L.
%   Non backtrackable version of member.
%
% @tbd use bultin memberchk/2
%
member_check(X,[X|_Xs]):-!.
member_check(X,[Y|Ys]):-not( X = Y),
            member_check(X,Ys),!.

%%  member_nth(?E,+L,?N) is nondet.
%   E is the Nth element of L
%
member_nth(A,L,N):-member_nth(A,L,1,N).
member_nth(A,[A|_B],N,N).
member_nth(A,[_B|C],N,N1):-
    N2 is N+1,
    member_nth(A,C,N2,N1).

%% nth_arg(+Arg,+Functor,?N) is det.
%   Return in N the Arg position in functor
%
% Example:
%
% ?- nth_arg(peter, persons(john, mary, peter), N)
%
%    N = 3
%

nth_arg(Field,Table,Pos):-
    functor(Table,_,Nfields),
    nth_arg(Field,Table,Pos,1,Nfields).
nth_arg(Field,Table,Nfields,Nfields,Nfields):-
    !,
    arg(Nfields,Table,Field).
nth_arg(Field,Table,Pos,Pos,__NFields):-
   arg(Pos,Table,Field),!.
nth_arg(Field,Table,Pos,N,Nfields):-
   N1 is N+1,
   nth_arg(Field,Table,Pos,N1,Nfields).

%%  set_nth(+List,+Value,+N,?NewList) is det.
%  Set the nth element of a list.
%  Fails if N=0 or N>length(List)
%
% Example:
%
% ?- set_nth([a, b, s, d], c, 3, L)
%
%    L =  [a, b, c, d]
%
set_nth(List,Value,N,NewList):-
   N1 is N-1,
   append(A,[_B|C],List),
   length(A,N1),
   append(A,[Value|C],NewList),!.

%% first_n(+L,+N,?M) is det.
%  M is the list of the first N elements
%       of L. If length of L < N then M=N
%
first_n(L,N,L):-
    length(L,I),
    I @=< N,!.
first_n(L,N,M):-
    append(M,_,L),
    length(M,N),!.

%% begins_with(?Small,?Long) is nondet.
%   Long list begins with Small list
%
begins_with(SmallList,BigList):-
     append(SmallList,_,BigList),!.

%% export(+File,+Format,+Fields,+Predicate) is det.
%  Outputs results of a query to a file
%     in a format usable by other programes.
%
%  @param File      path of the file to which to export
%  @param Format   choses the output format:
%          * 'tab' will separate field values by tabs
%          * 'del' will separate field values with commas
%  @param Fields    specifies the variables in the query that are to
%            be output.
%  @param  Predicate - is the query.
%
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

%% list_to_a0(+LIST,-STRING) is det.
% Concatenates a list of atoms into a single one %
%
list_to_a0([],'').
list_to_a0([A|R],S):-
    list_to_a0(R,S1),
    concat(A,S1,S),!.
%% list_to_a(+LIST,-STRING) is det.
%
%  Takes a list of atoms and returns a single string
%  but puts spaces between the atoms.
%
list_to_a([],'').
list_to_a([A],A).
list_to_a([A|R],S):-
    list_to_a(R,S1),
    concat(A,' ',A1),
    concat(A1,S1,S),!.

%% l_dif(+A,+B,?C) is det.
% C is the list of elements of A not in B
%
l_dif([],_,[]):-!.
l_dif([A|B],M,X):-
    member_check(A,M),!,
    l_dif(B,M,X).
l_dif([A|B],M,[A|X]):-
    l_dif(B,M,X),!.

%% gensymbol(+ROOT,-SYMBOL) is det.
% generates a symbol from ROOT.
%  Ex. gensym(root,X) would return with X=root1,
%  successif calls will give root2, root3, etc...
%  See Cocklish and Mellish, p.153 ff.
%
%  NOTE This is not thread local, so generated symbols all be unique across running threads.
% @see gensymbol_local/2 for a thread local version.
gensymbol(Root,Atom):-
    get_num(Root,Num),
    atom_concat(Root,Num,Atom),!.

 get_num(Root,Num):-
    get_shared_prop(current_num,Root,Num1),!,
    Num is Num1+1,
    set_shared_prop(current_num,Root,Num).
 get_num(Root,1):-set_shared_prop(current_num,Root,1),!.

%% gensymbol_local(+ROOT,-SYMBOL) is det.
%  Generates a symbol from ROOT. Symbols are unique to the local thread.
%  This means that paralel threads will generate the same symbols.
%
% @tbd TODO: reimplement gensymbol after deciding if it is thread local
% or not. Currently is global.
%
gensymbol_local(Root,Atom):-
    get_num_local(Root,Num),
    atom_concat(Root,Num,Atom),!.

 get_num_local(Root,Num):-
    get_prop(current_num,Root,Num1),!,
    Num is Num1+1,
    set_prop(current_num,Root,Num).
 get_num_local(Root,1):-
    set_prop(current_num,Root,1),!.

 %%  setgensymbol(+S,+N) is det.
 %    resets gensymbol counter.
 %
 setgensymbol(S,N) :- set_shared_prop(current_num,S,N),!.

 %%  setgensymbol_local(+S,+N) is det.
 %    resets gensymbol counter.
 %
 setgensymbol_local(S,N) :- set_prop(current_num,S,N),!.

 %% init_gensym(+ANON) is det.
 % Does nothing. Kept for not breaking the code since funcionality is purely cosmetic.
 % Originally allowed to init a gensym generator with a given number
 % so that generated symbols would start from that number. In SWI native
 % implementation of gensym it always start with zero.
 % Can't do in swi without reimplementing the whole gensym stuff.
 %
 init_gensym(__ANONYMOUS):-!.

%% a_upper_to_lower(+Upper,?Lower) is det.
% Lower is lowercase  version of Upper.
%
a_upper_to_lower(U,L):-
    name(U,UL),upper_to_lower(UL,LL),
    name(L,LL),!.
upper_to_lower([],[]):-!.
upper_to_lower([Char|Rest],[Char|NRest]):-
      char_type(Char,lower),
      upper_to_lower(Rest,NRest),!.
upper_to_lower([Char|Rest],[NChar|NRest]):-
     char_type(Char,upper),
     NChar is Char+32,
     upper_to_lower(Rest,NRest),!.

%% afirst_n(+Atom,+N,?Prefix) is det.
%
% truncate the Atom to the first N characters. If Atom is a number, return the number.
%
afirst_n(S,_,S):-number(S),!.
afirst_n(S,N,T):-
    stringof(L,S),
    first_n(L,N,M),
    stringof(M,T),!.

%% print_with_newlines(+Atom) is det.
%
%  Prints an atom, interpreting literal '\n' as newlines.
print_with_newlines(Atom) :-
    % 1. Split the atom into parts using '\n' as a delimiter.
    %    The backslash must be escaped: '\\n'.
    atomic_list_concat(Parts, '\\n', Atom),

    % 2. Join the parts back together using the '~n' format specifier.
    atomic_list_concat(Parts, '~n', FormatString),

    % 3. Use format/2 to print the result.
    format(FormatString, []).

/*
## Pre-2007-Git History

 Joaquim Carvalho, Florence, May 88
 Revision history:
    created 17 May 1988
    Modified 27 August 90
    'reverse' deleted and moved to the 'compatibility file'
    since it is built-in in version 2.0w of LPA
    nth_arg added 17 Sept 1990
    Reporting predicates added in November 1990
    writeln(X) moved to compatibility file FEB 1991
    Split facts moved out to a separated file in FEB 1991
    Several new predicates moved here from other programs
      list_to_a, list_to_a0, first_n, beggins_with,l_dif,
      last... FEB 1991.
    report(Predicate list) can now be called without calling
      set_report before (Out 93).

    removed a dependency on the lst (is_a_list) predicate (Nov 2000).

   Revision 1.1.1.1  2001/04/19 23:25:44  jrc
   New Repository.

   Revision 1.1.1.1  2001/04/18 23:34:39  jrc
   CVS repository moved from llinux to MacOSX.

   Revision 1.2  2006/05/16 10:52:51  Joaquim
   Update dos ficheiros prolog

   Revision 1.3  2001/01/15 18:47:39  jrc
   Line ends corrected

     Revision 1.3  2005/03/10 14:42:28  joaquim
     snapshot commit for purpose of moving the cvs directory.

     Revision 1.2  2004/05/10 06:23:39  joaquim
     No change except noting that the semantics of last
     in swi-prolog changed again. It now is last(LIST,Element)
     as in ?- last([a,c,d,f,v,g],A).
     A = g

     Revision 1.1  2004/04/08 14:45:24  ltiago
     Source code has passed from Joaquim to Tiago.
     Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
     File build.xml, web.xml and velocity.properties were changed

     Revision 1.2  2004/01/03 10:19:02  jrc
     Removed the last predicate because it became builtin in swi prolog 5.0.10. Note that the semantics of "last" in swi is different from what it was here: last(Element,List) in swi instead of last(List,Element) as defined previously here. Only dataCDS.pl used the last predicate

 */