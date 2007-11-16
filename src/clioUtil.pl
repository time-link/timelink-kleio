% vim: filetype=prolog ts=3
% $Date$ 
% $Author$
% $Id$
% $Log: clioUtil.pl,v $
% Revision 1.2  2006/05/16 10:52:42  Joaquim
% update dos ficheiros prolog
%
% Revision 1.1  2004/04/08 14:45:24  ltiago
% Source code has passed from Joaquim to Tiago.
% Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
% File build.xml, web.xml and velocity.properties were changed
%
% Revision 1.1.1.1  2001/04/19 23:25:43  jrc
% New Repository.
%
% Revision 1.1.1.1  2001/04/18 23:34:39  jrc
% CVS repository moved from llinux to MacOSX.
%
% Revision 1.2  2001/01/15 18:28:41  jrc
% Cleaned line endings and added cvs keywords
%
%******************************************************
%  join_mult(A,B) joins a list of lists into a single
%    list separating the values of the original lists by semicolons.
%    Usefull to transform multiple entries into single entries
% Example:
% : join_mult([[a, b, c], [d, e, f], [g, h, i]], C)
%   C =  [a, b, c, ;, d, e, f, ;, g, h, i]
% swi prolog version 18:19 06-11-1999
%******************************************************
%  %


join_mult([A],A):-!.
join_mult([],[]):-!.
join_mult([A|B],J):-
   join_mult(B,C),
   append(A,[';' |C],J),!.



afirst_n(S,_,S):-number(S),!.
afirst_n(S,N,T):-
    stringof(L,S),
    first_n(L,N,M),
    stringof(M,T),!.
    
% creates chartype predicates and
% outputs them to the screen 
%
% Usage: chartype("!@#$%^&"). 

createchartype([A|B]):-
    cct(A),
    createchartype(B).
createchartype([]):-!.
cct(X):-
     write('chartype('),
     write(X),
     write(','),
     put(39),put(X),put(39),
     write('):-!.'),nl.
%*************************************************************
% various string utilities
%*************************************************************
% %
a_upper_to_lower(U,L):-
    name(U,UL),upper_to_lower(UL,LL),
    name(L,LL),!.
upper_to_lower([],[]):-!.
upper_to_lower([Char|Rest],[Char|NRest]):-
      chartype(Char,lower),
      upper_to_lower(Rest,NRest),!.
upper_to_lower([Char|Rest],[NChar|NRest]):-
     chartype(Char,upper),
     NChar is Char+32,
     upper_to_lower(Rest,NRest),!.

%*************************************************************
% various properties utilities
%*************************************************************
% %
%**************************************
% add_to_prop(object,property,value) %
% adds value to property of object
%     previous property value must be 
%     a list or undefined
%     Does not add a value that is already
% in the property
%
% if it is not there add the value %
add_to_prop(O,P,V):- 
   get_prop(O,P,OldValue),
   \+ on(V,OldValue),
   set_prop(O,P,[V|OldValue]),!.
% it it is do nothing %
add_to_prop(O,P,V):-
   get_prop(O,P,OldValue),
   on(V,OldValue),!.
% if the property is not there add it %
add_to_prop(O,P,V):-
   \+ get_prop(O,P,_),
   set_prop(O,P,[V]),!.

%********************************************
% show_props(Atom).
%      displays all the properties and their values for ATOM).
%%
show_props(A):-show_props(A,1).
show_props(Atom,T):-
    get_props(Atom,List), % get all the properties of this atom%
    tab(T),write(Atom),nl,
    show_props(Atom,List,T),!.   % print one by one %
show_props(Atom,[Prop|OtherProps],T):-
    get_prop(Atom,Prop,Value),
    N is T+3,
    tab(N),write(Prop),write('='),write(Value),nl,
    show_props(Atom,OtherProps,T),!.
show_props(_,[],_):-!.
%******************************************************
%  showDataFlags : show current values for dataflags
%******************************************************
%  %
showDataFlags:-data_flag(N,F),chartype(C,F),
               name(S,[C]),
               write('dataflag '),
               write(N),tab(1),
               write(S),write(' ('),write(F),writeln(')'),
               fail,!.
showDataFlags.


