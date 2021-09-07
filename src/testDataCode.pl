/*
Test alternative methods for managing the CDS.
*/
:-use_module(dataCode).
:-use_module(lexical).

a_line2-->group,elements(E),{storeEls(E)}.
a_line2-->elements(E),{storeEls(E)}.

group-->[(names,N)],dataflag1,{newGroup(N),!}.
group-->fillSpace(__S),[(names,N)],dataflag1,{newGroup(N),!}.

elements([E|R])-->element(E),elements(R).
elements([])   -->[].

% triple quote handling 
% Everything inside """ ..... ... """ is stored as is
element(newElement(E)) -->[(names,E)],dataflag3,{!}. % must precede store core %
element(storeCore(E))  -->[(names,E)],{!}.
element(endElement)    -->dataflag2,{!}.


dataflag1-->[(dataflag,1)],{!}.
dataflag2-->[(dataflag,2)],{!}.
dataflag3-->[(dataflag,3)],{!}.
fillSpace(S)-->[(fill,S)],{!}.


newGroup2(group):-!.
newElement2(name) :- !.
storeCore2([a,gv,df,c]):-!.
endElement2 :-!.

