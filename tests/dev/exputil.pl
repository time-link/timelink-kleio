:-module(exputil,[
    join_mult/2
]).

/** <module> Utility predicates for data export 

    This was used for exporting data in tabular form.
    But could receive code for multi format export (xml,json)

    ln5 (old tab separated files for import in relation databases) not used any more
*/
:-use_module(dataCDS).
:-use_module(utilities).

/******************************************************
 ln5_extra_info: gathers extra information not contained
 in the core data of an element list. This includes comment
 and original wording aspects, and elements not contained
 in the list
******************************************************
  */

ln5_extra_info(Els,Extra):-
    rmore_aspects(Els,Xasp), % get other aspects */
    clio_elements(AllEls),
    l_dif(AllEls,Els,MoreEls), % get extra elements */
    rmore_els(MoreEls,Xmore),
    append(Xasp,Xmore,Extra),!.

rmore_aspects([],[]):-!.
rmore_aspects([E|M],Xasp):-
    clio_aspect(comment,E,C),
    rcomm(E,C,Xcom),
    clio_aspect(original,E,O),
    rorg(E,O,Xorg),
    append(Xcom,Xorg,X1),!,
    rmore_aspects(M,X2),
    append(X1,X2,Xasp),
    !.

rmore_els([],[]):-!.
rmore_els([E|M],Extra):-
    clio_aspect(core,E,CO),
    rcore(E,CO,Xcore),
    clio_aspect(comment,E,C),
    rcomm(E,C,Xcom),
    clio_aspect(original,E,O),
    rorg(E,O,Xorg),
    append(Xcore,Xcom,X0),
    append(X0,Xorg,X1),
    rmore_els(M,X2),
    append(X1,X2,Extra),!.
    

rcomm(_,[],[]):-!.
rcomm(_,mult([[],[]]),[]):-!.
rcomm(E,[A|B],X):-
    append(['Comentario ao campo'-E,':' |[A|B]],['.'],X),!.

rcomm(E,mult([A|B]),X):-
    join_mult([A|B],C),
    append(['Comentarios ao campo'-E,':' |C],['.'],X),!.

rorg(_,[],[]):-!.
rorg(_,mult([[],[]]),[]):-!.
rorg(E,[A|B],X):-
    append(['Expressão original do campo'-E,':' |[A|B]],['.'],X),!.
rorg(E,mult([A|B]),X):-
    join_mult([A|B],C),
    append(['Expressões originais do campo'-E,':' |C],['.'],X),!.

rcore(_,[],[]):-!.
rcore(_,mult([[],[]]),[]):-!.
rcore(E,[A|B],X):-
    append(['Campo adicional'-E,':' |[A|B]],['.'],X),!.
rcore(E,mult([A|_]),X):-
    append(['Campo adicional de várias entradas '-E,':' |A],['.'],X),!.



rexp(FILE,FIELDS):-
    tell(FILE),rexpfields(FIELDS),tell(user),!.

rexpfields([]):-nl,!.
rexpfields([A|B]):-
    rexpfield(A),
    rexpsep(A,B),
    rexpfields(B),!.

rexpsep(link(_),_):-nl,!. % after a link a return */
rexpsep(_,[link(_)|_]):-nl,!. % before a link a return */
rexpsep(_,[]):-!. % last field: no separator */
rexpsep(_,_):-put(9),!. % else: tab */

rexpfield([]):-
    write(''),!.
rexpfield([A|B]):-writelist0([A|B]),!.
rexpfield(mult([A|B])):-
    join_mult([A|B],L),  % join into a single list */
    writelist0(L),!. 
rexpfield(link(L)):- % a link */
    rexplink(L),!.
rexpfield(A):-
    atomic(A),
    write(A),!.
rexpfield(A):-
    A \=[_|_],
    functor(A,F,_),
    F \=link,
    write(A),!.

rexplink(mk(KEYS)):- % multikey fields to reflex */
   rexpfields(KEYS),!.

rexplink([]):-nl,!.
rexplink(A):-
   atomic(A),
   rexpfield(A),
   nl,!.
rexplink([A|B]):-
    rexpfield([A|B]),
    nl,!.
rexplink(mult([A|B])):-
    rexplink(A),
    rexplink(mult(B)),!.
rexplink(mult([])):-!.

%% join_mult(+A,?B) is det.
%    joins a list of lists into a single
%    list separating the values of the original lists by semicolons.
%    Usefull to transform multiple entries into single entries
%
% Example:
% :- join_mult([[a, b, c], [d, e, f], [g, h, i]], C)
%   C =  [a, b, c, ;, d, e, f, ;, g, h, i]
%
join_mult([A],A):-!.
join_mult([],[]):-!.
join_mult([A|B],J):-
   join_mult(B,C),
   append(A,[';' |C],J),!.
    