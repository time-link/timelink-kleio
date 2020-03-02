%******************************************************
% utility predicates for ln5 export */
%    
%******************************************************
%  */
%******************************************************
% ln5_extra_info: gathers extra information not contained
% in the core data of an element list. This includes comment
% and original wording aspects, and elements not contained
% in the list
%******************************************************
%  */

ln5_extra_info(Els,Extra):-
    rmore_aspects(Els,Xasp), % get other aspects */
    getCDElement_list(AllEls),
    l_dif(AllEls,Els,MoreEls), % get extra elements */
    rmore_els(MoreEls,Xmore),
    append(Xasp,Xmore,Extra),!.

rmore_aspects([],[]):-!.
rmore_aspects([E|M],Xasp):-
    get_aspect0(comment,E,C),
    rcomm(E,C,Xcom),
    get_aspect0(original,E,O),
    rorg(E,O,Xorg),
    append(Xcom,Xorg,X1),!,
    rmore_aspects(M,X2),
    append(X1,X2,Xasp),
    !.

rmore_els([],[]):-!.
rmore_els([E|M],Extra):-
    get_aspect(core,E,CO),
    rcore(E,CO,Xcore),
    get_aspect(comment,E,C),
    rcomm(E,C,Xcom),
    get_aspect(original,E,O),
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
    append(['Express�o original do campo'-E,':' |[A|B]],['.'],X),!.
rorg(E,mult([A|B]),X):-
    join_mult([A|B],C),
    append(['Express�es originais do campo'-E,':' |C],['.'],X),!.

rcore(_,[],[]):-!.
rcore(_,mult([[],[]]),[]):-!.
rcore(E,[A|B],X):-
    append(['Campo adicional'-E,':' |[A|B]],['.'],X),!.
rcore(E,mult([A|_]),X):-
    append(['Campo adicional de v�rias entradas '-E,':' |A],['.'],X),!.



rexp(FILE,FIELDS):-
    get_fname(FILE,F),
    tell(F),rexpfields(FIELDS),tell(user),!.

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

