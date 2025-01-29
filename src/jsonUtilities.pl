:- module(jsonUtilities,
        [
            dict_json_string/2,
            prolog_to_json/2
        ]).

:- use_module(library(http/json)).
/** <module> Json utilities.
**/

dict_json_string(Dict,String):-
    with_output_to(string(String), json_write(current_output,Dict)),
    !.


/** prolog_to_json
DEPRECATED We use swi-prolog dictionnaries for the same purpose.

Utility predicate to convert a Prolog data structure to a json term that can be passed on to json_write/2
Based on the conventions of json_read/2

*/
prolog_to_json(PrologList,Json):-
    ptoj(PrologList,Json),!.
prolog_to_json([],null).

ptoj(P,Json):-
    is_list(P),
    ptojlist(P,J),
    (list_of_attributes(P)->
        Json=json(J)
    ;
        Json=J).
ptoj([],[]).

ptoj(A,A):-
    is_dict(A),!.
ptoj(P,P):-
    number(P).
ptoj(@(true),true).
ptoj(@(false),false).
ptoj(@(null),null).
ptoj(P,J):-
    atom(P),
    atom_string(P,S),
    ptoj(S,J).
ptoj(P,J):-
    string(P),
    format(string(J),'~s',[P]).
ptoj(T=V,J):-!,
    ptoj(T,JT),
    ptoj(V,JV),
    atom_string(AJT,JT),
    J =.. [AJT,JV].
ptoj(P,json([J])):-!,
    P =.. [T,V],
    ptoj(T,JT),
    ptoj(V,JV),
    atom_string(AJT,JT),
    J =.. [AJT,JV].


ptojlist([P|MP],[J|MJ]):-
    ptoj(P,J),
    ptojlist(MP,MJ).
ptojlist([],[]).

list_of_attributes([A|B]):-
    functor(A,(=),2),
    list_of_attributes(B).
list_of_attributes([]).


tptoj(P,J):-
    prolog_to_json(P,J),
    writeln('========='),
    writeln(J),
    writeln('========='),
    current_output(Stream),
    json_write(Stream,J),nl,
    writeln('=========').
jtoj(J):-
    writeln('========='),
    writeln(J),
    writeln('========='),
    current_output(Stream),
    json_write(Stream,J),nl,
    writeln('=========').
