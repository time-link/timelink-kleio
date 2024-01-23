/** <module>tests-pldoc

Unit testing for Prolog
For docs see http://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)

To generate tests from this file do run_tests,
@author Joaquim Carvalho
@license MIT

*/


:- use_module(library(test_wizard)).
:- use_module(utilities).
:- use_module(persistence).
:- use_module(logging).
:- use_module(restServer).
:- use_module(threadSupport).
:- use_module(dataDictionary).
:- use_module(tokens).
%
% support for auto generation of tests
% to generate clean qrecord.queries and do 
% ?- make_tests(module_name, 'qrecord.queries', current_output).
%
:- set_prolog_flag(log_query_file, 'qrecord.queries').

% Tests of utilities.pl
% run_tests(utilities).

:- begin_tests(utilities).

test(member_check):-
    member_check(a,[c,a,d]).
test(member_check,[fail]):-
        member_check(_,[c,a,d]),
        fail.    
test(member_nth, [all(A==[3, 5, 7])]) :-
        member_nth(c, [a, b, c, d, c, e, c], A).
test(member_nth, [true(A==c), nondet]) :-
        member_nth(A, [a, b, c, d, c, e, c], 3).
test(member_nth, [all(A-B==[a-1, b-2, c-3, d-4])]) :-
        member_nth(A, [a, b, c, d], B).
test(list_to_a0):-
    L = [a,b,c,d],
    list_to_a0(L,A),
    atomic_list_concat(L, A).
test(list_to_a):-
        L = [a,b,c,d],
        list_to_a(L,A),
        atomic_list_concat(L, ' ', A).
:- end_tests(utilities).

% testing in swiCompatibility 
% run_tests(swiCompatibility)

:- begin_tests(swiCompatibility).
test(properties,[J == joaquim ]):-
        set_prop(person,name,joaquim),
        get_prop(person,name,J).

test(properties,[fail]):-
        set_prop(person,name,joaquim),
        get_prop(person,name,ian).    

test(properties,[true]):-
        del_prop(xpto,x). % succeeds even if property does not exist

test(properties,[true]):-
        del_props(xpto). % succeeds even if property does not exist        

:- end_tests(swiCompatibility).

:-begin_tests(dataDictionary).

test(clioStru_create_stru,[true]):-
        File = 'gacto2.str',
        set_prop(nomino,nomen,File),
        set_prop(nomino,translations,3),
        dataDictionary:create_stru(ok),
        clioStru(File).

test(clioStru_isDoc,[true]):-
        File = 'gacto2.str',
        set_prop(nomino,nomen,File),
        set_prop(nomino,primum,kleio),
        dataDictionary:create_stru(ok),
        isDoc(kleio).
test(clioGroup_create_groups,[true]):-
        Group = aGroup,
        create_groups([Group]),
        clioGroup(Group,Id),
        format('Creating group ~w~n',clioGroup(Group,Id)).

:- end_tests(dataDictionary).






        
        