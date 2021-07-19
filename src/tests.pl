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
:- use_module(restServer).
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

%tests of restServer
% run_tests(rest).
:-begin_tests(rest).
tpath(Path,Action,Options,Object) :-
        restServer:tokenize_path(Path,L),
        phrase(restServer:rest_path(Action,Options,Object),L),
        format('~nPath: ~w ~nAction: ~w~nOptions:~w~nObject:~w~n',
                [Path,Action,Options,Object]).

test(rest) :-
        tpath('translations/soure/b1685.cli',
                'translations',[], 'soure/b1685.cli').

test(rest) :-
        tpath('translations/echo=yes/structure=gacto2.str/path=Users/soure/b1685.cli',
                translations,
                [echo(yes),structure('gacto2.str'),path('Users/soure/b1685.cli')],
                'Users/soure/b1685.cli'
                ) .

test(rest) :-
        tpath('translations/echo=yes/structure=users/soure/structures/gacto2.str/path=Users/soure/b1685.cli',
                translations,
                [echo(yes),structure('users/soure/structures/gacto2.str'),path('Users/soure/b1685.cli')],
                 'Users/soure/b1685.cli').


:- end_tests(rest).

% to debug rest server it must be started in the "start"
% process because the debugger runs in a separate process
% than the one run by OPTION X - L or whatever swipl instance is running on the terminal

start_servers:-
        put_value(pool_mode,debug), % debug, pool or message
        catch(start_rest_server,E,log_error('~w',[E])),
        catch(start_debug_server,E2,log_error('~w',[E2])).

start:-
        set_log_level(info),
        working_directory(DD,DD),
        log_debug('Current dir ~w run this from the clio directory !!!!!!!!~n~n',[DD]),            
        kleiofiles:find_files_with_extension('tests/kleio-home/sources/api_tests/testes/sources/clioPPTestes/',cli,_,Files),
        member(File,Files),
        concat('/translations/echo=yes/structure=tests/dev/gacto2.str/path=', File, PathInfo),
        Request = [path_info(PathInfo)],
        rest:process_rest(Request),
        %show_processing_status,
        %sleep(1),
        fail.
start :-!.

    start2:-
        sleep(5),
        log_debug('Run this from the clio directory !!!!!!!!~n~n',[]),
        Request = [path_info('/translations/echo=yes/structure=tests/dev/gacto2.str/path=tests/reference_sources/soure/documents/Chancelarias/J5.cli')],
        rest:process_rest(Request),
        get_queued(Q),get_processing(P),log_debug('QUEUED ~w ~nPROCESSING ~w~n',[Q,P]).
    start3:-
        sleep(5),
        log_debug('Run this from the clio directory !!!!!!!!~n~n',[]),
        Request = [path_info('/translations/echo=no/structure=tests/dev/gacto2.str/path=tests/reference_sources/soure/documents/Chancelarias/xpto_comuns_soure.cli')],
        rest:process_rest(Request),
        show_processing_status.
        
    start4:-
        sleep(5),
        log_debug('Run this from the clio directory !!!!!!!!~n~n',[]),
        Request = [path_info('/translations/echo=no/structure=tests/dev/gacto2.str/path=tests/test_translations/coja-rol-1841.cli')],
        show_processing_status.
    start5:-
        sleep(5),
        log_debug('Run this from the clio directory !!!!!!!!~n~n',[]),
        Request = [path_info('/translations/echo=no/structure=tests/dev/gacto2.str/path=tests/test_translations/bapt1714.cli')],
        rest:process_rest(Request),
        show_processing_status.

    start6:-
        sleep(5),
        log_debug('Run this from the clio directory !!!!!!!!~n~n',[]),
        Request = [path_info('/translations/echo=no/structure=tests/dev/gacto2.str/path=tests/test_translations/cas1714-1722-com-celebrante.cli')],
        rest:process_rest(Request),
        show_processing_status.

% testing processing of "escritura" which is taking a very long time.

test_escritura:-
        set_log_level(debug),
        restServer:translate(
                './tests/kleio-home/sources/test_translations/varia/auc_cartulario18.cli',
                './tests/kleio-home/system/conf/kleio/stru/gacto2.str',
                yes).
 
test_teste:-
        restServer:translate(
                './tests/kleio-home/sources/api_tests/testes/sources/clioPPTestes/teste.cli',
                './tests/kleio-home/system/conf/kleio/stru/gacto2.str',
                yes).


show_prolog_stack:-
        member(StackType,[local,global,trail]),
        prolog_stack_property(StackType,min_free(MF)),
        prolog_stack_property(StackType,low(Low)),
        prolog_stack_property(StackType,factor(Factor)),
        prolog_stack_property(StackType,spare(Spare)),
        format('Stack ~12w ~22t min_free: ~w low: ~w factor: ~w spare: ~w~n',[StackType, MF,Low,Factor,Spare]),
        fail.
show_prolog_stack:-!.



        
        