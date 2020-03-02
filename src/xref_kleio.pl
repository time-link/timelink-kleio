/*
    This file allows cross referencing predicates in the source code for the kleio translator.
    It is usefull to ensure proper modular design, by showing which predicates are called

    */

?-xref_source(basicio).
?-xref_source(clioPP).
?-xref_source(counters).
?-xref_source(dataCDS).
?-xref_source(dataCode).
?-xref_source(dataDictionary).
?-xref_source(dataSyntax).
?-xref_source(errors).
?-xref_source(exputil).
?-xref_source(externals).
?-xref_source(gactoxml).
?-xref_source(inference).
?-xref_source(kleioFiles).
?-xref_source(lexical).
?-xref_source(logging).
?-xref_source(mappings).
?-xref_source(persistence).
?-xref_source(reports).
?-xref_source(restServer).
?-xref_source(serverStart).
?-xref_source(shellUtil).
?-xref_source(struCode).
?-xref_source(struSyntax).
?-xref_source(swiCompatibility).
?-xref_source(swiShell).
?-xref_source(threadSupport).
?-xref_source(tokens).
?-xref_source(topLevel).
?-xref_source(utilities).
?-xref_source(verif).

show_xref:-
    setof(X,xref_current_source(X),Sources),
    member(Source,Sources),
    nl,
    writeln('========================================================================'),
    writeln(Source),nl,
    show_usage_of_file(Source),nl,
    show_dependencies(Source),
    fail.
show_xref.

show_xref(OutputFile):-
    open(OutputFile,write,__stream,[lock(write),wait(true),alias(OutputFile)]),
    with_output_to(OutputFile,show_xref),
    close(OutputFile).


show_usage_of_file(Source):-
    writeln('Used by:'),    
    format('Line ~t~6|Predicate ~t~46|Called by  ~t~84|In File~n',[]),
    xref_defined(Source,C,local(Line)),xref_called(File,C,By), 
    Source \= File,
    format('[~w] ~t~6|~w ~t~46|~w ~t~84|~w~n',[Line,C,By,File]),
    fail.
show_usage_of_file(_).

show_dependencies(Source):-
    writeln('Dependencies:'),    
    format('Line ~t~6|Predicate ~t~46|Uses  ~t~84|In File~n',[]),  
    setof(dep(File,C,By,Line),
        (xref_called(Source,C,By), 
        xref_defined(File,C,local(Line)),
        Source \= File),Deps),
    member(dep(File,C,By,Line),Deps),
    format('[~w] ~t~6|~w ~t~46|~w ~t~84|~w~n',[Line,By,C,File]),
    fail.
show_dependencies(_).


