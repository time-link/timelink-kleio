
:- begin_tests(kleiofiles).

:- use_module(kleioFiles).

test(kleio_file_set_basic, [condition(true)]) :-
    kleio_home_dir(Home),
    atom_concat(Home, '/sources/reference_sources/roisdeconfessados/rol1.cli', File),
    exists_file(File),
    kleio_file_set(File, Set),
    assertion(is_list(Set)),
    assertion(member(kleio(_), Set)),
    assertion(member(rpt(_), Set)),
    assertion(member(err(_), Set)),
    assertion(member(xml(_), Set)).

test(kleio_file_set_content, [condition(true)]) :-
    kleio_home_dir(Home),
    atom_concat(Home, '/sources/reference_sources/roisdeconfessados/rol1.cli', File),
    exists_file(File),
    kleio_file_set(File, Set),
    member(kleio(Attrs), Set),
    assertion(member(name('rol1.cli'), Attrs)),
    assertion(member(base('rol1'), Attrs)),
    assertion(member(extension('cli'), Attrs)),
    assertion(member(tstatus(_), Attrs)), !.

test(kleio_file_set_tstatus_T, [condition(true)]) :-
    % This test assumes rol1.cli has not been translated or RPT is missing/old
    % Based on the manual test output where tstatus(T) was seen.
    kleio_home_dir(Home),
    atom_concat(Home, '/sources/reference_sources/roisdeconfessados/rol1.cli', File),
    exists_file(File),
    kleio_file_set(File, Set),
    member(kleio(Attrs), Set),
    member(tstatus(Status), Attrs),
    assertion(Status == 'T'), !.

test(kleio_file_set_directory, [condition(true)]) :-
    kleio_home_dir(Home),
    atom_concat(Home, '/sources/reference_sources/roisdeconfessados', Dir),
    exists_directory(Dir),
    kleio_file_set(Dir, Set),
    member(kleio(Attrs), Set),
    assertion(member(tstatus('D'), Attrs)),
    assertion(member(is_directory(yes), Attrs)), !.

:- end_tests(kleiofiles).
