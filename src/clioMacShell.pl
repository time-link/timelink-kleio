% text of window:  ClioShellLPA.prol %
%'<LOAD>'(_):-clio_shell.
%******************************************************
%  clio_shell:-  version for LPA Macprolog version 3.0
%    SYSTEM DEPENDENT
%******************************************************
%  %

clio_shell:-
%    kill_lpa_menus,%
    clio_init,
    set_report_file('clio input.rpt'),
    set_report(on),
    put_value(structure,[]),
    put_value(data_files,[]),
    put_value(format,[]),
    screen(Depth,Width),
    Dout is Depth-110,
    Wout is Width-10,
    wsize('· Output Window',102,5,Dout,Wout),
    wfont('· Output Window','Monaco',0,9),
    pclio_version,
    % install clio input menus %
    install_menu('Clio input',['Structure...','Export format...','Data...',
                               '(-','Echo','Report',
                               '(-','Execute','(-','Structure only','Data only']),
    disable_item('Clio input','Execute'),
    disable_item('Clio input','Structure only'),
    disable_item('Clio input','Data only'),
    update_menu,
    !.
'Clio input'('Structure...'):-
    report([writeln('Structure definition file: ')]),
    old('TEXT',File),
    report([writeln(File),nl]),
    put_value(structure,File),
    update_menu.

'Clio input'('Data...'):-
    report([writeln('Data files: ')]),
    get_value(data_files,DFILES),
    mdialog(60,70,220,450,
      [button(190, 10, 20,70,'Ok'),
       button(190,260, 20,60,'Cancel'),
       button(190,100, 20,70,'Add...'),
       button(190,180, 20,70,'Delete'),
       menu(10,30,160,400,DFILES,[],SFILES)],
      Btn,
      data_choose(SFILES)),
    get_value(data_files,FILES),
    forall(member(D,FILES),report([tab(3),writeln(D),nl])),
    report([nl]),
    update_menu,!.
'Clio input'('Data...'):-update_menu,!.
data_choose(D,1,_):-!.
data_choose(D,3,_):-!,
    get_value(data_files,SFILES),
    old('TEXT',File2),
    append(SFILES,[File2],NFILES),
    setditem(D,5,[NFILES,[]]),
    put_value(data_files,NFILES),
    fail.
data_choose(D,4,SFILES):-!,
    get_value(data_files,FILES),
    remove_files(FILES,SFILES,NFILES),
    setditem(D,5,[NFILES,[]]),
    put_value(data_files,NFILES),
    fail.
remove_files([],_,[]):-!.
remove_files(F,[],F):-!.
remove_files([A|B],S,C):-
    member_check(A,S),
    remove_files(B,S,C),!.
remove_files([A|B],S,[A|C]):-
    \+ member_check(A,S),
    remove_files(B,S,C),!.



'Clio input'('Export format...'):-
    report([writeln('Export format file: ')]),
    old('PROL',File2),
    report([writeln(File2),nl]),
    del_old_format,
    put_value(format,File2),
    source_load(File2),
    whide(File2),
    update_menu.

'Clio input'('Structure only'):-
    wfront('· Output Window'),
    get_value(structure,Filename),
    break_fname(Filename,P,File,Base,Ext),
    set_path(P),
    list_to_a0([P,':',Base,'.','rpt'],Report),
    (report_status(on)->prepare_report(Report);true),
    banner(stru(Filename),
           ['Processing structure file',File]),
    (report_status(on)->close_report_file;true),
    !.

'Clio input'('Data only'):-
    wfront('· Output Window'),
    get_value(data_files,DFiles),
    member(DataFile,DFiles),
    process_data(DataFile),
	   fail.
'Clio input'('Data only'):-!.

process_data(DataFile):-
    put_value(data_file,DataFile),
    break_fname(DataFile,P,File,Base,Ext),
    set_path(P),
    list_to_a0([P,':',Base,'.','rpt'],Report),
    (report_status(on)->prepare_report(Report);true),
    banner(dat(DataFile),
           ['Processing Data file ',File]),
    (report_status(on)->close_report_file;true),
    !.

prepare_report(R):-
   set_report_file(R),
   set_report(on),
   report([pclio_version]),!.
   

del_old_format:-
    get_value(format,[]),!.
del_old_format:-
    get_value(format,W),
    source_close(W),!.
 

'Clio input'('Echo'):-
    (marked_item('Clio input','Echo')->put_value(echo,no);put_value(echo,yes)),
    get_value(echo,E),
    report([writeln('Echo'-E)]),
    update_menu.
'Clio input'('Report'):-
    (marked_item('Clio input','Report')->set_report(off);set_report(on)),
    report_status(X),
    report([writeln('Report'-X)]),
    update_menu.


    
'Clio input'('Execute'):-
    run.

kill_lpa_menus:-
    kill_menu('Find'),
    kill_menu('Windows'),
    kill_menu('Fonts'),
    kill_menu('Eval'),
    !.

update_menu:-
    check_item_echo,
    check_stru,
    check_data,
    check_format,
    check_report,
    check_execute,
    check_stru_only,
    check_data_only.

check_stru:-
    get_value(structure,S),
    S \= [],
    mark_item('Clio input','Structure...').
check_stru:-
    unmark_item('Clio input','Structure...').

check_data:-
    get_value(data_files,S),
    S \= [],
    mark_item('Clio input','Data...').
check_data:-
    unmark_item('Clio input','Data...').

check_format:-
    get_value(format,S),
    S \= [],
    mark_item('Clio input','Export format...').
check_format:-
    unmark_item('Clio input','Export format...').

check_report:-
    report_status(on),
    mark_item('Clio input','Report').
check_report:-
    unmark_item('Clio input','Report').

check_execute:-
    get_value(structure,S),
    S \= [],
    get_value(data_files,D),
    D \= [],  
    get_value(format,F),
    F \= [],
    enable_item('Clio input','Execute'),!.
check_execute:-
    disable_item('Clio input','Execute'),!.
check_stru_only:-
    get_value(structure,[]),
    disable_item('Clio input','Structure only'),!.
check_stru_only:-
    enable_item('Clio input','Structure only'),!.
check_data_only:-
    get_value(data_files,[]),
    disable_item('Clio input','Data only'),!.
check_data_only:-
    enable_item('Clio input','Data only'),!.

check_item_echo:-
    get_value(echo,yes) -> mark_item('Clio input','Echo');
                           unmark_item('Clio input','Echo').
run:-
    wfront('· Output Window'),
    get_value(structure,Filename),
    writeln(structure-Filename),
    break_fname(Filename,P,File,Base,Ext),
    set_path(P),
    list_to_a0([P,':',Base,'.','rpt'],Report),
    (report_status(on)->prepare_report(Report);true),
    banner(stru(Filename),
           ['Processing structure file',File]),
   (report_status(on)->close_report_file;true),
    get_value(data_files,DFiles),
    member(DataFile,DFiles),
    process_data(DataFile),
    fail.
run:-!.
%******************************************************
%  pclio_version prints version,
%    compiler version, date and time
%******************************************************
%  %
pclio_version:-
      clio_version(P),writeln(P),
      date(Day,Month,Year), D = Day-Month-Year,
      write(D),
      time(Hour,Min,_),Time = Hour-Min,
      tab(1),write(Time),
      ver(System,Version,Date,Serial),Vers = System-Version-Date-Serial,
      tab(1),writeln(Vers),
      space(E,S),writelistln(['Eval space:',E,'Total space:',S]),!.      

status:-
    windows(prog,List),
    member(W,List),
    wchg(W),
    writeln('Changed window'-W),
    fail.
status.
