:-module(shellUtil,[
    shell_to_list/3,
    shell_to_codes/3,
    count_lines/2
    ]).

/** <module> Utilities for shell commands
 
 */

%% shell_to_list(++ShellCMD,--Code,--List) is det.
%  
% Execute the ShellCMD return the shell Code and the List of lines output by shell.
% Note that this predicate works by redirecting output of shell command to a temp_file
% and then reading the file and returning its contents. So ShellCMD should not redirect
% output.
%
shell_to_list(ShellCMD,Code,List):-
    tmp_file_stream(text, TmpFile, Stream),
    close(Stream),
    atomics_to_string([ShellCMD, " > ",TmpFile],PipedCMD),
    %with_mutex('shell_output', sync_shell(PipedCMD,TmpFile,Code,Codes)),
    sync_shell(PipedCMD,TmpFile,Code,Codes),
    (Codes = [] 
        -> List = []
        ; (
            shell_codes_to_list(Codes,List)
        )
    ),!.

shell_codes_to_list(Codes,List):-
    string_codes(S,Codes),
    text_to_string(S, String),
    split_string(String,"\n","\n",L),!,
    string_list_to_atom(List,L),!.

string_list_to_atom([],[]):-!.
string_list_to_atom([A|B],[SA|SB]):-
    atom_string(A,SA),
    string_list_to_atom(B,SB).

sync_shell(Cmd,TmpFile,Code,Codes):-
    shell(Cmd,Code),
    read_file_to_codes(TmpFile, Codes, []),
    delete_file(TmpFile).
    
%% shell_to_codes(++ShellCMD,--Code,--List) is det.
%  
% Execute the ShellCMD return the shell Code and the output as a list of codes.
% This predicate is useful when grammars are used to parse the shell output.
%
% Note that this predicate works by redirecting output of shell command to a temp_file
% and then reading the file and returning its contents. So ShellCMD should not redirect
% output.
shell_to_codes(ShellCMD,Code,Codes):-
    tmp_file_stream(text, TmpFile, Stream),
    close(Stream),
    atomics_to_string([ShellCMD, " > ",TmpFile],PipedCMD),
    with_mutex('shell_output', sync_shell(PipedCMD,TmpFile,Code,Codes)),
    !.

%% count_lines(+File,?Count) is det.
%
% Returns number of lines in a file using _wc_ shell command.
%
% @param File   Shell File pattern, can include wild cards.
% @param Count  List of count(Lines,FileName) terms. If File is a pattern then the last count is count(TotalLines,total).
%
% ### Example
%
% ?- count_lines('*.pl',L),member(C,L),writeln(C),fail.
%
%      count(155,basicio.pl)
%      count(130,clioPP.pl)
%      count(22,clioStart.pl)
%      count(93,clioUtil.pl)
%      count(61,counters.pl)
%      count(388,dataCDS.pl)
%      count(446,dataCode.pl)
%      count(686,dataDictionary.pl)
%      count(103,dataSyntax.pl)
%      count(22,debug.pl)
%      count(126,errors.pl)
%      count(118,exputil.pl)
%      count(237,externals.pl)
%      count(641,gacto.pl)
%      count(1852,gactoxml.pl)
%      count(2999,inference.pl)
%      count(212,kleioFiles.pl)
%      count(263,lexical.pl)
%      count(443,mappings.pl)
%      count(115,reports.pl)
%      count(353,restServer.pl)
%      count(14,serverStart.pl)
%      count(64,shellUtil.pl)
%      count(321,struCode.pl)
%      count(392,struSyntax.pl)
%      count(684,swiCompatibility.pl)
%      count(176,swiShell.pl)
%      count(2,swiStart.pl)
%      count(103,tests.pl)
%      count(93,threadSupport.pl)
%      count(351,topLevel.pl)
%      count(323,utilities.pl)
%      count(61,verif.pl)
%      count(12049,total)
%      false.

count_lines(File,Count):-
    string_concat('wc -l ', File, Cmd),
    shell_to_list(Cmd,_,L),
    process_wc_output(L,Count).

process_wc_output([],[]):-!.
process_wc_output([Line|Lines],[Count|More]):-
        split_string(Line," ", " ",SLine),
        process_wc_line(SLine,Count),
        process_wc_output(Lines,More).

process_wc_line([Number,File],count(N,File)):-number_string(N,Number),!.
process_wc_line([""|R],Count):- process_wc_line(R,Count).
