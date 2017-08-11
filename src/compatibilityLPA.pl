% text of window:  compatibility.lpa %
%*******************************************************************
% text of window:  compatibility.lpa 
% Contains predicate s to extend or modify LPA PROLOG on the Mac,
%   for compatibility with other Prolog's, namely Salford PROLOG
%   for the PRIME.
%  All these here are implementation dependent. Even if written
%   in standard Prolog, they are peculiar
%   to this Prolog version in the sense that in others they might
%   be built in.
%
%   Contents:
%     A. general purpose predicates
%       member(X,L) -> X is a member of list L
%       writeln (X) writes X and adds a carriage return
%       writelist0(L). writes on the atoms of a list
%       writelist0ln(L) same but with a return at the end
%       writelist(L) wirte the members of list L with a space in between
%       writelistln(L) same but with return at the end
%       ptime(C) show the time it takes to execute a predicate
%       break_fname(F,P,N,B,E): decomposes a filename
%
%     B. file handling predicates
%       open_file_read(F) -> Opens file for reading.
%       open_file_write(O) -> Creates or erases and opens file for writing.
%       close_file(F) -> closes file F.
%       consult_file(F) - adds predicates from file F
%
% 
% Joaquim Carvalho, Florence, May 88
%
% Revision history:
%    created 17 May 1988
%    modified for LPA version 2.0W in August 90
%    modified for LPA version 3.01w in September 1990.
%    bug in break_fname that prevented handling of files with
%    no extensions corrected in AUG 91
%    modified for LPA MacProlog32 version 1.06 in September 95
%    more LPA MacProlog32 compatibility predicates added Sept 96, Nov 96
%    map predicate added March 98
%*******************************************************************
% %
%*******************************************************************
%*******************************************************************
% A. General purpose predicates %
% Here you will find common primitives of Prolog that where not
% implemented as built-ins in this version.
% This is done here to avoid spreading of definitions of this type
% through the code where they would be tiresome to delete or add
% when the programs move around systems.
%
%
%**********************************************************
% writeln (X) writes X and adds a carriage return
%**********************************************************
% %
writeln(X):-write(X),nl.
%**********************************************************
% writelist0(L). writes on the atoms of a list %
% writelist0ln(L) same but with a return at the end %
%%
writelist0(L):-
    member(X,L),
    write(X),
    fail,!.
writelist0(_):-!.
writelist0ln(L):-writelist0(L),nl.

%**********************************************************
% writelist(L) wirte the members of list L with a space in between %
% writelistln(L) same but with return at the end
%%
writelist([X|[Y|Z]]):-write(X),tab(1),writelist([Y|Z]),!.
writelist([X|[]]):-write(X),!.
writelistln(L):-writelist(L),nl.
%**********************************************************
% get_time(CALL,TIME) time is the time in seconds that
%          CALL takes to execute
%%
get_time(Call,Time):-
    ms(Call,T),
    Time is (T/1000)/60.
%**********************************************************
% ptime(CALL) execute CALL and print the time it takes to execute
%%
ptime(Call):-ptime(1,Call).
ptime(N,Call):-
    get_time(Call,Time),
    stime(Time,H-M-S),
    functor(Call,C,_),
    tab(N),write('Time for '),write(C),tab(2),
    write(H),write(h),
    write(M),write(m),
    write(S),writeln(s),!.

stime(T,H-M-S):-
    H1 is (T/3600),
    H is int(H1),
    M1 is (T-H*3600)/60,
    M is int(M1),
    S is T-H*3600-M*60,
    !.

%******************************************************************
% break_fname(F,P,N,B,E): decomposes filename F into
%                         P - path
%                         N - name of file
%                         B - base of name of file (without extensions)
%                         E - extensions
%  Example:
% break_fname('QuimCi:Soure:Soure Clio fonte:casamentos:cas1695.cli',
%               P,N,B,E)
% returns
%     P = 'QuimCi:Soure:Soure Clio fonte:casamentos'
%     N = 'cas1695.cli'
%     B = 'cas1695'
%     E = 'cli'
%******************************************************************
%%
break_fname(FullName,Path,FileName,Base,Extension):-
    stringof(Chars_Full_Name,FullName),reverse(Chars_Full_Name,RCFN),
    append(RFileName,[':'|RPATH],RCFN),
    reverse(RFileName,CharsFileName),reverse(RPATH,CPATH),
    stringof(CharsFileName,FileName),stringof(CPATH,Path),
    (append(CharsBase,['.'|Cext],CharsFileName)->
         stringof(CharsBase,Base); 
         Base =FileName,Cext=[]),
    stringof(Cext,Extension),!.

%******************************************************************
%  B. file handling predicates %
%
%******************************************************************
%       open_file_read(F) -> Opens file for reading.
%*******************************************************************
% %
open_file_read(F):-open(F,read).

%******************************************************************* 
%       open_file_write(O) -> Creates or erases and opens file for writing.
%*******************************************************************
% % 
open_file_write(O):-open(O,write).

%*******************************************************************
%
%   close_file(F). Closes file F. F must be instantiated with the
%                    name of a currently open file.
%*******************************************************************
% %
close_file(F):-close(F).
%******************************************
% consult_file(F) - adds predicates from file F
%*******************************************
% %
consult_file(F):-
   open_file_read(F),
   see(F),
   load_data.
load_data:- read(X),
                   assert(X),
                   write(X),nl,
                   load_data.
load_data:-seeing(F),close(F),seen.

%*******************************************
% Compatibility for MacProlog32
%*******************************************
%%
% sort/4
sort( List, Sorted, Path, -1 ):-
  !,
  sort( List, Sorted, Path ).
sort( List, Sorted, Path, 1 ):-
  sort( List, SortedAsc, Path ),
  reverse( SortedAsc, Sorted ).

+(X,Y,Z) :- Z is X+Y.
-(X,Y,Z) :- Z is X-Y.
*(X,Y,Z) :- Z is X*Y.
/(X,Y,Z) :- Z is X/Y.
Ö(X,Y,Z) :- Z is X/Y.
%(X,Y,Z) :- Z is X%Y.
^(X,Y,Z) :- Z is X^Y.
pwr(X,Y,Z) :- Z is X^Y.
mod(X,Y,Z) :- Z is X mod Y.
/\(X,Y,Z) :- Z is X /\ Y.
\/(X,Y,Z) :- Z is X \/ Y.
>>(X,Y,Z) :- Z is X >> Y.
<<(X,Y,Z) :- Z is X << Y.
È(X,Y,Z) :- Z is X >> Y.
Ç(X,Y,Z) :- Z is X << Y.
\(X,Z) :- Z is \(X).
sqrt(X,Z) :- ( number(X) -> Z is sqrt(X) ; X is Z*Z ).
abs(X,Z) :- Z is abs(X).
int(X,Z) :- Z is int(X).
sign(X,Z) :- Z is sign(X).
ln(X,Z) :- Z is ln(X).
cos(X,Z) :- Z is cos(X).
sin(X,Z) :- Z is sin(X).
tan(X,Z) :- Z is tan(X).
pi( PI ) :- PI is pi.

% file commands %
%****************************************
File handling.
Files in MacProlog32 are referred to by a full or partial pathname,
or a logical file specification tuple,
rather than File, Vol.
*****************************************%

% bload/[2,3]
% No special loading required for Boot files
bload( File ):-
  ensure_loaded( File ).

bload( File, Vol ):-
  ensure_loaded( File ).


% cload/[1,2]
cload( File ):-
  ensure_loaded( File ).

cload( File, Vol ):-
  ensure_loaded( File ).


% create/[1,3]
% NB create is now handled by open/2
create( File ):-
  open( File, write ).
create( File, Vol, Type ):-
  open( File, write ),
  ftype( File, Creator, _ ),
  stype( File, Creator, Type ).


% delete/2
delete( File, Vol ):-
   delete( File ).


% fcdate/[4,5]
fcdate( File, Vol, Yr, Month, Day ):-
    fcdate( File, Yr, Month, Day ).
fcdate( File, Yr, Month, Day ):-
    fcdatetime( File, [Yr, Month, Day, _, _, _] ).


% fcopy/4
fcopy( F1, V1, F2, V2 ):-
  fcopy( F1, F2 ).


% fctime/[4,5]
fctime( File, Vol, Hr, Min, Sec ):-
    fctime( File, Hr, Min, Sec ).
fctime( File, Hr, Min, Sec ):-
    fcdatetime( File, [ _, _, _, Hr, Min, Sec ] ).


% find_file/2
% Discontinued in MacProlog32
find_file( File, _ ).


% flen/3
flen( File, Vol, Len ):-
  flen( File, Len ).


% fmdate/[4,5]
fmdate( File, Vol, Yr, Month, Day ):-
    fmdate( File, Yr, Month, Day ).
fmdate( File, Yr, Month, Day ):-
    fmdatetime( File, [Yr, Month, Day, _, _, _] ).


% fmtime/[4,5]
fmtime( File, Vol, Hr, Min, Sec ):-
    fmtime( File, Hr, Min, Sec ).
fmtime( File, Hr, Min, Sec ):-
    fmdatetime( File, [ _, _, _, Hr, Min, Sec ] ).


% fsize/3
fsize( File, Vol, Size ):-
  fsize( File, Size ).


% ftype/4
ftype( File, Vol, Type, Creator ):-
  ftype( File, Type, Creator ).


% get_current_file/3
get_current_file( File, Vol, Type ):-
   get_current_file( File, Type ).


% get_path/1
get_path( Name ):-
   var( Name ),
   dvol( Name ).


% launch_app/5
launch_app( File, Vol, Switch, Continue, PSN ):-
			launch_app( File, Switch, Continue, PSN ).


% new/4
% NB new/3 requires File, Prompt, InitName as arguments
% new/2 requires File, Prompt
new( File, Vol, Prompt, InitName ):-
  new( File, Prompt, InitName ).


% old/3
old( FTypes, File, Vol ):-
  old( FTypes, File ).


% NB open/2 requires FileName and Mode as arguments


% prolog_path/1
prolog_path( Path ):-
  var( Path ),
  absolute_file_name( prolog(''), Path ).


% prolog_vol/0
prolog_vol:-
   dvol( prolog ).

%% prolog_vol/1  discontinued


% rename/3 
rename( File, Vol, NewName ):-
  rename( File, NewName ).


% res_create/2
res_create( File, Vol ):-
  res_create( File ).


% res_open/2
res_open( File, Vol ):-
  res_open( File ).


% seek/[2,3]
seek( File, Posn ):-
  stream_position( File, Posn ).

seek( File, 1, Posn ):-
  stream_position( File, _, Posn ).

seek( File, 2, Posn ):-
  flen( File, Len ),
  NewPosn is Len - Posn,
  stream_position( File, _, NewPosn ).
  
seek( File, 3, Move ):-
  stream_position( File, NowPosn ),
  NewPosn is NowPosn + Move,
  stream_position( File, _, NewPosn ) .


% set_path/1
set_path( Path ):-
   writeln(Path),
   dvol( Path ).


% source_close/1
source_close( File ):-
  abolish_files( [File] ).


% source_load/1
source_load( File, Vol ):-
  source_load( File ).


% stype/4
stype( File, Vol, Type, Creator ):-
  stype( File, Type, Creator ).


% tload/2
tload( File, Vol ):-
   tload( File ).


% tsave/3
tsave( File, Vol, Wins ):-
   tsave( File, Wins ).


%% vol_path/2  discontinued

% stringof/2
stringof( ListOfChars, Atom ):-
  atom( Atom ),
  !,
  name( Atom, ListOfBytes ),
  bytes_to_chars( ListOfBytes, ListOfChars ).
stringof( ListOfChars, Atom ):-
  bytes_to_chars( ListOfBytes, ListOfChars ),
  atmbyt( Atom0, ListOfBytes ),
  Atom = Atom0.

bytes_to_chars( [], [] ).
bytes_to_chars( [Byte|Bytes], [Char|Chars] ):-
  charof( Char, Byte ),
  bytes_to_chars( Bytes, Chars ).

on(X,Y):-member(X,Y).

%**************************
 Generalized map program
map/[2,3,4,5]
***************************%

map(Rel, Input):-
    map1(Rel, Input).

map(Rel, Input, Output):-
    map2(Rel, Input, Output).

map(Rel, Input, Start, Finish):-
    map3(Rel, Input, Start, Finish).

map(Rel, Input, Output, Start, Finish):-
    map4(Rel, Input, Output, Start, Finish).


% Each kind of map is handled separately %

% Type check
map1(Rel, L):-
    var( L ),
    !,
    fail.
map1(Rel, []):-
    !.
map1(Rel, [Val| List]):-
    make_map_goal(Rel, Call, [Val]),
    Call,
    map1(Rel, List).

% List conversion
map2(Rel, [], []):-
    !.
map2(Rel, [El1| List1], [El2| List2]):-
    make_map_goal(Rel, Call, [El1, El2]),
    Call,
    map2(Rel, List1, List2).

% Accumulator map
map3(Rel, [], Value, Value):-
    !.
map3(Rel, [El| List], Acc, Value):-
    make_map_goal(Rel, Call, [El, Acc, Int]),
    Call,
    map3(Rel, List, Int, Value).

% Produce and accumulate
map4(Rel, [], [], Value, Value):-
    !.
map4(Rel, [El1| List1], [El2| List2], Acc, Value):-
    make_map_goal(Rel, Call, [El1, El2, Acc, Int]),
    Call,
    map4(Rel, List1, List2, Int, Value).

% Make each map goal
make_map_goal( Rel(Fixed), Call, Rest ):-
    Call =.. [Rel, Fixed| Rest],
    !.
make_map_goal( Rel, Call,  Args ):-
    Call =.. [Rel| Args].

