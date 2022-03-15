:-module(lexical,[
    get_tokens/3,
    names/3,
    chartype/2,
    data_flag/2,
    data_flag_char/2,
    createchartype/1,
    showDataFlags/0,
    test_lexical/2
    ]).

/**  <module> Lexical rules

     This file contains the code for the tokenization of input
     both for kleio commands (e.g. structure definitions)
      and for kleio source data files.

     It contains a  predicate 'get_tokens'/3 which is called
     by readlines/1 in the 'clio input top level' file, together with
     a grammar for tokenisation of input.
     
*/
:-use_module(utilities).
:-use_module(errors).

%% get_tokens(+F,+Chars,?Tokens) is det.
%
% Transforms a list of characteres into
% a list of tokens. Tokenisation varies according to F which can be
% either _dat_ if chars correspond to Kleio source data or _cmd_ if
% they correspond to a command.
%
% The 'Chars' List and 'F' are bound on call.
% The 'Chars' list is build from the input file by 'readlines2'
% Each element in 'Chars' is represented by a structure (Char, Type),
% where 'Char' is the Ascii code for the character and 
% 'Type' is a constant that gives the type of the char,
% (digit, letter, eof, etc...). 
% Char types are defined by 'chartype' predicates. 
%
get_tokens(F,Chars,Tokens):-
   phrase(toks(F,Tokens),Chars),!.  % call the grammar

%******************************************************
%  Grammar for the tokenisation of kleio files 
%    (commands and source data)
%******************************************************
%  toks is the top level
%    it is defined simply as a list of 'tok'
%******************************************************
%  %
toks(__F,[])-->[].
toks(F,[T|R])-->tok(F,T),toks(F,R).
toks(F,[TOK])-->tok(F,TOK).

%******************************************************
% tokens in data files 
%******************************************************
%  %
tok(dat,(tquote,V)) -->tquote(V),{!}.
tok(dat,(dquote,V)) -->dquote(V).
tok(dat,(names,V))-->names(V),!.
tok(dat,(fill,V))-->fillsp(V),!.
tok(dat,(dataflag,N))-->[(__C,T)],{data_flag(N,T),!}.
tok(dat,(number,N))-->num(N),!.
tok(dat,(T,C))-->other((T,C),[upper,lower,digit,space,tab]),
             {\+ data_flag(_,T),!}.
%******************************************************
% tokens in command files 
%******************************************************
%  %
tok(cmd,(fill,V))   -->fillsp(V).
tok(cmd,(name,V))   -->names(V).
tok(cmd,(number,V)) -->num(V).
tok(cmd,(string,V)) -->dqstring(V).
tok(cmd,(dataflag,2))-->[(__C,T)],{data_flag(2,T)}.
tok(cmd,(T)) -->other(T,[upper,lower,digit,space,tab]),
                       {\+ data_flag(2,T)}.

%******************************************************
%   alpha numeric strings (letters or digits). 
%******************************************************
% 
alpha(V)	           -->alphaList(L),{!,name(V,L)}.

alphaList([C|R])    -->alphaChar(C),alphaList(R).
alphaList([C])      -->alphaChar(C).

alphaChar(C)        -->[(C,upper)].
alphaChar(C)        -->[(C,lower)].
alphaChar(C)        -->[(C,digit)].

 %
%******************************************************
%   fill characters (spaces and tabs) 
%******************************************************
%  %
fillsp(V)             -->fill_List(L),{!,name(V,L)}.

fill_List([C|R])    -->fillChar(C),fill_List(R).
fill_List([C])      -->fillChar(C).

fillChar(C)         -->[(C,space)],!.
fillChar(C)         -->[(C,tab)],!.

%******************************************************
%   anychar except those on the list O %
%******************************************************
%  %
other((T,C),O)-->[(C,T)],{\+ member_check(T,O)}.

%******************************************************
%   keywords: we take any non ambiguous abreviation %
%******************************************************
% CURRENTLY NOT USED COMMENTED OUT
%kw(V)-->letters(L),{!,fkw(L,V)}.

letters(V)-->letterList(L),{!, upper_to_lower(L,V)}.
 %
%******************************************************
%  List of letters
%******************************************************
%/ CURRENTLY NOT USED COMMENTED OUT

letterList([C|R])-->letterChar(C),letterList(R).
letterList([C])-->letterChar(C).
%
letterChar(C)-->[(C,upper)].
letterChar(C)-->[(C,lower)].


%****************************************************** 
% names: sequences of letters, digits,
%    point, minus and underscore, beginning with a letter
%******************************************************
%%
names(V)           -->letterChar(C),nameList(L),{!,name(V,[C|L])}.

nameList([C|R])    -->nameChar(C),!,nameList(R).
nameList([C])      -->nameChar(C).
nameList([])       -->[].

nameChar(C)        -->[(C,T)],
                      {member(T,[upper,lower,digit,point,minus,underscore])}.


%*****************************************************************
%     numbers.
%*****************************************************************
%%

num(V)-->numberList(L),[(C,point)],numberList(R),
           {!, append(L,[C|R],N), atom_codes(V,N)}.
num(V)-->numberList(L),[(C,point)],
           {!,append(L,[C],N),atom_codes(V,N)}.
% TODO change bellow name for atom_codes in order to avoid loosing trailing zeros in numbers.
num(V)-->numberList(L),{!,atom_codes(V,L)}.

numberList([C|R])-->[(C,digit)],numberList(R).
numberList([C])  -->[(C,digit)].

numberChar(C) --> [(C,digit)] 
                | [(C,point)].

%******************************************************
%  quoted strings with double quotes
%******************************************************
%  %
% in data files we allow multiline quoted strings
%  so we just make the quote a token and dataSyntax will hand the rest
dquote(V) --> [(Q,doblequote)],{name(V,[Q]),!}.

% in command files we process quotes within a line. And provide the quoted string as a token

dqstring(V)-->[(_,doblequote)],!,qstr(doblequote,L),
             {!,qname(L,V)}.
qname(Chars,Quoted) :- name(S,Chars),
              re_replace("\""/g,"\\\"",S,T),
              atomic_list_concat(["\"",T,"\""],Quoted),!.

qstr(Q,[])   -->[(_,Q)],!.
qstr(Q,[C|R])  -->[(_,backslash),(C,_)],qstr(Q,R),!.
qstr(__Q,[])   -->[(_,return)],{!,error_out(' Closing quote not found')}.
qstr(Q,[C|R])-->qstrChar(Q,C),!,qstr(Q,R).
qstr(Q,[C])  -->qstrChar(Q,C),!.
qstrChar(Q,C)-->[(C,T)],{T \= Q}.

%******************************************************
%  tripe double quotes For multiline information
%******************************************************
%  %
tquote(V) --> [(Q,doblequote),(Q,doblequote),(Q,doblequote)],{name(V,[Q,Q,Q]),!}.

%% chartype(?Code,?Type) is det.
%
%  Types of characters
%  
chartype(32,space):-!.
chartype(9,tab):-!.
chartype(X,lower):- char_type(X,lower),!.   % lower case letters %
chartype(X,upper):- char_type(X,upper),!. % upper case letters %
chartype(X,digit):- char_type(X,digit), !. % digits %
chartype(X,return):-char_type(X,end_of_line),!. 
chartype(33,exclamation):-!.
chartype(34,doblequote):-!.
chartype(35,cardinal):-!.
chartype(36,dollar):-!.
chartype(37,percent):-!.
chartype(38,and):-!.
chartype(39,singlequote):-!.
chartype(40,openround):-!.
chartype(41,closeround):-!.
chartype(42,asterix):-!.
chartype(43,plus):-!.
chartype(44,comma):-!.
chartype(45,minus):-!.
chartype(46,point):-!.
chartype(47,slash):-!.
chartype(58,colon):-!.
chartype(59,semicolon):-!.
chartype(60,less):-!.
chartype(61,equal):-!.
chartype(62,greater):-!.
chartype(63,question):-!.
chartype(64,at):-!.
chartype(91,squareopen):-!.
chartype(92,backslash):-!.
chartype(93,squareclose):-!.
chartype(94,circunflex):-!.
chartype(95,underscore):-!.
chartype(_,unknown):-!.

%% createchartype(+Charlist) is det.
%
%  Outputs a list of chartype/2 to current output
%
% Usage: chartype(`!@#$%^&`). 
%
% ==
%?- lexical:createchartype(`!"#$%&7()`).
% chartype(33,'!'):-!.
% chartype(34,'"'):-!.
% chartype(35,'#'):-!.
% chartype(36,'$'):-!.
% chartype(37,'%'):-!.
% chartype(38,'&'):-!.
% chartype(55,'7'):-!.
% chartype(40,'('):-!.
% chartype(41,')'):-!.
% ==

createchartype([A|B]):-
  cct(A),
  createchartype(B).
createchartype([]):-!.
cct(X):-
   write('chartype('),
   write(X),
   write(','),
   put(39),put(X),put(39),
   write('):-!.'),nl.



%% data_flag(N,Name) is det.
% 
% Defines data fags: special characters used in kleio data files
%
data_flag(1,dollar).
data_flag(2,slash).
data_flag(3,equal).
data_flag(4,cardinal).
data_flag(5,percent).
data_flag(6,less).
data_flag(7,greater).
data_flag(8,semicolon).
data_flag(9,colon).
data_flag(10,backslash).

data_flag_char(N,C):-data_flag(N,T),
                     chartype(C,T).

data_flag_list(L):-setof((N,X),(data_flag_char(N,X)),L).

%% showDataFlags is det.
%  Show current values for dataflags
%
showDataFlags:-data_flag(N,F),chartype(C,F),
               name(S,[C]),
               write('dataflag '),
               write(N),tab(1),
               write(S),write(' ('),write(F),writeln(')'),
               fail,!.
showDataFlags.

%% test_lexical(+Chars,-Types) is det.
% Classify the chars from back quoted string or codes list.
% 
% To debug the full list of result do
%   set_prolog_flag(answer_write_options,[max_depth(0)]). 
% 
test_lexical(Chars,Types):-
  bagof((C,T),(member(C,Chars),chartype(C,T)),Types).


%% test_get_tokens(+Type,+Chars,-Tokens) is det.
% Tokening the chars from back quoted string.
% 
% To debug the full list of result do
%   set_prolog_flag(answer_write_options,[max_depth(0)]). 
%
test_get_tokens(Type,Chars,Tokens):-
  test_lexical(Chars,TypedChars),
  get_tokens(Type,TypedChars,Tokens).

breakpoint:- true. %place holder for break points

:-begin_tests(lexical).



test(get_tokens_dat_quotes):-
    Chars = `acto$asf.4#"htpp://timelink.uc.pt?"/24/5/1958/obs=url\r`,
    string_codes(String,Chars),
    format('Line  ~w~n:',[String]),
    test_lexical(Chars,TypedChars),
    writeln(TypedChars),
    get_tokens(dat,TypedChars,Tokens),
    writeln(Tokens).

test(get_tokens_dat_quotes_with_quotes):-
    Chars = `acto$asf.4#"htpp://timelink.uc.pt?\\\"xpto\\\""/24/5/1958/obs=url\r`,
    string_codes(String,Chars),
    format('Line  ~w~n:',[String]),
    test_lexical(Chars,TypedChars),
    writeln(TypedChars),
    get_tokens(dat,TypedChars,Tokens),
    writeln(Tokens).

test(get_tokens_dat_quotes_dangling):-
    Chars = `acto$asf.4/obs="url\r`,
    string_codes(String,Chars),
    format('Line  ~w~n:',[String]),
    test_lexical(Chars,TypedChars),
    writeln(TypedChars),
    get_tokens(dat,TypedChars,Tokens),
    writeln(Tokens).

  test(triple_quote):-
    Chars = `""" \r one line \rtwo lines\r\r"""`,
    string_codes(String,Chars),
    format('Line  ~w~n:',[String]),
    test_lexical(Chars,TypedChars),
    writeln(TypedChars),
    get_tokens(dat,TypedChars,Tokens),
    writeln(Tokens),
    member_check((tquote,_),Tokens).
  
:- end_tests(lexical).


    






% Pre-git history
% $Id$
% $Date$
% $Author$
% $Log: lexical.pl,v $
% Revision 1.2  2006/05/16 10:52:50  Joaquim
% update dos ficheiros prolog
%
% Revision 1.1  2004/04/08 14:45:24  ltiago
% Source code has passed from Joaquim to Tiago.
% Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
% File build.xml, web.xml and velocity.properties were changed
%
% Revision 1.1.1.1  2001/04/19 23:25:43  jrc
% New Repository.
%
% Revision 1.1.1.1  2001/04/18 23:34:39  jrc
% CVS repository moved from llinux to MacOSX.
%
% Revision 1.3  2001/01/11 13:18:35  jrc
% Removed a bug introduced in a previous patch where <CR> (ascii 13)
% were removed from the input stream, making Mac originated files
% unusable. The file basicio.pl was also involved in this bug.
%
% Now both CR and NL have "return" chartype. basicio.pl handles
% the CR+NL sequence as a single end of line.
%
/*

(Even older) History
Dataflags added 27 AUG 90
Keyword detection in command files was 'moved up' to
  stru syntax. 21 SEPT 90
string to number conversion added to num(V) in  2 OCT 90 
cuts, added in several rules to improve efficiency 14 OCT 90.
order of chartype predicates changed to speed resolution FEB 91
chartype(10,return) added FEB 91 for compatibility with BIMprolog on UNIX

*/