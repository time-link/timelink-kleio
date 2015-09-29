% vim: filetype=prolog
% $Id$
% $Date$
% $Author$
% $Log: basicio.pl,v $
% Revision 1.2  2006/05/16 10:51:13  joaquim
% update dos ficheiros prolog
%
% Revision 1.1  2004/04/08 14:45:23  ltiago
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
% Revision 1.3  2001/01/11 13:20:23  jrc
% Modified to process correctly end of line in unix, dos, and mac.
% See also lexical.pl for more information on end of lines.
% 
%/*********************************************
% readline2(LineChars,LastChar) read line from input
%           LineChars  - input line (as a char list)
%  each element of the char list is a (C,T) struct 
%  where C is the ASCII code of the char and T the
%  type of the char as defined by the chartypes predicates
%           LastChar - Last char on input line (CR or EOF).
%  Notes:
%      End of line is detected through chartype predicates.
%      and a (13,return) is included in the LineChars.
%      Line and LastChar are variables on Call
%      if a line is terminated by end of file "eof" is returned
%      as LastChar, but not included in LineChars.
%      
% Modified to swi-prolog in 6 de Novembro de 1999
% Modified to skip characteres of type "skip" Sept 2000
% check the CVS log keyword above for subsequent changes
%/*********************************************
%    %
readline2(LineChars,Last):-
     getchar2(Char),
     readl2(Char,LineChars,Last),!.

readline2(_,_):-report([writeln('Readline2 unexpected fail')]),!.

readl2(eof,[],eof):-!.
readl2((R,return),[(R,return)],(R,return)):-!.
readl2((Char,Type),[(Char,Type)|Rest],Next):-
     getchar2(C),
     readl2(C,Rest,Next).

%/*********************************************
% getchar(Byte):-  Byte must be unbound on call
%                  put in Byte the ASCII code for the next char
%                  in the default output stream, or 'eof'.
%                  
%/*********************************************
%%
getchar(Byte):-get_code(Byte), Byte > -1,!.
getchar(eof):-!.

%/*********************************************
% getchar2(Char):-  Char must be unbound on call.
%                 Char will be bound to a (C,T) struct 
%                 where C is the ASCII code of the char and T the
%                 type of the char as defined by the chartypes 
%                  predicates.
%                 if eof was detected C is bound to'eof'
%/*********************************************
%%


getchar2((Byte,Type)):-
		  get_code(Byte),% changed from get_byte(Byte) 22-9-2015
		  Byte > -1,
		  chartype(Byte,Type),
		  (Byte = 13 -> skip_nl ; true), !. % skip_nl added 22-9-2015
getchar2(eof):-!.

skip_nl :- peek_code(10),get_code(_).
skip_nl.

%/*********************************************
% putchar(Byte):- writes char to output stream or EOF
%                 if Byte = 'eof'
%/*********************************************
%%

putchar(eof):-nl,write('EOF'),nl,!.
putchar(X):-put_code(X),!.


%/*********************************************
% putchar2(Char):- If Char is a strucutre (C,T)
%                writes C to output stream
%                  If Char is 'eof' writes EOF
%/*********************************************
%%
        
putchar2(eof):-nl,write('EOF'),nl,!.
putchar2((X,_)):-put_code(X),!.

%/*********************************************
% get_codes(CharList,CodeLisT)
%           CharList: bound on call
%            each element of the char list is a (C,T) struct 
%            where C is the ASCII code of the char and T the
%            type of the char as defined by the chartypes predicates
%            If 'eof' appears in the CharList it is ignored.
%           CodeList: unbound on call.
%             is bound to the list of ASCII codes in the CharList
%/*********************************************
%%
get_codes([],[]):-!.
get_codes([(C,_)|RestChars],[C|RestCodes]):-
     get_codes(RestChars,RestCodes),!.
get_codes([eof|RestChars],[RestCodes]):-
     get_codes(RestChars,RestCodes),!.
%/*********************************************
% get_string(CharList,String)
%           CharList: bound on call
%            each element of the char list is a (C,T) struct 
%            where C is the ASCII code of the char and T the
%            type of the char as defined by the chartypes predicates
%           String: unbound on call.
%             is bound to a string made of the list of ASCII codes
%             in the CharList
%/*********************************************
%%
get_string(CharList,String):-
     get_codes(CharList,CodeList),
     name(String,CodeList),!.


















