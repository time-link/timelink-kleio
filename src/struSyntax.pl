
:-module(struSyntax,[

    compile_command/2,
    engkw/2

    ]).

/**<module> Syntax of Kleio stucture files


  compile_command(Cmd,Tokens)
    Takes a token list and analyses the syntactic structure
    Execution of the command takes place as the command is
    analysed, with initialization and finalization code
    being called here. Execution of the command, the acutal
    processing of the strucutre definition is triggered by
    the syntactic rules as they decode the token list.

    The grammar is mostly data driven: we don't expand
    each command variant. The grammar reflects
    the general form of a command and relies on data predicates
    to check for different command variants.

    compile_command/1 is called by processLine/2 in topLevel.pl and calls predicates
    in struCode.pl and dataDictionnary.pl .

    History

    *   stable OCT 90.
    *   exitus command set to "ok" in 14-NOV-1990
    *   signum added to the keywords in 14-Nov-1990
    *   English keywords treatment added in 9-Sept-2000

*************************************************************
*/
:-use_module(lexical).
:-use_module(dataSyntax).
:-use_module(struCode).
:-use_module(errors).
:-use_module(persistence).
:-use_module(utilities).

:-dynamic(gdoc/2).
:-dynamic(edoc/2).
:-dynamic(edoc/3).

compile_command(CMD,Tokens):-
    stripSpaces(Tokens,CleanToks),
   % writeln(CleanToks),
    phrase(cliocmd,[CMD|CleanToks]),!.
compile_command(CMD,_):-
  get_value(stru_file,S),
  get_prop(line,number,L),
  get_prop(line,text,Line),
  get_prop(line,last,Last),
  error_out(['*** Unable to compile ',CMD,' command.!'],
    [file(S),line_number(L),line_text(Line),last_line_text(Last)]).

%***********************************************
% stripSpaces(Tokens,CleanToks)
% removes fill and return tokens from token list
%************************************************
% %
stripSpaces([],[]):-!.
stripSpaces([(fill,_)|Tokens],CleanToks):-
  stripSpaces(Tokens,CleanToks),!.
stripSpaces([(return,_)|Tokens],CleanToks):-
  stripSpaces(Tokens,CleanToks),!.
stripSpaces([Tok|Tokens],[Tok|CleanToks]):-
  stripSpaces(Tokens,CleanToks),!.

%*************************************************************************%
cliocmd -->nota,inlineDoc,!.
cliocmd -->nota,skipRest,!.

cliocmd -->goodCommand(C ),parlist(C), % finalization code called here %
                  {close_command(C,_)},!.
cliocmd -->notYet(C),skipRest,
                  {error_out(['*** Command not implemented:',C])},!.
cliocmd -->badCommand(C),skipRest,
                  {error_out(['*** Unknow command. Check spelling.',C])},!.


nota-->[C],{is_kw(C,nota)},!.
                      % initialization code called here %
goodCommand(K)-->[C],{(is_kw(C,K),command(K,ok),init_command(K))},!.

badCommand(K)-->[C],{(is_kw(C,K),\+ command(K,_))}.

notYet(K)-->[C],{(is_kw(C,K),command(K,notYet))}.



parlist(C)     -->param(C,P,V),
                  semicolon,{execParam(C,P,V)},parlist(C).
parlist(C)     -->param(C,P,V),{execParam(C,P,V)},!.
parlist(C)     -->badparam(C),
                  semicolon,parlist(C).
parlist(C)     -->badparam(C).
parlist(_)     -->[].

param(C,P,V)  --> par(C,P),equal,val(C,P,V).

par(C,K)-->[(name,P)],{(is_kw(P,K),params(C,L),member_check(K,L))}.
par(_,K)-->[(name,P)],{(\+ is_kw(P,K),
                        error_out(['** bap param ',P]),fail)}.
%******************************************************
%  bad parameters
%    1 - bad parameter name
%    2 - bad value
%******************************************************
%  %
badparam(C)-->badpar(C,P),equal,val(C,_,_),%try to skip a value anyway%
             {error_out(['***Bad parameter ',P,' for command ',C])},!.
badparam(C)  --> par(C,P),equal,val(C,P2,V),
                     {(P \= P2,
                      error_out(['***Bad value ',V,
                    ' for ',P,' for command ',C]))},!.

badpar(C,P)-->[(_,P)],{(params(C,L),\+ member_check(P,L))}.


params(nota,_).
params(nomino,
      [nomen,primum,modus,antiquum,scribe,plures,identificatio]).
params(terminus,
      [nomen,modus,primum,secundum,ordo,fons,prae,post,
       pars,sine,signa,forma,ceteri,identificatio,cumule,
       solum]).
params(pars,
     [nomen,ordo,sequentian,identificatio,signum,fons,
      prae,post,locus, ceteri,certe,pars,solum,semper,repetitio]).
params(exitus,[nomen]).


val(nomino,nomen,V)-->[(name,V)].
val(nomino,primum,V)-->[(name,V)].
val(nomino,modus,V)-->perAdHoc(V).
val(nomino,antiquum,V)-->sicNon(V).
val(nomino,scribe,V)-->scribType(V).
val(nomino,plures,V)-->sicNon(V).
val(nomino,identificatio,V)-->sicNon(V).

val(pars,nomen,V)-->list(V).
val(pars,ordo,V)-->sicNon(V).
val(pars,sequentia,V)-->sicNon(V).
val(pars,identificatio,V)-->sicNon(V).
val(pars,signum,V)-->[(name,V)].
val(pars,fons,V)-->[(name,V)].
val(pars,prae,V)-->sicNon(V).
val(pars,post,V)-->sicNon(V).
val(pars,locus,V)-->list(V).
val(pars,ceteri,V)-->list(V).
val(pars,certe,V)-->list(V).
val(pars,pars,V)-->list(V).
val(pars,solum,V)-->list2(V).
val(pars,semper,V)-->list2(V).
val(pars,repetitio,V)-->list(V).

val(terminus,nomen,V)       -->list(V).
val(terminus,modus,V)       -->clioType(V).
val(terminus,primum,V)      -->clioType(V).
val(terminus,secundum,V)    -->clioType(V).
val(terminus,ordo,simplex)  -->[(_,simplex)].
val(terminus,ordo,multiplex)-->[(_,multiplex)].
val(terminus,fons,V)        -->[(name,V)].
val(terminus,prae,V)        -->sicNon(V).
val(terminus,post,V)        -->sicNon(V).
val(terminus,pars,V)        -->[(name,V)].
val(terminus,sine,V)        -->sicNon(V).
val(terminus,signa,V)       -->sicNon(V).
val(terminus,forma,V)       -->sicNon(V).
val(terminus,ceteri,V)      -->sicNon(V).
val(terminus,identificatio,V)-->sicNon(V).
val(terminus,cumule,V)       -->sicNon(V).
val(terminus,solum,V)        -->sicNon(V).

val(exitus,nomen,V)          -->[(name,V)].


%skipRest-->[(_,_)].
skipRest-->[(_,_)],skipRest.
skipRest-->[].

%
%
% TODO trying to implement a javadoc type documentation. Imcomplete
%
%
inlineDoc-->groupDoc(gdoc(G,DOC)),{storeGroupDoc(gdoc(G,DOC)),!}.
inlineDoc-->elementDoc(E),{storeElementDoc(E),!}.
inlineDoc-->elementGroupDoc(E),{storeGroupElementDoc(E),!}.
%inlineDoc-->{!}.
groupDoc(gdoc(G,DOC)) -->docKeyword,groupDocKeyword,groupDocName(G),getDocText(DOC),!.
elementDoc(edoc(E,DOC)) -->docKeyword,elementDocKeyword,elementDocName(E),getDocText(DOC),!.
elementGroupDoc(edoc(E,DOC)) -->docKeyword,elementInGroupDocKeyword,elementDocName(E),getDocText(DOC),!.

docKeyword            -->[(name,doc)].
groupDocKeyword       -->[(name,group)].
groupDocName(G)       -->[(name,G)].
elementDocKeyword       -->[(name,element)].
elementInGroupDocKeyword -->[(name,'element-in-group')].
elementDocName(G)       -->[(name,G)].

getDocText([N,' '|R]) -->[(name,N)],getDocText(R),!.
getDocText([N,' '|R]) -->[(number,N)],getDocText(R),!.
getDocText(['"',N,'"',' '|R]) -->[(string,N)],getDocText(R),!.
getDocText([N|R])     -->[(dataflag,Nb)],getDocText(R),{lexical:data_flag_char(Nb,C),name(N,[C])},!.
getDocText([N|R])     -->[(_,C)],getDocText(R),{name(N,[C])},!.
getDocText([])        -->[],!.
%
%
storeGroupDoc(gdoc(G,DOC)):- assert(gdoc(G,DOC)),put_value(lastGroupDoc,G).
storeGroupElementDoc(edoc(E,DOC)):- (get_value(lastGroupDoc,G);G='*'),assert(edoc(G,E,DOC)).
storeElementDoc(edoc(E,DOC)):- assert(edoc(E,DOC)).



% checking for legal values of params

perAdHoc(permanens)-->[(name,N)], {is_kw(N,permanens)}.
perAdHoc(adHoc)-->[(name,N)], {is_kw(N,adHoc)}.

% ad hoc needs ad hoc treatement!
perAdHoc(adHoc)-->[(name,ad)],[(name,hoc)].

sicNon(sic)-->[(name,N)], {is_kw(N,sic)}.
sicNon(non)-->[(name,N)], {is_kw(N,non)}.

scribType(generanda)-->[(name,G)],{is_kw(G,generanda)}.
scribType(nomina)-->[(name,N)],{is_kw(N,nomina)}.
scribType(structura)-->[(name,S)],{is_kw(S,structura)}.
scribType(partes)-->[(name,P)],{is_kw(P,partes)}.

list([First|Rest])-->[(_,First)],[(comma,_)],list(Rest).
list([Name])-->[(_,Name)].

list2([First|Rest])-->nameSlash(First),[(comma,_)],list(Rest).
list2([Name])-->nameSlash(Name).

nameSlash(Name)-->[(_,Name)].
nameSlash(Name/N)-->[(_,Name)],a_slash,a_number(N).


a_slash -->[(dataflag,2)].
a_number(N)-->[(number,N)].

clioType( lingua  )--> [(_,V)],{is_kw(V, lingua)}.
clioType( tempora )--> [(_,V)],{is_kw(V, tempora)}.
clioType( numerus )--> [(_,V)],{is_kw(V, numerus)}.
clioType( condicio)--> [(_,V)],{is_kw(V, condicio)}.
clioType( situs   )--> [(_,V)],{is_kw(V, situs)}.
cliotype( relatio )--> [(_,V)],{is_kw(V, relatio)}.

equal-->[(equal,_)],{!}.
equal-->[(Op,_)],{Op \= equal,error_out('*** equal sign expected.'),fail}.

semicolon-->[(semicolon,_)],!.

%command(nota,ok).%
command(nomino,ok).
command(terminus,ok).
command(pars,ok).
command(exitus,ok).
command(confirmatio,notYet).
command(continuatio,notYet).
command(finis,notYet).
command(inspectio,notYet).
command(lege,notYet).
command(negatio,notYet).
command(optiones,notYet).
command(quaero,notYet).
command(repertorium,notYet).
command(scribe,notYet).

%******************************************************
%  is_kw find a keyword;
%    takes C and checks if it is an keyword K %
%    the behaviour of this predicate was changed
%    to not required a unambiguous keyword prefix
%    as before, but instead to backtrack through all
%    valid keywords for C. This was necessary to
%    process english keywords that can have various
%    latin equivalents. The main parser will (hopefully)
%    deal with the right form.
%******************************************************
%  %

is_kw(C,K):-
    a_upper_to_lower(C,L),
    fkw(L,K)
%    , writelist(['is_kw returned ',K,' for ',C]),nl
    .

%********************************************%
fkw(T,K):-
    name(T,L),
    setof(X,matchkw(L,X),Ks),
    % writelist(['fkw for ',T,' returned ' | Ks]),nl,
    member(K,Ks).

matchkw(T,K):-
    engkw(EKW,K),
    name(EKW,LEKW),
    begins_with(T,LEKW).

matchkw(T,K):-
	keyword(clio,K),
	name(K,EK),
	begins_with(T,EK).

%******************************************************
%  Keywords
%******************************************************
%  %
keyword(clio, antiquum ).
keyword(clio, certe ).
keyword(clio, ceteri ).
keyword(clio, confirmatio).
keyword(clio, continuatio).
keyword(clio, cumulatio ).
keyword(clio, exitus).
keyword(clio, finis ).
keyword(clio, fons ).
keyword(clio, identificatio ).
keyword(clio, inspectio ).
keyword(clio, lege ).
keyword(clio, locus ).
keyword(clio, modus ).
keyword(clio, negatio ).
keyword(clio, nomen ).
keyword(clio, nomino).
keyword(clio, nota ).
keyword(clio, optiones ).
keyword(clio, ordo ).
keyword(clio, pars).
keyword(clio, plures ).
keyword(clio, post ).
keyword(clio, prae ).
keyword(clio, primum ).
keyword(clio, quaero ).
keyword(clio, repertorium ).
keyword(clio, repetitio ).
keyword(clio, scribe ).
keyword(clio, semper ).
keyword(clio, sine ).
keyword(clio, signum).
keyword(clio, solum ).
keyword(clio, terminus).
keyword(clio, sic).
keyword(clio, non).

% english equivalents to the latin keywords.

engkw(current,      antiquum).
engkw(overwrite,    antiquum).
engkw(guaranteed,   certe).
engkw(precise,      certe).
engkw(also,         ceteri).
engkw(confirm,      confirmatio).
engkw(confirmation, confirmatio).
engkw(continue,     continuatio).
engkw(continuation, continuatio).
engkw(cumulate,     cumulatio).
engkw(exit,         exitus).
engkw(stop,         finis).
engkw(source,       fons).
engkw(identification,identificatio).
engkw(read,         lege).
engkw(position,     locus).
engkw(type,         modus).
engkw(negate,       negatio).
engkw(name,         nomen).
engkw(database,     nomino).
engkw(note,         nota).
engkw(options,      optiones).
engkw(order,        ordo).
engkw(part,         pars).
engkw(more,         plures).
engkw(after,        post).
engkw(suffix,       post).
engkw(before,       prae).
engkw(prefix,       prae).
engkw(preparation,  prae).
engkw(first,        primum).
engkw(query,        quaero).
engkw(catalogue,    repertorium).
engkw(arbitrary,    repetitio).
engkw(repeat,       repetitio).
engkw(write,        scribe).
engkw(always,       semper).
engkw(without,      sine).
engkw(sign,         signum).
engkw(only,         solum).
engkw(element,      terminus).
engkw(yes,          sic).
engkw(no,           non).
engkw(permanent,    permanens).
engkw(temporary,    adHoc).
engkw(generic,      generanda).
engkw(terms,        nomina).
engkw(names,        nomina).
engkw(structure,    structura).
engkw(parts,        partes).
engkw(text,         lingua).
engkw(date,         tempora).
engkw(number,       numerus).
engkw(count,        numerus).
engkw(numbers,      numerus).
engkw(category,     conditio).
engkw(location,     situs).
engkw(relation,     relatio).

