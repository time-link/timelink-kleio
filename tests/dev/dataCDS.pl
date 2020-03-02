:-module(dataCDS,[
    getCDS/1,
    setCDS/1,
    createCD/0,
    delCD/0,
    setCDField/2,
    getCDField/2,
    get_aspects/3,
    get_aspect/3,
    getCDElement_list/1,
    getCDAnc/2,
    makeID/1,
    printEntries/2
    ]).

/** <module> Current Data Storage API
 
This module provides access to Current Data Storage.

During input, groups and processing info are stored as properties of the atom _cds_ .

Access to this data structure is included Api in externals.pl

For efficiency? the current values of the various properties are
manipulated through a term, acessible through getCDS/1 and setCDS/1 and getCDField/2 and setCDField/2.


==    
    cds(CPATH,CGROUP,CGROUPID,
         LOCUSCOUNT,ELEMENTLIST,CELEMENT,
        ENTRYLIST,CASPECT,CCORE,CORIGINAL,
          CCOMMENT).
==   

   The list of the elements of the cds structure is the following:

==    
               cpath,  % current ancestors of Cgroup%
               cgroup,       % current group %
               cgroupID,     % kleio id of current group%
               locusCount,   % certe count %
               elementList,  % element list %
               celement,     % current element %
               entryList,    % current entries %
               caspect,       % current aspect being input%
               ccore,        % current core %
               coriginal,    % current original aspect %
               ccomment]).   % current comment aspect %

 cpath=[DOC(DocID),Group1(GID1)...]
 elementList=[element(name,entryList),...]
 entryList=[entry(core,original,comment),...]
    
### Predicates that deal with the current data storage
    
* cdsFields(L) - L is the list of legal field names
* getCDS(C)   - gets the currents cds
* setCDS(C)   - sets the current cds
* createCD    - creates an empty data storage structure
* delCD       - erases current data storage structure
* cleanCD     - puts every field of cds to a default value
* setCDField(Field,Value)
              - set Field in the CDS structure
* getCDField(Field,Value) 
              - returns value for Field in CDS
* get_aspect(Asp,Element,Info) return aspect info
* get_aspects(Asp,ElementList,InfoList) 
                   same for a list of elements
* getCDElement_list(List) list of elements of current group
* makeID(ID)   - constructs ID for current group
      changed Ago 97 to allow for ids up to 42 chars instead of 12
* getCDAnc(A,ID) - get name and ID of ancestor.
* showCDS      - prints current values for CDS 

*/

:-use_module(dataCode). 
:-use_module(dataDictionary).
:-use_module(errors).
:-use_module(persistence).
:-use_module(utilities).

%******************************************************
%  %
%*****************************************************
%  cdsFields(L) lists legal fields of CDS
%******************************************************
%%
cdsFields([cpath, cgroup,cgroupID,locusCount,elementList,celement,entryList,caspect,
ccore, coriginal, ccomment]).


%*****************************************************
%  getCDS(C)
%******************************************************
%%
getCDS(cds(CPATH,CGROUP,CGROUPID,LOCUSCOUNT,ELEMENTLIST,CELEMENT,
                ENTRYLIST,CASPECT,CCORE,CORIGINAL,CCOMMENT)):-
    get_prop(cds,cpath,CPATH),
    get_prop(cds,cgroup,CGROUP),
    get_prop(cds,cgroupID,CGROUPID),
    get_prop(cds,locusCount,LOCUSCOUNT),
    get_prop(cds,elementList,ELEMENTLIST),
    get_prop(cds,celement,CELEMENT),
    get_prop(cds,entrylist,ENTRYLIST),
    get_prop(cds,caspect,CASPECT),
    get_prop(cds,ccore,CCORE),
    get_prop(cds,coriginal,CORIGINAL),
    get_prop(cds,ccomment,CCOMMENT),!.


%*****************************************************
%  setCDS(C)
%******************************************************
%%
setCDS(cds(CPATH,CGROUP,CGROUPID,LOCUSCOUNT,ELEMENTLIST,CELEMENT,
                ENTRYLIST,CASPECT,CCORE,CORIGINAL,CCOMMENT)):-
    set_prop(cds,cpath,CPATH),
    set_prop(cds,cgroup,CGROUP),
    set_prop(cds,cgroupID,CGROUPID),
    set_prop(cds,locusCount,LOCUSCOUNT),
    set_prop(cds,elementList,ELEMENTLIST),
    set_prop(cds,celement,CELEMENT),
    set_prop(cds,entryList,ENTRYLIST),
    set_prop(cds,caspect,CASPECT),
    set_prop(cds,ccore,CCORE),
    set_prop(cds,coriginal,CORIGINAL),
    set_prop(cds,ccomment,CCOMMENT),!.

%******************************************************
%  createCD - creates an empty data storage structure
%******************************************************
%  %
createCD:-
    setCDS(cds(__CPATH,__CGROUP,__CGROUPID,__LOCUSCOUNT,__ELEMENTLIST,__CELEMENT,
                _ENTRYLIST,__CASPECT,__CCORE,__CORIGINAL,__CCOMMENT)),
    cleanCD,!.
%******************************************************
%  delCD  erases current data storage structure
%******************************************************
%  %
delCD:-del_props(cds),!.
    
%******************************************************
%  cleanCD put every field of cds to a default value
%    change if template is changed
%******************************************************
%  %
cleanCD:-
    setCDS(cds([],[],[],0,[],[],[],core,[],[],[])),!.
%******************************************************
%  setCDField(Field,Value)- set Field in the CDS structure
%        to Value
%******************************************************
%  %

setCDField(Field,Value):-
    cdsFields(Fs),member_check(Field,Fs),!,
    set_prop(cds,Field,Value).

setCDField(Field,__Value):-
    error_out(['** SetCDField: ilegal field:',Field, '(internal error)']),!.

%******************************************************
%  getCDField(Field,Value) returns value for Field in CDS
%******************************************************
%  %
getCDField(Field,Value):-
    cdsFields(Fs),member_check(Field,Fs),!,
    get_prop(cds,Field,Value).

getCDField(Field,__Value):-
    error_out(['** getCDField: ilegal field:',Field, '(internal error)']),!.
%******************************************************
%  getCDElement_list(List) list of elements of current group
%******************************************************
%  %
getCDElement_list(List):-
    getCDField(elementList,Els),
    bagof(E,a_element(E,Els),List),
    !.
getCDElement_list([]):-!.

a_element(E,Els):-
        member(element(E,_),Els).


%******************************************************
%  get_aspects(Asp,ElementNameList,CoreList)
%    return core information for a list of elements
%******************************************************
%  %
get_aspects(__A,[],[]):-!.
get_aspects(A,Elements,CoreList):-
    getCDField(elementList,Els),!, % get list of elements %
    getCDFs(A,Elements,Els,CoreList),!.

getCDFs(A,[E|Els],Elements,[C|Cores]):-
    getCDF(A,E,Elements,C),
    getCDFs(A,Els,Elements,Cores),!.
getCDFs(_,[],_,[]):-!.
%******************************************************
%  get_aspect(Asp,Element,Core) return aspect info
%    for Element.Aspect can be 'core', 'original', 'comment'
%     Returns [] if inexistant.
%    When there are multiple entries for the information
%    a structure mult(L) is returned where L is the list of
%    entries.
%******************************************************
%  %
get_aspect(A, E,Info):-
    getCDField(elementList,Els),% get list of elements %
    member(element(E,L),Els),   % get element entries  %
    gaspect(A,L,Info).  
          
get_aspect(__A,E,[]):-
    getCDField(elementList,Els),
    \+ member(element(E,__L),Els). % no element %

% more eficient but not backtrackable version %
get_aspect0(A, E,Info):-
    \+ var(A), \+ var(E), 
    getCDField(elementList,Els),!, % get list of elements %
    getCDF(A,E,Els,Info).

getCDF(A,E,Els,Info):-
    member_check(element(E,L),Els), !,  % get element entries  %
    gaspect(A,L,Info).  
getCDF(__A,E,Els,[]):-
    getCDField(elementList,Els),
    \+ member_check(element(E,__L),Els). % no element %

gaspect(A,L,Info):-
    g_asp(A,L,I),
    (I = [Info]; Info = mult(I)),!. % flag multiple entries with mult%

g_asp(__A,[],[]).
g_asp(core,[entry(Core,_,_)|MoreEntries],[Core|MoreCore]):-
    g_asp(core,MoreEntries,MoreCore).
g_asp(original,[entry(_,Org,_)|MoreEntries],[Org|MoreOrg]):-
    g_asp(original,MoreEntries,MoreOrg).
g_asp(comment,[entry(_,_,Comm)|MoreEntries],[Comm|MoreComm]):-
    g_asp(comment,MoreEntries,MoreComm).

% currently not used. make it an utility latter %
mk_string([A],A):-!. % single element lists-> element %
mk_string([A|B],S):- % small lists -> string %
    length([A|B],N),
    N  @< 12,
    list_to_a([A|B],S),!. 
mk_string([A|B],[A|B]):-  % big lists -> keep them %
    length([A|B],N),
    N @>=12.
mk_string(C,C):-
    atomic(C).

%******************************************************
%  getCDElement(Element,Value) return element info
%    for Element. Returns [] if inexistant
%******************************************************
%  %
getCDElement(E,V):-
    getCDField(elementList,Els),% get list of elements %
    member(element(E,V),Els).   % get element entries  %

getCDElement(E,[]):-
    getCDField(elementList,Els),
    \+ member(element(E,_),Els). % no element %
%******************************************************
%  getCDAnc(A,ID) -get name and ID of ancestor
%    of current group
%******************************************************
%  %

getCDAnc(A,ID):-        % get ancestor (last element in path) %
    getCDField(cpath,P),
    last(P,Anc),
    Anc =..[A,ID],!.
getCDAnc([],[]):-       % path empty: G is a doc, no ancestor %
    getCDField(cpath,[]).

%******************************************************
%  makeID(ID) - constructs ID for current group
% see kleio manual p.57
% changed so that ids could have up to 42 chars
%******************************************************
%  %
makeID(ID):-
    getCDField(cgroup,G),
    clioGroup(G,I),
    mkid(G,I,S),
    afirst_n(S,42,ID),
    setCDField(cgroupID,ID),!.

mkid(G,I,ID):-
    isDoc(G),
    mkid_doc(G,I,ID),!.

mkid(G,I,ID):-
    \+ isDoc(G),
    mkid_group(G,I,ID),!.

mkid_doc(G,I,ID):- % see identificatio of doc. Kleio man. p.65%
    mkid_group(G,I,ID), % MUDAR DEPOIS %
    !.

mkid_group(G,I,ID):- % see if there is an element id %
    mkid_el_id(G,I,ID);
    mkid_count(G,I,ID),% if not use the counter %
    !.
mkid_el_id(G,__I,ID):-  % check if an element is the ID %
    getCDField(elementList,E),!,% get elements of current group%
    member(element(El,__Ent),E),
    clioElement(El,EID),% see if it has identificatio=sic %
    get_prop(EID,identificatio,sic),
    mkid_entry1(G,El,ID),!.   % get first aspect of first entry %

% mudar: isto devia usar o GetCDELEMENT_ASPECT %
mkid_entry1(__G,E,ID):-
    get_aspect(core,E,Core),
    mult(__A) \= Core, % multiple entries not allowed in id fields %
    list_to_a0(Core,ID),!. % tirei a variante [ID]=Core %
mkid_entry1(G,E,[]):-
    error_out(['** bad value in identification element: ',
                (G,E)]),
    fail,!.

% note: every group receives a default signum from its name.
%    this is done by set_pars_defaults in the group utilities file %

mkid_count(G,I,ID):-        % get signum par %
    get_prop(I,signum,S),
    inc_group_count(G,I,N),   % inc counter for this group%
    concat(S,'-',S1),        % make id %
    concat(S1,N,ID),
    !.
    
%******************************************************
%  showCDS - prints current values for CDS 
%    structure
%******************************************************
%  %

showCDS:-
    write('============== Error Report ==========='), nl,
    write('Current group  :'),printCDpath,nl,
    writeln('Current entries'),
    getCDField(entryList,Ent),printEntries(6,Ent),
    write('Elements       :'),
    getCDElement_list(Elist),writelistln(Elist),
    getCDField(elementList,EL),printElements(EL),
    writeln('Control:'),
    printFields([locusCount,celement,caspect,ccore,
                 coriginal, ccomment]),nl,!.

printCDpath:-
    getCDField(cpath,CPATH),
    getCDField(cgroup,CGROUP),
    getCDField(cgroupID,CGROUPID),
    concat([CGROUP,'=',CGROUPID],CG),
    append(CPATH,[CG],FULL_PATH),nl,
    pCDpath(FULL_PATH),!.
pCDpath([]):-!.
pCDpath([A]):-write(A),!.
pCDpath([A|B]):-A =.. [G,ID],concat([G,'=',ID],CG),write(CG),nl,pCDpath(B),!.

printField(F):-
    getCDField(F,V),write(F),write(': '),write(V),nl,!.

printFields([]):-nl,!.
printFields([F|R]):-
    printField(F),write('. '),printFields(R),!.

printElement(element(Name,Entries)):-
    tab(6),write(Name),write('='),printEntries(12,Entries),!.
printElements([]):-!.
printElements([E|R]):-
    printElement(E),printElements(R),!.

printEntries(_,[]):-!.
printEntries(N,[E|R]):-
    printEntry(N,E),printEntries(N,R),!.

printEntry(N,entry(Core,Original,Comment)):-
    printAsp(N,core,Core),
    printAsp(N,org,Original),
    printAsp(N,com,Comment),nl,!.

printAsp(_,_,[]):-!.
printAsp(__N,core,V):-
    /* tab(N),  write(T),write(': '),*/
    writelist0(V),
    !.
printAsp(__N,org,V):-
    /* tab(N),  write(T),write(': '),*/
    write('%'),writelist0(V),
    !.
printAsp(__N,com,V):-
    /* tab(N),  write(T),write(': '),*/
    write('#'),writelist0(V),
    !.    

% vim/prolog
% text of window:  data code cds %
%******************************************************
%  data code aux : auxiliary code for the data code
%    window.
% swi port 18:22 06-11-1999
% XSB port 21:43 18-01-2000
%     CDS handling was changed for the XSB version
%         instead of having the all structure as a single
%         property of atom cds, we store now each field
%         as a separate property to decrease the amount of
%         data being moved around.
%	  dataCode.pl was also reviewed to take profit
%         of the new organization.
%******************************************************
%%
