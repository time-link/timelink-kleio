:-module(dataCode,[
    initData/1,
    closeData/1,
    storeEls/1,
    newGroup/1,
    flushGroup/0,
    inc_group_count/3
    ]).
/**<module> Code executed during parsing of kleio data files.

	Overview
	========

   Information in data files is kept at the group level
    in a temporary storage structure called 'cds' (current
    data structure). When the end of a group is reached in
    the input data, the information in the temporary data
    structure is stored in the database. The exact form
    the database storage takes is determined by the 
    predicates in database predicates file. 
    
    The following preciates are defined bellow:
    
    initData(FileName) intializes the processing of a
      data file
    closeData(FileName) finalizes the processing of a
      data file
   
    storeEls takes a list of calls of the predicates bellow
      and executes them in order.  
    newGroup (when a new group is detected)
    newElement( when an explicit element is detected: element=entry)
    endElement (when an end of element is detected: '/')
    newAspect (when a new aspect is detected: '#' or "%")
    newEntry (when a new entry is detected: ';')
    storeAspect (when processing text for an aspect of an element).
   
*/
:-use_module(logging).
:-use_module(dataCDS).
:-use_module(dataDictionary).
:-use_module(errors).
:-use_module(gactoxml). % TODO: this should not be hard coded. A call back pattern maybe.
:-use_module(reports).
:-use_module(persistence).
:-use_module(utilities).

%  initData(FileName) intializes the processing of a
%    data file
%******************************************************
%  %
initData(FileName):-
    log_debug('translate: initData --> ~w',[FileName]),
    delCD, %cleans temporary data storage%
    createCD, % creates an empty data storage structure%
    initGroupCounters, 
    initErrorCount,
    %put_value(max_errors,100),
    db_init, % call user defined database initialization code %
    !.
%******************************************************
%  closeData(FileName) finalizes the processing of a
%    data file
%******************************************************
%  %
closeData(FileName):-
    log_debug('translate: closeData ->~w~n',[FileName]),
    db_close,!. % call user defined database clean-up code %
%******************************************************
%    storeEls takes a list of calls of the above predicates
%      and executes them in order.  
%******************************************************
% %
storeEls(E):-
    member(P,E),
    call(P),
    fail.
storeEls(_).

%******************************************************
%    storeElsR takes a list of calls of the above predicates
%      and executes them in order. Recursive version 
%      with record based CDS Storage
%******************************************************
% %
storeElsR([]):- !.
storeElsR([A|B]):-
    dataCDS:getCDSR(CDSR),
    storeElsR(CDSR,[A|B],CDSR_Final),
    dataCDS:setCDSR(CDSR_Final),!.

storeElsR(CDSR,[],CDSR):-!.
storeElsR(CDSR,[A|B],CDSR_Final):-
    storeElsR_A(CDSR,A,CDSR_A),
    storeElsR(CDSR_A,B,CDSR_Final).

storeElsR_A(CDSR,storeCore(C),CDSR_A):-
    storeCoreR(C,CDSR,CDSR_A),!.

storeElsR_A(CDSR,newElement(C),CDSR_A):-
    newElementR(C,CDSR,CDSR_A),!.

%******************************************************
%  newGroup(G) called when a new group is encountered
%    tests if the new group is a doc if true calls newDoc
%    if not
%    finishes processing of current group, and initializes
%    the new group storage
%    Future checks on semper, solum, and repetitio
%    will be done here
%******************************************************
%  %

newGroup(N):-
    isDoc(N),!, % if it is a doc jump%
    newDoc(N).
newGroup(N):-
    \+ isDoc(N), !,   % if it is not %
    flushGroup,        % flush last group %
    initNewGroup(N).% initialize a new one %
    
newGroup(N):-
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['** Failure. Could not process group ',N],[line_number(L),line_text(Line)]),!.

%  newDoc(N) processes a new doc and does the newgroup
%    processing afterwards
%  %
newDoc(N):-
    report([write('** New document: '),writeln(N)]),
    flushGroup,
    initNewGroup(N),!.

%  flushGroup: flushes current group, storing it 
%    futures checks on certe will be made here
%  %

flushGroup:-
    getCDField(cgroup,[]),!. % no current group %
flushGroup:-
    getCDField(cgroup,G),!,
    G \= [],
    endElement,
    makeID(ID),   % construct an ID for this group %
    check_elements(G,ID), % checks certe elements, etc.%
    db_store.   % here the user defined database storage is called %
flushGroup:-
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['** Failure. Could not process ',flushGroup],[line_number(L),line_text(Line)]),!.

check_elements(G,GID):- % tests if certe elements where registered%
    clioGroup(G,ID),         % see if it has a locus list %
    get_prop(ID,certe,CerteList),
    getCDElement_list(Els),
    setof(X,(member(X,CerteList), \+ member(X,Els)),[A|B]),
    list_to_a([A|B],Missing),
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['** Error: missing element(s) in ',G,'(',GID,') must have: ' | Missing],[line_number(L),line_text(Line)]),!.
check_elements(_,_):-!.


%  initNewGroup(G). Intializes a new group on the current
%    data storage structure. Information regarding
%    the precedent group is erased.
%    change if the CDS table is changed
%  %
% FALTA VERIFICAR SE O GRUPO e' BOM 
%    se nao for e' preciso armazenar este facto para
%    o processamento seguinte saltar o lixo %
initNewGroup(G):- 
    %log_debug('Entering initNewGroup~w',[G]),
    getCDField(cpath,OLDPATH),
    getCDField(cgroup,OG),
    getCDField(cgroupID,OID),
    updatePath(OG,OID,G,OLDPATH,NEWPATH),          
    resetGroupCounters(G),  
    setCDS(cds(NEWPATH,G,[],0,[],[],[],[],[],[],core,[],[],[])),
    % save the line information for the start of the group
    get_prop(line,number,L),
    get_prop(line,text,Line),
    set_prop(gline,number,L),
    set_prop(gline,text,Line),
    %writeln('exiting initNewGroup'-G),

    !.

initNewGroup(G):-
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['***', G, ' group not stored.'],[line_number(L),line_text(Line)]),
    getCDS(cds(CPATH,OG,OID,__LOCUSCOUNT,__ELEMENTLIST,__CELEMENT,
                _ENTRYLIST,__CASPECT,__CCORE,__CORIGINAL,__CCOMMENT)),
    resetGroupCounters(G),  
    setCDS(cds(CPATH,OG,OID,0,[],[],[],core,[],[],[])),!.

%*********************************************************
% update path links the newgroup to the existing path. %
% If the new group can be linked to several of the preceeding
% groups in the current path the following precedences apply:
%  First the group will be tested as a contained group of the current group
%  if that fails then the current path is examined from the lowest level
%  to the top to check if the new group is contained in any of the
%  groups of the current path.
%  In short, each new group is linked to the current path in such a way
%  that the resulting path is the longest possible. When faced with a choice
%  between two alternative links, update_path takes the one that keeps the
%  new group at the lowest level.
%%
updatePath(OldGroup,OldID,NewGroup,Path,NewPath):-
    updtp(OldGroup,OldID,NewGroup,Path,NewPath),!.

updatePath(OldGroup,OldID,NewGroup,Path,Path):-
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['*** Current group ',NewGroup,' cannot be linked with previous group ',
                    OldGroup,'(',OldID,')'], [line_number(L),line_text(Line)]),
    fail.

% if old group is the same as new group no change in path %
updtp(NewGroup,_,NewGroup,P,P):-!.

% if no path ancestor of NewGroup is a doc %
updtp(Anc,OldID,__NewGroup,[],[A]):-
    isDoc(Anc),
    A=..[Anc,OldID],!.

% if NewGroup is a document path = [] %
updtp(__OldGroup,__OldID,NewGroup,_,[]):-
    isDoc(NewGroup),!.

% else OldGroup is ancestor of NewGroup 
%   Add OldGroup to path %
updtp(OldGroup,OldID,NewGroup,Path,NewPath):- 
    anc_of(NewGroup,OldGroup),         
    A=..[OldGroup,OldID],
    append(Path,[A],NewPath),!.

% else see if ancestor is up in path %
updtp(__OldGroup,__OldID,NewGroup,Path,NewPath):- 
    reverse(Path,RPath), %reverse the path list %
    member(A,RPath),     % get groups from path in reverse order%
    A =.. [Anc,__AID],     % get group names %          
    anc_of(NewGroup,Anc),% see if they are the ancestor of newGroup%
    cutListAfter(Path,A,NewPath),!. % if so cut path at that point %
        
cutListAfter([E|_L],E,[E]):-!.
cutListAfter(L,G,NL):-
    append([A|B],[G|_C],L),
    append([A|B],[G],NL),!.

%******************************************************
%  newElement(E) called when a new explicit element
%    'elementName=' is encountered in the input data file.
%    Stores the element name as the current element.
%    if the element name is not is the current group definition
%    (in the locus, ceteri and certe lists) a warning is
%    given but the element is stored anyway.
%******************************************************
%  %
newElement(E):-
    verify_element(E),
    setCDField(celement,E),   % set current element %
    !.
newElement(E):-
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['** Failure: ',newElement(E)], [line_number(L),line_text(Line)]),!.

verify_element(E):-
    getCDField(cgroup,G),
    velement(E,G),!.

velement(E,G):-element_of(E,G),!.
velement(E,G):-
    \+ element_of(E,G),
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['** ',G,' unknown element: ',E,'.'], [line_number(L),line_text(Line)]),
    !.

newElementR(E,CDSR,CDSRA):-
    verify_elementR(E,CDSR),
    dataCDS:setCDRField(celement,E,CDSR,CDSRA),
    !.

verify_elementR(E,CDSR):-
    dataCDS:getCDRField(cgroup,G,CDSR),
    velement(E,G),!.

%******************************************************
%  endElement. adds current element to the current
%    group element list. If no current element name is 
%    defined the locus list of the current group is
%    used to find an implicit element name
%******************************************************
%  %

endElement:-
    endEntry,  % add current entry to the element entry list%
    check_ename,% see if current element has a name %
    storeElement,!.
endElement:-
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['** Failure: ',endElement], [line_number(L), line_text(Line)]),!.

check_ename:-getCDField(celement,A), A\= [].
check_ename:-
    getCDField(celement,[]),% current element undefined %
    getCDField(cgroup,G),   % get current group %
    clioGroup(G,ID),         % see if it has a locus list %
    get_prop(ID,locus,LocusList),
    getCDField(locusCount,N),% get locus counter %
    N1 is N+1,               % increment counter %
    member_nth(EName,LocusList,N1),% find locus element %
    setCDField(celement,EName),    % set current el. to it %
    setCDField(locusCount,N1), % update locus counter %
    !.
check_ename:-
    %getCDField(entryList,Entries),   %get current entries %
    getCDField(coreEntryList,CoreEntries),
    getCDField(originalEntryList,OriginalEntries),
    getCDField(commentEntryList,CommentEntries),
    Entries = [CoreEntries,OriginalEntries,CommentEntries],
    with_output_to(string(S),printEntries(1,Entries)),
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['** Failure: undefined element. ',S], [line_number(L), line_text(Line)]),
    setCDField(celement,'UNDEFINED'),
    !.

%**************************************************************
%  storeElement - store temporarily current element
%    adds current element to the current
%    group element list and cleans current element data
%  %
% TEM DE TER EM CONTA QUE O ELEMENTO PODE JA TER
%    APARECIDO %
%***************************************************************
%%
storeElement:-
    getCDField(elementList,Elements),
    getCDField(celement,EName),
    getCDField(coreEntryList,CoreEntries),
    getCDField(originalEntryList,OriginalEntries),
    getCDField(commentEntryList,CommentEntries),
    append(Elements,[element(EName,[CoreEntries,OriginalEntries,CommentEntries])],NewEls),
    setCDField(elementList,NewEls),
    setCDField(celement,[]),
    setCDField(entryList,[]),
    setCDField(coreEntryList,[]),
    setCDField(originalEntryList,[]),
    setCDField(commentEntryList,[]),
    setCDField(caspect,core),
    setCDField(ccore,[]),
    setCDField(coriginal,[]),
    setCDField(ccomment,[]),
    !.
storeElement:-
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['** Failure: ',storeElement], [line_number(L),line_text(Line)]),!.


%******************************************************
%  newEntry - adds current entry to the CDS entryList
%******************************************************
%  %
newEntry:-
    endEntry,!.

%  endEntry - adds current entry to the CDS entryList
% 
%  %
endEntry:-
    getCDField(caspect,CAspect),
    endEntry(CAspect),!.

endEntry:-
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['** Failure: ',endEntry],[line_number(L),line_text(Line)]),!.

endEntry(core):-
    getCDField(coreEntryList,Elist),
    getCDField(ccore,C),
    rmv_lead_space(C,C1),                  % remove leading spaces %
    reverse(C1,Core),
    append(Elist,[entry(Core)],NElist),   % add it to current list %
    setCDField(coreEntryList,NElist),                % clean current entry %
    setCDField(ccore,[]),!.

endEntry(original):-
    getCDField(originalEntryList,Elist),
    getCDField(coriginal,C),
    rmv_lead_space(C,C1),                  % remove leading spaces %
    reverse(C1,Original),
    append(Elist,[entry(Original)],NElist),   % add it to current list %
    setCDField(originalEntryList,NElist),                % clean current entry %
    setCDField(coriginal,[]),!.

endEntry(comment):-
    getCDField(commentEntryList,Elist),
    getCDField(ccomment,C),
    rmv_lead_space(C,C1),                  % remove leading spaces %
    reverse(C1,Comment),
    append(Elist,[entry(Comment)],NElist),   % add it to current list %
    setCDField(commentEntryList,NElist),                % clean current entry %
    setCDField(ccomment,[]),!.


rmv_trail_space(A,B):-
    reverse(A,C),
    rmv_lead_space(C,D),
    reverse(D,B),!.
rmv_lead_space([],[]):-!.
rmv_lead_space([' '|B],C):-
    rmv_lead_space(B,C),!.
rmv_lead_space(A,A):-!.
%******************************************************
%  newAspect(T) changes current aspect to T
%******************************************************
%  %
newAspect(T):-
    endEntry,
    setCDField(caspect,T),!.

%******************************************************
%  storeCore(I) - add I to core information of current el. 
%    stores I according to CDS field caspect
%******************************************************
%  %
storeCore(I):-getCDField(caspect,A),stCore(A,I),!.
storeCore(I):-
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['** Failure: ',storeCore(I)],[line_number(L),line_text(Line)]),!.
stCore(core,I):-
    getCDField(ccore,C),!,
    setCDField(ccore,[I|C]),!.
stCore(original,I):-
    getCDField(coriginal,C),!,
    setCDField(coriginal,[I|C]),!.
stCore(comment,I):-
    getCDField(ccomment,C),!,
    setCDField(ccomment,[I|C]),!.

%******************************************************
%  storeCoreR(I) - add I to core information of current el. 
%    stores I according to CDS field caspect
%    Record version
%******************************************************
%  %
storeCoreR(CDSR,I,CDSR_Next):-
    dataCDS:getCDRField(CDSR,caspect,A),
    stCoreR(CDSR,A,I,CDSR_Next),!.

storeCoreR(_,I,_):-
    get_prop(gline,number,L),
    get_prop(gline,text,Line),
    error_out(['** Failure: ',storeCore(I)],[line_number(L),line_text(Line)]),!.

stCoreR(CDSR,core,I,CDSR_Next):-
    dataCDS:getCDRField(ccore,C,CDSR),!,
    dataCDS:setCDRField(ccore,[I|C],CDSR,CDSR_Next),!.
stCoreR(CDSR,original,I,CDSR_Next):-
    dataCDS:getCDRField(coriginal,C,CDSR),!,
    dataCDS:setCDRField(coriginal,[I|C],CDSR,CDSR_Next),!.
stCoreR(CDSR,comment,I,CDSR_Next):-
    dataCDS:getCDRField(ccore,C,CDSR),!,
    dataCDS:setCDRField(ccore,[I|C],CDSR,CDSR_Next),!.



%******************************************************
%  initGroupCounters
%******************************************************
%  %
initGroupCounters:-
    clioGroup(__G,ID),
    set_prop(ID,counter,0),
    fail,!.
initGroupCounters:-!. 

%******************************************************
%  resetGroupCounters(G) - reset group counters of
%    subgroups of G. If a subgroup has the property
%    parent group is encountered in the input data.
%     (kleio man. 68)
%******************************************************
%  %
resetGroupCounters(G):-
    subgroups(G,L),% get the subgroups%
    member(D,L),
    clioGroup(D,ID),
    rgc(ID),
    fail.
resetGroupCounters(__G):-!. 

rgc(ID):-
    get_prop(ID,identificatio,non),
    set_prop(ID,counter,0),!.
rgc(ID):-get_prop(ID,identificatio,sic),!.

% increment group counters %
inc_group_count(__G,ID,N):-
    get_prop(ID,counter,N1),
    N is N1+1,
    set_prop(ID,counter,N),!.



:- begin_tests(dataCDS).

test(setCDRField):-
    mytest(setCDRField),!.

mytest(setCDRField):-
    dataCDS:createCD,
    dataCDS:getCDSR(CDSR),
    dataCDS:setCDRField(caspect,ccore,CDSR,CDSR_new),
    dataCDS:getCDRField(caspect,ccore,CDSR_new),
    dataCDS:setCDSR(CDSR_new),
    check_for_persistence(caspect,ccore).

% this test if changes to CDSR persist.
check_for_persistence(Field,Value):-
    dataCDS:getCDSR(CDSR),
    dataCDS:getCDRField(Field,Value,CDSR),!.

    
% vim: filetype=prolog ts=3
/*
	===============================================================================
 	dataCode.pl 
		  This file contains the predicates
		  called by the syntactic analyser for data
	     files.

 $Id$
 $Date$
 $Author$
 
 History:
 ========

 $Log: dataCode.pl,v $
 Revision 1.2  2006/05/16 10:52:43  Joaquim
 update dos ficheiros prolog

 Revision 1.1  2004/04/08 14:45:24  ltiago
 Source code has passed from Joaquim to Tiago.
 Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
 File build.xml, web.xml and velocity.properties were changed

 Revision 1.1.1.1  2001/04/19 23:25:43  jrc
 New Repository.

 Revision 1.1.1.1  2001/04/18 23:34:39  jrc
 CVS repository moved from llinux to MacOSX.

 Revision 1.3  2001/01/28 00:06:30  jrc
 Cleaned some error messages.


	Put under source control January 2001.

   Feb 2000 optimized for new CDS storage model (see dataCDS.p).

   Port to XSB 22:23 18-01-2000

   November 1999 ported to swi prolog
   
	In July 1991 the storeCore predicate was changed. Tokens
    are stored in reverse order to quicken things up and then
    reversed in endEntry.

   In January 1991 the procedure taken by newGroup to link
    the new group with the current path was changed to
    deal more consistently with groups that belong
    to several groups. See comments below at the predicate
    update_path

   */



