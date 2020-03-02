:-module(dataDictionary,[
        create_stru/1,
        clean_stru/1,
        clioStru/1,
        clioGroup/2,
        clioElement/2,
        isDoc/1,
        anc_of/2,
        subgroups/2,
        element_of/2,
        create_groups/1,
        create_elements/1,
        copy_fons_e/2,
        copy_fons_g/2,
        get_group_prop/3,
        set_group_prop/3,
        set_groups_prop/3,
        set_element_prop/3,
        get_element_prop/3,
        set_elements_prop/3,
        set_group_defaults/1,
        set_element_defaults/1,
        all_groups/1,
        all_elements/1,
        show_stru/0,
        show_groups/0,
        show_elements/0,
        clean_groups/1,
        clean_elements/1,
        make_html_doc/1
    ]).
/** <module> Code for dealing with the data dictionary.

    The data dictionnary stores the information contained in
    the kleio structure file
    FALTA AQUI A EXPLICACAO DAS ESTRUTURAS EXTERNAS
    TODO: Para ter processamento com multiplos schemas (strus) podia-se usar 
    convenções semelhantes às que do mysql: use_stru(S) daqui em diante os comandos dizem respeito a S
    Isso talvez envolvesse apenas make_group e make_element e clioGroup e clioElement, mas tem de ser analisado com cuidado.

* create_stru(S) S= status of nomino command
    creates a predicate clioStru(F) where F is the nomen
    parameter of the nomino command and copies the
    other parameters of nomino as properties of F    
*  clean_stru(F):- cleans any existing structure definition
     for file F
*  isDoc(N) tests if N is a document
*  anc_of(G,A) returns in A the ancestor of G    
*  subgroups(G,S) S is the list of subgroups of G    
*  element_of(E,G):- checks to which group belongs
    element G.    
*  create_groups(NameList)
     creates, if not yet created, clauses for the groups
     in NameList
*  create_elements(NameList)
     creates, if not yet created, clauses for the elements
      in NameList
*  set_group_prop(Group,Property,Value)
      stores the property and value in the group
*  set_groups_prop(List,Prop,Value)
     same as above but sets the property for a list of groups
*  set_element_prop(Element,Property,Value)
      stores the property and value in the element
*  set_elements_prop(List,Prop,Value)
     same as above but sets the property for a list of elements
*  set_group_defaults(Group) sets default values
*  set_element_defaults(Element) sets default values
    
*  all_groups(List) - list of all the currently defined groups
*  all_elements(List) - list of all the currently defined elements
*  show_stru:- show current structure definition
*  show_groups:- show the properties of all groups
*  show_elements:- show the properties of all elements
    
*  clean_groups. deletes previous group definitions
*  clean_elements. deletes previous element definitions

*/
:-use_module(utilities).
:-use_module(errors).
:-use_module(reports).
:-use_module(persistence).
:-use_module(dataCDS).
:-use_module(apiTranslations).

% dynamic thread local
?-thread_local(clioStru_/1).
?-thread_local(clioGroup_/2).
?-thread_local(clioElement_/2).


%******************************************************
%  create_stru(S) S= status of nomino command
%    creates a predicate clioStru(F) where F is the nomen
%    parameter of the nomino command and copies the
%    other parameters of nomino as properties of F
%    Previous clioStru are deleted
%******************************************************
%  %
create_stru(notOk):-error_out('** Structure definition failed'),!.
create_stru(ok):-
    get_prop(nomino,nomen,File),    % get strucuture name %
    report([writelistln(['Creating new structure definition for:',File])]),
    clean_stru(File),               % delete previous definition if any %
    assert(clioStru_(File)),         % store a structure predicate %
    get_props(nomino,List),
    forall(member(P,List),(get_prop(nomino,P,V),set_prop(File,P,V))),!.

%% clioStru(?FileName) is nondet.
%
% FileName is a Kleio structure file stored.
%
clioStru(FileName):-clause(clioStru_(FileName),true).


%*************************************************************
%    clean_stru(F):- cleans any existing structure definition
%      for file F
%******************************************************
%  %
clean_stru(F):-
   retractall(clioStru_(F)),% clean previous structure %
   del_props(F),       % delete previous properties %
   clean_groups(F),       % clean groups definition %
   clean_elements(F),!.      % clean elements definition %

%******************************************************
%  isDoc(N) tests if N is a document (inspecting the
%    primum property of the current structure)
%******************************************************
%  %
isDoc(N):-
    clioStru(S),         % check if this is a new doc %
    get_prop(S,primum,N).
%******************************************************
%  anc_of(G,A) returns in A the ancestor of G
%   backtracks on several ancestors
%    by order of the structure definition file
%    If no ancestor (the case with documents)
%    A = []
%******************************************************
%  %
anc_of(G,[]):-isDoc(G).
anc_of(G,A):-          
    clioGroup(A,ID),
    get_prop(ID,repetitio,L),
    member(G,L).
anc_of(G,A):-
    clioGroup(A,ID),
    get_prop(ID,semper,L),
    member(G,L).
anc_of(G,A):-
    clioGroup(A,__ID),
    get_prop(G,solum,L),
    member(A,L).
anc_of(G,A):-
    clioGroup(A,ID),
    get_prop(ID,pars,L),
    member(G,L).
%******************************************************
%  subgroups(G,S) S is the list of subgroups of G
%******************************************************
%  %
subgroups(G,S):-
    findall(D,(anc_of(D,G),clioGroup(D,_)),S),!.
%******************************************************
%  element_of(E,G):- checks to which group belongs
%    element G.
%    Note that acording to kleio manual, p.69
%    certe lists are redundant with locus and ceteri
%******************************************************
%  %
element_of(E,G):-
    clioGroup(G,ID),
    get_prop(ID,certe,L),
    member(E,L).
element_of(E,G):-
    clioGroup(G,ID),
    get_prop(ID,ceteri,L),
    member(E,L).

%*************************************************************
% create_groups(NameList)
%   creates, if not yet created, structures for the groups
%   in NameList
%*************************************************************
% %
create_groups([]):-!.
create_groups([Group|OtherGroups]):-
   create_group(Group),
   create_groups(OtherGroups),!.
create_group(Group):-
   make_group(Group),
   set_group_defaults(Group),!.

%% make_group(+Group) is det. 
%
% Creates a group definition.
%      Does nothing if there is already a group definition
%      for Group
%
make_group(Group):-
   clioGroup(Group,_),!.

make_group(Group):-
   gensym(cgroup,Id),   % generate an Id %
   assert(clioGroup_(Group,Id)),!. % TODO: make multi schema aware

%% clioGroup(?GroupName,?GroupId) is nondet.
%
% GroupName is a Group defined in the current Scheme and GroupId
% is the internal id of the Group.
%
% @see make_group/1
%
clioGroup(GroupName,GroupId):-
    clause(clioGroup_(GroupName,GroupId),true).

%*************************************************************
% create_elements(NameList)
%   creates, if not yet created, structures for the elements
%   in NameList
%*************************************************************
% %
create_elements([]):-!.
create_elements([Element|Elements]):-
   create_element(Element),
   create_elements(Elements),!.
create_element(Element):-
   make_element(Element),
   set_element_defaults(Element),!.
%*************************************************************
% make_element(Element)  creates an element definition.
%      does nothing if there is already an element definition
%      for Element
%*************************************************************
% %
make_element(Element):-
   clioElement(Element,_),!.
make_element(Element):-
   gensym(cel,Id),   % generate an Id %
   assert(clioElement_(Element,Id)),!.

%%  clioElement(?ElementName,?ElementId) is nondet.
%
% ElementName exists in the current schema with internal identifier 
% ElementId
%
clioElement(ElementName,ElementId):-
    clause(clioElement_(ElementName,ElementId),true).


%% set_group_prop(+Group,+Property,+Value) is det.
%
%      stores the property and value in the group
%
set_group_prop(G,P,V):-
   clioGroup(G,I),
   set_prop(I,P,V),!.
%*************************************************************
% get_group_prop(Group,Property,Value)
%      stores the property and value in the group
%  NEW AUG 97
% cut removed to make it backtrackable
%*************************************************************
% %
get_group_prop(G,P,V):-
   clioGroup(G,I),
   get_prop(I,P,V).
%*************************************************************
% set_groups_prop(List,Prop,Value)
%      same as above but sets the property for a list of groups
%*************************************************************
% %
set_groups_prop(L,P,V):-
   forall(member(G,L),set_group_prop(G,P,V)),!.

%*************************************************************
% set_element_prop(Element,Property,Value)
%      stores the property and value in the element
%*************************************************************
% %
set_element_prop(E,P,V):-
   clioElement(E,I),
   set_prop(I,P,V),!.

%*************************************************************
% get_element_prop(Element,Property,Value)
%      gets the property of an element
%      NEW Nov 2000.
%*************************************************************
% %
get_element_prop(E,P,V):-
   clioElement(E,I),
   get_prop(I,P,V).
%
%*************************************************************
% set_elements_prop(List,Prop,Value)
%      same as above but sets the property for a list of elements
%*************************************************************
% %
set_elements_prop(L,P,V):-
   forall(member(E,L),set_element_prop(E,P,V)),!.

%*************************************************************
% set_group_defaults(Group) sets default values
%   for this group properties.
%  see CLIO manual p.67, 5.3.2.
%*************************************************************
% %
set_group_defaults(Group):-
   set_pars_defaults(Group), %set defaults of pars directive %
   set_generic_g(Group),       %copy generic group properties if any %
   !.
%*************************************************************
% set_element_defaults(Element) sets default values
%   for this element properties.
%  see CLIO manual p.73, .
%*************************************************************
% %
set_element_defaults(E):-
   set_terminus_defaults(E), %set defaults of terminus  directive %
   set_generic_e(E),       %copy generic element properties if any %
   !.

set_pars_defaults(Group):-
   clioGroup(Group,Id),      %get the group id %
   % store default params %
   set_prop(Id,ordo,sic),
   set_prop(Id,sequentia,sic),
   set_prop(Id,identificatio,non),% MUDAR deve ser sic se for um doc % 
   set_prop(Id,post,non),
   set_prop(Id,prae,non),
   make_signum(Group,Signum),
   set_prop(Id,signum,Signum),!.

make_signum(Group,Signum):- % take the first 3 letters of group name%
   name(Group,Chars),
   mks(Chars,S),
   name(Signum,S),!.

mks(Chars,[A,B,C]):-
   append([A,B,C],_,Chars),!.
mks([A,B],[A,B]):-!.
mks([A],[A]):-!. 

% pars, sine, signa, forma, ceteri, solum params not implemented %
set_terminus_defaults(Element):-
   clioElement(Element,Id),      %get the element id %
   % store default params %
   set_prop(Id,modus,lingua),
   set_prop(Id,primum,lingua),
   set_prop(Id,secundum,lingua),
   set_prop(Id,ordo,simplex),
   set_prop(Id,identificatio,non),% MUDAR deve ser sic se for um doc % 
   set_prop(Id,post,non),
   set_prop(Id,prae,non),
   set_prop(Id,cumule,non),!.


set_generic_g(Group):-
   generic_groups(Group,List), % belongs to generic group?%
   copy_groups(Group,List),!.  % if so copy properties %
set_generic_g(__Group):-!.

set_generic_e(E):-
   generic_elements(E,List), % belongs to generic element?%
   copy_elements(E,List),!.  % if so copy properties %
set_generic_e(__E):-!.

%*************************************************************
% generic_groups(Group,List) 
%      List contains the names of generic groups that match
%      Group
%*************************************************************
% %
generic_groups(Group,List):-
   get_prae(group,Group,Prae),
   get_post(group,Group,Post),
   setof(G,(member(G,Prae);member(G,Post)),List),!. % union %

%*************************************************************
% generic_elements(Element,List) 
%      List contains the names of generic elements that match
%      Element
%*************************************************************
% %
generic_elements(Element,List):-
   get_prae(element,Element,Prae),
   get_post(element,Element,Post),
   setof(N,(member(N,Prae);member(N,Post)),List),!. % union %

get_prae(T,Name,Prae):-
   setof(N,isPrae(T,N,Name),Prae);Prae=[],!.
get_post(T,Name,Post):-
   setof(N,isPost(T,N,Name),Post); Post=[],!.

isPrae(T,N,Name):-        % N is a generic name %
   get_name(T,N,ID),  % get names with prae property%
   N \= Name,       % but not the one we are testing %
   get_prop(ID,prae,sic), % filter to those that have it = sic%
   name(N,NN),
   name(Name,NName),
   begins_with(NN,NName).
isPost(T,N,Name):-        % N is a generic group %
   get_name(T,N,ID),  % get names with post property%
   N \= Name,       % but not the one we are testing %
   get_prop(ID,post,sic), % filter to those that have it = sic%
   name(N,NN),
   name(Name,NName),
   append(_,NN,NName).

get_name(group,N,I):-
   clioGroup(N,I).
get_name(element,N,I):-
    clioElement(N,I).
%*************************************************************
% copy_groups(Group, List)
%      copies the properties of the groups in List to
%          group Group
%*************************************************************
% %
copy_groups(__G,[]):-!.
copy_groups(G,[A|B]):-
   copy_group(G,A),
   copy_groups(G,B),!.

copy_group(G,H):- %copies properties in H to G %
   clioGroup(H,ID), %get id of group H)%
   get_props(ID,List),% get its properties %
   forall((member(P,List),P \= fons),(get_prop(ID,P,V),set_group_prop(G,P,V))),!.

%*************************************************************
% copy_elements(Element, List)
%      copies the properties of the elements in List to
%          group Element
%*************************************************************
% %
copy_elements(__E,[]):-!.
copy_elements(E,[A|B]):-
   copy_element(E,A),
   copy_elements(E,B),!.

copy_element(E,H):- %copies properties in H to E %
   clioElement(H,ID), %get id of element H)%
   get_props(ID,List),% get its properties %
   forall((member(P,List), P \= fons),(get_prop(ID,P,V),set_element_prop(E,P,V))),!.
 
%*************************************************************
% copy_fons_g(Group,Groups)
%      copies the proprieties of Group to Groups
%      assuming that Group exists
%*************************************************************
% %
copy_fons_g(Group,Groups):-
   clioGroup(Group,__ID),
   forall(member(G,Groups),copy_group(G,Group)),!.
copy_fons_g(Group,__Groups):-
   \+ clioGroup(Group,__ID),
   warning_out(['copy_fons - undefined fons group',Group]),!.

%*************************************************************
% copy_fons_e(Element,Elements)
%      copies the proprieties of Element to Elements
%      assuming that Element exists
%*************************************************************
% %
copy_fons_e(Element,Elements):-
   clioElement(Element,__ID),
   forall(member(E,Elements),copy_element(E,Element)),!.
copy_fons_e(Element,__Elements):-
   \+ clioElement(Element,__ID),
   warning_out('copy_fons - undefined fons element'),!.

%*************************************************************
% all_groups(List) - list of all the currently defined groups
%*************************************************************
% %
all_groups(List):-findall(G,clioGroup(G,_),List),!.

%*************************************************************
% all_elements(List) - list of all the currently defined elements
%*************************************************************
% %
all_elements(List):-findall(E,clioElement(E,_),List),!.

%******************************************************
%  show_stru:- show current structure definition
%******************************************************
%  %
show_stru:-
    clioStru(S),
    show_structure(S).
show_stru:- \+ clioStru(__S),
          error_out('**No structure definition.'),!.

show_structure(S):-
    write('Structure definition for: '),write(S),tab(1),
    get_prop(S,primum,D), % get the document name %
    write('document: '),writeln(D), 
    shgroup(D,1),
    show_elements.
shgroup(G,N):-
    clioGroup(G,ID),
    tab(N),write(G),
    N1 is N+5,show_props(ID,N1),
    subgroups(G,L),
    tab(N1),writelist0ln(['Subgroups of ',G,': ',L]),
    shgroups(L,N1),
    tab(N),write('End '),writeln(G).
shgroups([],_):-!.
shgroups([G|R],N):-
    shgroup(G,N),
    shgroups(R,N).

show_els(G,N):-
    element_of(E,G),
    clioElement(E,ID),
    tab(N),write(' Terminus for: '),
    write(E),
    N1 is N+5,show_props(ID,N1),
    fail.
show_els(_,_):-!.
    

%*************************************************************
% show_groups:- show the properties of all groups
%      
%*************************************************************
% %

show_groups:-nl,write('Groups (cliogroup): '),listClioGroups,
              forall(clioGroup(G,ID),
              (writeln('-------------'),
               write(G),tab(2),show_props(ID))).

listClioGroups:-clioGroup(G,_),
                write(G),tab(1),fail.
listClioGroups:-nl,!.

%% make_html_doc(+Path) is det.
%  Write to Path html documentation of current processed kleio structure (schema).
%
%
make_html_doc(DocPath):-
             (exists_directory(DocPath) ;  make_directory(DocPath)),
             working_directory(CD,DocPath),
             show_groups_html,
             working_directory(_,CD),!.

%! make_json_doc(+ClioFile,+Path,-JsonStru) is det.
%  Generate a JSON representation of a kleio stru file (Schema)
%
% Format of the json file
% {
%  { "path": "PATH TO ThE STRUCTURE FILE USED"
% { "groups": [
% 	"GroupName1":{
% 	 "minimal": "String",
% 	 "typical": "String",
% 	 "complete": "String",
% 	"occurs":[ "group1","group2"],
% 	"includes:["group2","group3"]
% 	},
% 	"GroupName2":
% 		{	}
%     ]
%}
make_json_doc(ClioFile,DocPath,JSON_STRU):-
   (exists_directory(DocPath) ;  make_directory(DocPath)),
   working_directory(CD,DocPath),
   collect_groups_json(GroupsInfo),
   JSON_STRU={path:ClioFile,groups:GroupsInfo},
   working_directory(_,CD),!.
             
show_groups_html:-
              clioGroup(G,_),
              %cd(doc),
              concat(G,'.html',GFILE),
              writeln(writing-GFILE),
              open_file_write(GFILE),
              group_to_html(GFILE,G),
              writeln(closing-GFILE),
              close_file(GFILE),
              fail.
show_groups_html:-!.

collect_groups_json(GroupsInfo):-
   setof(G,I^clioGroup(G,I),ListOfGroups),
   collect_groups_json(ListOfGroups,GroupsInfo).

collect_groups_json([G|MoreGroups],[GInfo|MoreGInfo]):-
   collect_group_json(G,GInfo),
   collect_groups_json(MoreGroups,MoreGInfo).

collect_group_json(G,I):-
    clioGroup(G,ID),
        get_props(ID,P),
    (get_prop(ID,certe,C); C=[]),
    (get_prop(ID,locus,L); L=[]),
    (get_prop(ID,ceteri,X); X=[]),
    (get_prop(ID,pars,Pars); Pars=[]),
    (get_prop(ID,repetitio,Repetitio); Repetitio=[]),
    I = {G:{}}.


collect_group_json(_,_):-!.

group_to_html(File,G):-
    telling(O),
    tell(File),
    clioGroup(G,ID),
    writeln('<html>'),
    writeln('<head>'),
    writelist(['<title>',G,'</title>']),
    writeln('</head>'),    
    writeln('<body>'),
    writeln('<!-- '),
    get_props(ID,P),
    writelist0ln([G,'[',ID,']','=' , P]),
    (get_prop(ID,certe,C); C=[]),writeln(certe-C),
    (get_prop(ID,locus,L); L=[]),writeln(locus-L),
    (get_prop(ID,ceteri,X); X=[]),writeln(ceteri-X),!,
    (get_prop(ID,pars,Pars); Pars=[]),writeln(pars-Pars),!,
    (get_prop(ID,repetitio,Repetitio); Repetitio=[]),writeln(repetitio-Repetitio),!,
    writeln(' -->'),
    writelist0ln(['<font size="+2">',G,'$</font>']),
    (member(fons,P) -> 
        (writeln(' (based on  :'),get_prop(ID,fons,Fons),
         writelist0ln(['<A href="',Fons,'.html">',Fons,'</A>)<br>']))
         ;
         true
         ),
    (clause(gdoc(G,Doc),true)->(list_to_a0(Doc,SDoc),write(SDoc),writeln('<br>'),show_edocs(G));true),
    writeln('<blockquote>'),
    writelist0(['Minimal:<b>  ',G,'$']),show_positional(C,L),write('</b>'),
    writelist0(['Typical:<b>  ',G,'$']),show_locus(C,L),write('</b>'),
    sort(X,SX),merge_set(L,C,T),merge_set(T,SX,All),
   
    writelist0(['Complete: <b>',G,'$']),show_positional(All,L), write('</b>'),
    sort(Pars,SPars),sort(Repetitio,SRepetitio),merge_set(SPars,SRepetitio,Includes),
    writeln('Occurs in: <b>'), show_ancestors(G),write('</b>'),
    writeln('Includes: <b>'), show_included(Includes),write('</b>'),
    writeln('</blockquote>'),
    writeln('</body>'),
    writeln('</html>'),
   % told,
    tell(O).

	 
show_edocs(G):-	 
	write('<DL>'),
	 clause(edoc(G,E,Doc),true),
	 write('<DT>'),write(E),write('</DT>'),
	 list_to_a0(Doc,SDoc),write('<DD>'),write(SDoc),write('</DD>'),writeln('<br>'),
	 fail.
show_edocs(_):-!.
	 
  % mostra todos os Locus (elementos posicionais) que sao certe (obrigatorios)

show_positional(Certe,Locus):-
  show_positional(Certe,Locus,no,Ws),
  show_nonpositional(Certe,Locus,Ws).

show_positional(Certe,[L|Locus],WriteSlash,Ws3):-!,
    (WriteSlash = yes -> write('/'); true),
    (member(L,Certe) -> (write(L),Ws2=yes);true),
    (Ws2=WriteSlash -> true;true), % this unifies Ws2 with WriteSlash if unbound
    show_positional(Certe,Locus,Ws2,Ws3).
show_positional(__Certe,[],Ws,Ws):-!.

show_nonpositional(Certe,Locus,yes):-
    member(C,Certe),
    \+ member(C,Locus),
    write('/'),write(C),write('='),write('...'),    
    fail.
show_nonpositional(Certe,Locus,no):-
    % writeln(show_nonpositional(Certe,Locus,no)),
    member(C,Certe),
    \+ member(C,Locus),
    write(C),write('='),write('...'),    
    fail.    
show_nonpositional(__Certe,__Locus,_):-write('<br>'),!.


show_locus(Certe,Locus):-
   show_locus2(Locus,no,Ws),
   show_nonpositional(Certe,Locus,Ws).
show_locus2([L|Locus],no,W):-
    write(L),
    show_locus2(Locus,yes,W).
show_locus2([L|Locus],yes,W):-
    write('/'),write(L),
    show_locus2(Locus,yes,W).
show_locus2([],W,W):-nl,!.

show_included(I):-show_includes(I,no).
show_includes([I|Rest],no):-
    writelist0(['<A HREF="',I,'.html">',I,'</A>']),
    show_includes(Rest,yes).
show_includes([I|Rest],yes):-
    writelist0([', <A HREF="',I,'.html">',I,'</A>']),
    show_includes(Rest,yes).  
show_includes([],_):-write('<br>'),nl,!.  

show_ancestors(G):-
    anc_of(G,A),
    writelist0ln(['<A HREF="',A,'.html">',A,'</A>']),   
    fail.
show_ancestors(_):-writeln('<br>'),!.
     
%*************************************************************
% show_elements:- show the properties of all elements
%      
%*************************************************************
% %
show_elements:-nl,write('Elements (clioElement): '),listClioElements,
              forall(clioElement(E,ID),
              (writeln('-------------'),
               write(E),tab(2),show_props(ID))).

listClioElements:-clioElement(E,_),
                write(E),tab(1),fail.
listClioElements:-nl,!.
%*************************************************************
% clean_groups. deletes previous group definitions
%*************************************************************
% %
clean_groups(__F):-
   all_groups(List),      
   remove_groups(List),
   init_gensym(cgroup),!.


remove_groups([G|Other]):-
   forall(retract(clioGroup_(G,ID)),del_props(ID)),
   %report([write('** Previous definition of group '),write(G),
   %        write(' deleted.'),nl]),
   remove_groups(Other),!.
remove_groups([]):-!.
%*************************************************************
% clean_elements. deletes previous element definitions
%*************************************************************
% %
clean_elements(__F):-
   all_elements(List),      
   remove_elements(List),
   init_gensym(cel),!.


remove_elements([E|Other]):-
   forall(retract(clioElement_(E,ID)),del_props(ID)),
   %report([write('** Previous definition of element '),write(E),
   %        write(' deleted.'),nl]),
   remove_elements(Other),!.
remove_elements([]):-!.


% vim: filetype=prolog ts=3
% $Date$ 
% $Author$
% $Id$
% $Log: dataDictionary.pl,v $
% Revision 1.2  2006/05/16 10:52:43  Joaquim
% update dos ficheiros prolog
%
% Revision 1.5  2006/03/27 20:52:18  jrc
% New token for documentation of groups.
%
% Revision 1.4  2006/01/07 19:38:25  jrc
% Fized a few quirks with the generation html docs.
%
% Revision 1.3  2004/07/14 04:04:59  joaquim
% Auto makes doc for kleio formats when loading the translator
%
% Revision 1.2  2004/06/18 07:58:57  joaquim
% HTML documentation (show_groups_html). Fixes regarding dynamic predicates
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
% Revision 1.7  2001/01/15 18:25:57  jrc
% CVS headers, line end cleaning and minor changes
%
% ####  History
%    Stable and commented Oct 90.
%    get_group_prop added Aug 97
%    get_element_prop added Nov 2000.
%
%    changed behaviour of fons/source so that original fons/source
%    parameter is preserved across fons/source copying - this is
%    necessary for preserving nested fons/source hierarchies.Sept 2000
%    get_group_prop was made bactrakable. Sept 2000
%*************************************************************