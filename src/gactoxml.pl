/*
	===============================================================================
 	gactoxml.pl Generic translator for Sources that follow

 $Id: gactoxml.pl,v 1.2 2006/05/16 10:52:46 Joaquim Exp $
 $Date: 2006/05/16 10:52:46 $
 $Author: Joaquim $

 History:
 ========
 First version August 1997.

 $Log: gactoxml.pl,v $
 Revision 1.2  2006/05/16 10:52:46  Joaquim
 update dos ficheiros prolog

 Revision 1.6  2006/03/27 20:46:32  jrc
 Added functionality for prefixing ids. Also created hooks for pretty printing of clio files.

 Revision 1.5  2005/12/29 09:59:40  jrc
 process_function in object groups is now Ok.
 Also changed the way by which the mapping of a element of a group was determined.
 Previously the mapping was found directly or by trying to see if the element extended another
 element for which a mapping was known. Now another techinque is used: the mapping is also
 searched in the super class of the group's class.

 Revision 1.4  2005/05/16 11:43:17  jrc
 Change to make_auto_relations2 in order to fix a bug where parents where always assumed as dependent
  groups of their children, which is not the case, most of the parente stuff is
  at the same level as their children. This causes under generation of relations
  between parents and children. It also changes the relation from parent to child
  instead of the opposite, which caused odd expressions since in portugue the child 
  originated relation varies according to the sex of the child "filho"/"filha" while
  the other way around is always the same "pai" or "mae". This had been done in the
  old scheme in April 2002 but got lost somehow in the new scheme.

 Revision 1.3  2005/03/10 14:42:05  joaquim
 snapshot commit for purpose of moving the cvs directory.

 Revision 1.2  2004/05/11 13:42:36  joaquim
 Update gacto.xml from a uncommited version. Recent changes had been lost in the move to Tiago.

 Revision 1.1  2004/04/08 14:45:24  ltiago
 Source code has passed from Joaquim to Tiago.
 Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
 File build.xml, web.xml and velocity.properties were changed

 Revision 1.13  2002/09/22 18:06:45  jrc
 Implements a new scheme for auto relations and
 database mappings. This new scheme removes
 pseudo-groups from the kleio str file.
 gacto.xml is still backwards compatible with the old pseudo-group scheme.
 To use pseudo group auto rels, the old scheme, use set_autorel_mode(1), to
 use the new scheme use set_autorel_mode(2).

 This setting can be included in kleio source files, in the kleio header with the element
 autorels=1|2

 The new class mapping scheme is auto detected by the presence of mapping predicates.

 Revision 1.12  2002/06/23 10:26:05  jrc
 includes prototyping code for new generation relation inferences (not in production).

 Revision 1.11  2002/04/29 15:09:25  jrc
 Added a library for pattern matching relation rules.

 Revision 1.10  2002/04/27 16:45:21  jrc
 Cleaned old unused code. Introduced group order and group level as exported attributes for groups.

 Revision 1.9  2002/04/27 11:45:34  jrc
 This version handles generic auto-relations, no longer tied to persons inside acts. See comments of the do_auto_rels predicate.

 Revision 1.8  2002/04/26 14:46:16  jrc
 Now works by group scope.

 Revision 1.7  2002/04/05 15:44:01  jrc
 Small aletrations in the layout of produced xml. Some comment corrections.

 Revision 1.6  2002/02/24 20:48:19  jrc
 Changed auto relations generated for parents to children, to avoid gender related variations.

 Revision 1.5  2002/02/10 19:47:44  jrc
 Changes to handle correctly multiple entries while exporting to XML. Also XML export now cleans non-ascii characteres.

 Revision 1.4  2001/10/14 18:28:07  jrc
 Line endings changed for DOS.

 Revision 1.3  2001/10/02 06:52:42  jrc
 Quick patch to handle "group-elements" like the ones in obitos. Requires further work.

 Revision 1.2  2001/04/20 11:06:46  jrc
 Class mapping are now output for superr classes up the inheritence tree.
 This avoids missing mapping for classes not included in the source but
 specialized by classes included in the source.

 Revision 1.1.1.1  2001/04/19 23:25:43  jrc
 New Repository.

 Revision 1.1.1.1  2001/04/18 23:34:39  jrc
 CVS repository moved from llinux to MacOSX.

 Revision 1.28  2001/04/08 16:49:03  jrc
 Changed report echoing each act.

 Revision 1.27  2001/03/27 16:12:58  jrc
 Corrected bug in the ID of group object.

 Revision 1.26  2001/03/19 00:52:12  jrc
 Function in act added.

 Revision 1.25  2001/03/15 13:20:36  jrc
 Maintenance release.

 Revision 1.24  2001/03/06 06:35:58  jrc
 Consistent output of relation mapping.

 Revision 1.23  2001/03/05 05:35:01  jrc
 Uses the path_sep predicate to guess the path separator while constructing the xml file name.

 Revision 1.22  2001/03/05 02:55:39  jrc
 Removed some of the debugging code.

 Revision 1.21  2001/03/05 02:43:58  jrc
 Automatic relations framework implemented.

 Revision 1.20  2001/03/03 05:11:23  jrc
 Removed de Relation XML elements. Containement relations are handles inside groups and same as relations are handed as normal historical relations.

 Revision 1.19  2001/03/02 16:53:39  jrc
 Group export format changed to supply "inside" and "class" as elements. This makes insertion in the hierarchical db easier for idb.

 Revision 1.18  2001/02/18 02:31:11  jrc
 	Changed message for end of translation.

 Revision 1.17  2001/02/15 14:54:02  jrc
 Element mapping attribute CLASS now included in XML export

 Revision 1.16  2001/02/14 04:49:18  jrc
 Corrected problems with element mapping. Modified automatic Id generation
 to use just the first 3 letters of class names. Synchronized with the
 first DBCLass version that actually imported something.

 Revision 1.15  2001/02/12 15:15:02  jrc
 Added class attribute in element mappings.

 Revision 1.14  2001/02/11 21:42:16  jrc
 Fixed problems with mappings. Fixed missing id in contained-in auto rels.

 Revision 1.13  2001/02/10 23:36:31  jrc
 Corrected get_ancestor so that top level groups inside kleio group
 report "root" as parent.

 Revision 1.12  2001/02/10 23:22:09  jrc
 First version with complete outpuit of Class and Attribute Mappings.
 Dates now processed in sources.
 New date format on WHEN attribute of Kleio XML element.
 Improved indentation in XML file.

 Revision 1.11  2001/02/08 14:10:54  jrc
 Corrected bug in processing of attribute date.

 Revision 1.10  2001/02/07 14:27:50  jrc
 Interim release. Better compliance to the XML export specs.

 Revision 1.9  2001/01/26 14:06:46  jrc
 Interim version with element and group mapping, and overall xml structure
 as specific.

 Still missing: infered relations, object mapping (no
 RCH class defined for it) and consolidation listings. Output of group to class mappings not yet done.

    Improvements: erase xml output file if errors occur in the translation.
	 Should be triggered completely from command line arguments:
							1 - strufile
							2 _ datafile
							3 - destination for xml file
							4 - directory of strus
							5 - directory of data
							6 - directory for report file.

	Acces to comand line arguments is done with current_prolog_flag(argv,ArgList).
	First member of the list is the prolog interpreter name pl.

 Known issues: iso chars in xml code must cleaned or encoding declared
  (kleio group?), bad implementation of relation-type pseudo-groups.

 Revision 1.8  2001/01/18 21:06:26  jrc
 rch class mapping basically implemented.
 First steps towards normalization by the new xml DTD.

 Revision 1.7  2001/01/14 00:45:26  jrc
 Cleaned version can handle casamentos. More files under source control.
 Minor bugs cleaned in various prolog files.

 Still missing: new xml format not implemented. No default relations.
 No vocabulary reports.

 Revision 1.6  2000/12/20 15:09:51  jrc
 Object processing added.


*/
/*


 the Source-Act-Person model.

 Overview

 This translator provides a bridge between a person oriented
 data model for Micro Historical studies and Kleio as a source
 transcription notation.

 The translator is based on a generic kleio structure definition
 called gacto.str.

 The generic structure describes a source as a document containing
 acts which in turn contain references to persons or objects.

The structure of person related data like attributes and relations with other
 persons is  defined. A basic structure for acts is also provided.

 gacto.str includes some "abstract" groups for persons. There
 are two of these abstract groups called male and female.

 When a stru file is created for a particular source document
 specific person groups are created with names that describe
 the function of persons in the source. These concrete person
 groups are connected to the abstract groups through the fons/source
 parameter. This allows the gacto translator to know the gender
 of the person and thus validate a certain amount of information.

 One of the main aspects of the translation is automatically generate
 kinship relations around the main actor of each act. The fact that
 a person group extends a female actor abstract group allow the
 kleio processor and this translator to understand that a person
 group called husband may follow and generate the appropriate
 kin relatioship.
 */
   /*
   	================================================================================
      db_init - initializes database
   	================================================================================
   */
   /* operators for the inference rules. see inference.pl */
   :-op(230,fx,if).
   :-op(220,xfy,then).
   :-op(210,xfy,and).
   :-op(210,xfy,or).
   /* operators for mappings files. see mappings.pl */
   :-op(230,fx,mapping).
   :-op(220,xfx,to).
   :-op(210,fx,class).
   :-op(209,xfy,super).
   :-op(208,xfy,table).
   :-op(207,xfy,with).
   :-op(206,fx,attributes).
   :-op(204,xfy,and).
   :-op(203,xfy,column).
   :-op(203,xfy,baseclass).
   :-op(203,xfy,coltype).
   :-op(203,xfy,colsize).
   :-op(203,xfy,colprecision).
   :-op(203,xfy,pkey).

   ?-dynamic(group_path/1).
   ?-dynamic(carel/4).
   ?-dynamic(carelnot/2).
   ?-dynamic(used_id/1).



   db_init:-
   	report( [
   		writeln('** Generic Act translation module (XML). $Revision: 1.2 $ $Date: 2006/05/16 10:52:46 $'),
   		writeln('     Joaquim Ramos de Carvalho (joaquim@dei.uc.pt) ')
   		] ),
   	get_value(data_file,D),
   	break_fname(D,Path,_File,Base,_Ext),
   	path_sep(Sep),
   	list_to_a0([Path,Sep,Base],SOURCE),writeln(path-Path-sep-Sep-base-Base),
   	put_value(source,SOURCE),
   	concat(SOURCE,'.xml',XMLFILE),put_value(xmlfile,XMLFILE),
   	open_file_write(XMLFILE),
   	% prolog_to_os_filename(PD,D), file_directory_name(PD,Dir).


   	(concat(_X,'_ids',Base)
   			 -> put_value(clioPP,false) % this is a ids file, no PP necessary   	
   			 ; (
           concat(SOURCE,'.ids',PPFILE),
           open_file_write(PPFILE),
           put_value(clioppfile,PPFILE),
           put_value(clioPP,true)
   			 )
   	),
   
   	xml_write('<?xml version=\'1.0\'?>'),
   	del_props(rclass), % erase cache of class mappings
      del_props(act),    % erase any act context that may have survived last translation
      del_props(person),
      del_props(autorels), %erase context for auto relation generation
   	setgensymbol(lsa,0),setgensymbol(rela,0),setcount(group,1),
      set_autorel_mode(2), % default auto rels mode
      retractall(group_path(_)),   % group_path is the basis for new scheme for generating relations
      retractall(used_id(_)),
   	index_auto_rel_clauses,
   	!.

   /*
   	================================================================================
      db_close - cleans up database
   	================================================================================

     */
   db_close:-
      do_auto_rels,  do_auto_rels2,
   	report([writeln('** Translation finished.')]),
   	(get_value(clioPP,true)->clioPP_close;true),
   	error_count(E),
   	( E = 0 -> change_to_ids;true),
   	xml_write(['</KLEIO>']),
   	xml_nl,
   	xml_close.


   	/* we now renmae the .ids (pretty printed .cli and ids) to .cli and the .cli to .org.
   	  If org already exists we rename .cli to .old. If .old exists we discard it and rename .cli to .old.

   	  The end result is: .cli is the last pretty printed translation with sucess.
   	  .old is the last .cli that was translated
   	  .org is the original .cli that was first translated with success.
   	  */
     change_to_ids:-
      get_value(data_file,D),
      get_value(source,SOURCE),
      concat(SOURCE,'.org',Original),
      concat(SOURCE,'.old',Last),
      concat(SOURCE,'.ids',Ids),
      (exists_file(Last) ->delete_file(Last);true),
      (exists_file(Original)->rename_file(D,Last);rename_file(D,Original)),
      rename_file(Ids,D),!.
    change_to_ids:-
       report([writeln('** Problem renaming files.')]) .


   /*
   	================================================================================
      db_store  - stores the current group
   	================================================================================

   	This is the top level predicate that stores
      the various types of predicates generated from
      the current data structure

      we get the source (fons) parameter of the group
      and use that to determine the group handling code
      In this way the source can have their own vocabulary
      and by stating the source or fons get the right processor
   */

   db_store:-
       clio_path(P),								% necessary for automatic relation processing
       length(P,CurrentLevel),
       (get_prop(group,level,OldLevel) -> true ; OldLevel=0),
      % report([writeln('Checking for auto-rels old level'-OldLevel-' current level '-CurrentLevel)]),
      ( OldLevel @> CurrentLevel ->
            do_auto_rels(CurrentLevel);   % going up in the tree calculate relations
            true),							  % goind down, do nothing
      set_prop(group,level,CurrentLevel),
   	  clio_group(G,ID0), % get the current group
      get_group_id(G,ID0,ID),
   	  group_export(G,ID),
   	  process_same_as(G,ID),			% this was moved here so that any group can have  same_as elements
      get_ancestor(_,AncID),
      add_to_prop(autorels,groups,(G, ID, AncID, CurrentLevel)),    % this is used by automatic relation processing
   	  save_group_path(P,G,ID),
   	  (get_value(clioPP,true)->clioPP(G,ID);true), %if necessary we produce a ids file
      !.

   db_store:-
   	error_out(('** INTERNAL ERROR: Failure: db_store')),!.

   /** this selects which autorel method is used.
       Method 1 uses special kleio groups, introduced in the kleio str file for generating  auto rels
       Method 2 uses a separete rule file -- inference.pl
       */

   set_autorel_mode(1):-put_value(autorelmode,1),!.
   set_autorel_mode(2):-put_value(autorelmode,2),!.
   set_autorel_mode(''):-!.
   set_autorel_mode(O):-	report([
   		writelist0ln(['*** WARNING bad autorel mode =',O])
   		]
   		),!.

   /** this saves the current path with the ids created by this translator modules
       current path for each processed group is stored as a group_path(Path) clause
       that is available latter for auto rel processing */
   save_group_path(Path,GroupName,GroupId):-
      getCurrentPath(CurrentPath),
      length(Path,LPath),
      first_n(CurrentPath,LPath,NewAncPath),
      G =.. [GroupName,GroupId],
      append(NewAncPath,[G],NewPath),
      setCurrentPath(NewPath),
      assert(group_path(NewPath)),!.

   getCurrentPath(CurrentPath) :-
      get_prop(autorels,currentpath,CurrentPath),!.
   getCurrentPath([]):-!.

   setCurrentPath(CurrentPath):-
      set_prop(autorels,currentpath,CurrentPath),!.

   /*
   	================================================================================
      group_export: switches to various groups
   	================================================================================

      New groups, that do not extends the base group types
      will need a clause here to connect to their export module.
   */
   group_export(kleio,_) :- !,
   	report([
   		writeln('========================='),
   		writeln('kleio translation started'),
   		writeln('=========================')
   		]),
   	get_aspects(core,[structure,translator,autorels,obs,prefix],[S,_T,AR,O,SP]),
   	report([
   		writelist0ln(['Structure: '|S]),
   		writelist0ln(['Prefix: '|SP]),
   		writelist0ln(['Autorel: '|SP]),
   		writelist0ln(['Obs: '|O])
   		]
   		),
      ([AutoMode] = AR; AutoMode = ''),
      set_autorel_mode(AutoMode),
   	clio_stru_file(Stru),
   	clio_data_file(Data),
   	list_to_a0(O,OS),
   	list_to_a0(SP,Space),
   	(SP = [_H|_T] -> put_value(useIdPrefix,yes);  put_value(useIdPrefix,no)),
   	put_value(idPrefix,Space),
    now(Year,Month,Day,Hour,Minute,Seconds),
   	Date=Year-Month-Day,
   	Time=Hour:Minute:Seconds,
   	xml_write(['<KLEIO STRUCTURE="',Stru,'" SOURCE="',Data,'" TRANSLATOR="gactoxml2.str" WHEN="',Date,' ',Time,'" OBS="',OS,'" SPACE="',Space,'">']),
   	xml_nl.

   group_export(Source,ID):-
   	group_derived(Source,'historical-source'),
   	historical_source_export(Source,ID),!.

   group_export(Act,ID) :-
   	group_derived(Act,'historical-act'),
   	historical_act_export(Act,ID),!.

   group_export(Person,ID) :-
   	group_derived(Person,person),
   	person_export(Person,ID),!.

   group_export(Object,ID) :-
   	group_derived(Object,object),
   	object_export(Object,ID),!.

   group_export(Attribute,ID) :-
   	group_derived(Attribute,attribute),%writeln('attributed detected'),
   	attribute_export(Attribute,ID),!.

   group_export(Relation,ID) :-
   	group_derived(Relation,relation),
   	relation_export(Relation,ID),!.

   group_export(End,ID) :-
   	group_derived(End,end),
      %report([writeln('**processing end group')]),
   	process_end(End,ID),!.

   group_export(GE,ID) :-
      group_derived(GE,'group-element'),
      process_group_element(GE,ID),!.

   group_derived(G,G).
   group_derived(G,S) :- clio_extends(G,S).



   /*
   	================================================================================
      Group processors.
   	================================================================================

      Predicates in this area are called by the group_export
      switchboard. They recieve the actual group name used in
      the clio source file  and the auto-generated clio ID
      (which can be used or not).


    */

   historical_source_export(Source,Id):-
      do_auto_rels2,
   	report([writelist0ln(['** Processing source ',Source,'$',Id])]),
   	 (get_date(Date1) -> Date=Date1 ;  % we handle long dates and day/month/year dates
   		(get_y_m_d(Date2) -> Date=Date2; Date=0)),
   	 set_prop(source,date,Date),
   	clio_data_file(Data),
   	 group_to_xml(Source,Id,[date([Date],[],[]),kleiofile([Data],[],[])]),
   	 put_value(current_source,Source-Id),
   	!.

   /*

   	historical_act_export: exports to xml the info on an act group.
      ======================
   		Keeps in memory the following information, as properties of the
   			atom 'act':

   				id: id of the current act
   				group: the actual group that constitutes the act
   				date: the date of the act

   	The date of the act can be registered either as the attribute date or as
   	year, month,day in separate attributes.

   */
   historical_act_export(Group,Id):-
      do_auto_rels2,
      report([writelist0ln(['** Processing act ',Group,'$',Id])]),
      del_props(act),
      del_props(person),
      set_prop(act,id,Id),
      set_prop(act,group,Group),
      (get_date(Date1) -> Date = Date1 ;
               (get_y_m_d(Date2) -> Date = Date2 ;
                        (get_prop(source,date,Date3) -> Date = Date3 ;
                              (Date = 0,
                                    report([writelist0ln(['* Warning: No date in act ',
                                                         Group,'(',Id,')'
                                                         ]
                                                      )
                                          ])
                              )
                        )
                  )
         ),
      set_prop(act,date,Date),
      setgensymbol(per,0), % we reset the counter so that auto ids are local to the act
      setgensymbol(obj,0),
      setgensymbol(rel,0),
      setgensymbol(att,0),
      group_to_xml(Group,Id,[date([Date],[],[]),type([Group],[],[])]),
      !.

   person_export(G,ID):-
   	set_prop(person,id,ID),
   	infer_sex(G,S),
   	belement_aspect(core,name,Name),
      set_prop(person,name,Name),
      set_prop(person,sex,S),
      set_prop(person,group,G),
      clio_elements(Els),
      (member(sex,Els) -> SexElement = []; SexElement=[sex([S],[],[])]),
   	(belement_aspect(core,id,[]) -> IdElement=[id([ID],[],[])];IdElement = [] ),
   	append(SexElement,IdElement, ExtraElements ),
   	group_to_xml(G,ID,ExtraElements),
   	process_function_in_act(G,ID),
      !.

   object_export(G,ID) :-
   	(belement_aspect(core,id,[]) ->  IdElement=[id([ID],[],[])]; IdElement=[]),
   	group_to_xml(G,ID,IdElement),
   	process_function_in_act(G,ID),
   	report(writeln(G-ID-'** was stored with function in ACT verify act')),
      !.

   attribute_export(G,ID) :-
   	get_ancestor(_Anc,AncId),
   	(get_date(Date1) -> Date = Date1 ; get_prop(act,date,Date)),
      group_to_xml(G,ID,[id([ID],[],[]),entity([AncId],[],[]),date([Date],[],[])]).


   relation_export(G,ID) :-
   	get_ancestor(_Anc,AncId),
   	(get_date(Date1) -> Date = Date1 ; get_prop(act,date,Date)),
   	group_to_xml(G,ID,[id([ID],[],[]),origin([AncId],[],[]),date([Date],[],[])]).

   /* Ending an act */

   process_end(_,_):- do_auto_rels,do_auto_rels2,!.

   /* todo process group as element of previous act /group will only have an element called value */
   process_group_element(GE,ID):-
   	  group_to_xml(GE,ID,[]).

   /*
   	================================================================
   	auxiliary functions for group processors
   	================================================================
   */
   /*
     process_same_as(Group,Id)
     =========================

   		tests if an element named same_as (or a specialization of such an element)
   		exists in the current group. If so , generates a relation between the current group and
   		the group whose id is the value of the same-as element.
   		This is used both in person and in objects
   */
   process_same_as(_Group,Id):-
   	belement_aspect(core,same_as,[SameId]),
           writelistln(['   ** processing_same_as for ',Id]),
   	(is_list(SameId) -> list_to_a0(SameId,SID0); SID0 = SameId),
   	check_id_prefix(SID0,SID),
   	gensymbol(rela,Rid),
   	get_ancestor(_anc,AncID),
           get_prop(act,date,Date),
   	rch_class(relation,Class,Super,Table,Mapping),
   	ensure_class(relation,Class,Super,Table,Mapping),
      inccount(group,GroupNumber),
      clio_path(P),
      length(P,CurrentLevel),
      ThisLevel is CurrentLevel+1,
      get_prop(line,number,N),
      xml_nl,
   	xml_write(['<RELATION ID="',AncID-Rid,'" ORG="',Id,'" DEST="',SID,'" TYPE="META" VALUE="same_as"/>']),
   	xml_nl,
      xml_write(['<GROUP ID="',AncID-Rid,'" NAME="relation"  ORDER="',GroupNumber,'" LEVEL="',ThisLevel,'"  CLASS="relation" LINE="',N,'">']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="line" CLASS="line"><core>',N,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="groupname" CLASS="groupname"><core>relation</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="inside" CLASS="inside"><core>',AncID,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="class" CLASS="class"><core>relation</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="order" CLASS="order"><core>',GroupNumber,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="level" CLASS="level"><core>',ThisLevel,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="type" CLASS="type"><core>identification</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="value" CLASS="value"><core>same as</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="destname" CLASS="destname"><core>n/a</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="origin" CLASS="origin"><core>',Id,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="destination" CLASS="destination"><core>',SID,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="id" CLASS="id"><core>',AncID-Rid,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="date" CLASS="date"><core>',Date,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['</GROUP>']),
      !.

   process_same_as(_,_).

   process_function_in_act(Group,Id):-
   	gensymbol(rela,Rid),
   	get_ancestor(_anc,AncID),
      get_prop(act,date,Date),
   	get_prop(act,group,ActGroup),
   	get_prop(act,id,ActId),
   	rch_class(relation,Class,Super,Table,Mapping),
   	ensure_class(relation,Class,Super,Table,Mapping),xml_nl,
      inccount(group,GroupNumber),
      clio_path(P),
      length(P,CurrentLevel),
      get_prop(line,number,N),
      ThisLevel is CurrentLevel+1,
      xml_write(['<GROUP ID="',AncID-Rid,'" NAME="relation"  ORDER="',GroupNumber,'" LEVEL="',ThisLevel,'" CLASS="relation" LINE="',N,'">']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="line" CLASS="line"><core>',N,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="groupname" CLASS="groupname"><core>relation</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="inside" CLASS="inside"><core>',Id,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="class" CLASS="class"><core>relation</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="order" CLASS="order"><core>',GroupNumber,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="level" CLASS="level"><core>',ThisLevel,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="type" CLASS="type"><core>function-in-act</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="value" CLASS="value"><core>',Group,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="destname" CLASS="destname"><core>',ActGroup,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="origin" CLASS="origin"><core>',Id,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="destination" CLASS="destination"><core>',ActId,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="id" CLASS="id"><core>',AncID-Rid,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="date" CLASS="date"><core>',Date,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['</GROUP>']),
      !.
   process_function_in_act(_,_).
   /*
   	infer_sex(Group,Sex)
      ====================


   */
   infer_sex(_G,S):-
   		  belement_aspect(core,sex,[S]).

   infer_sex(G,S):-
   		  clio_extends(G,Class),
   		  get_sex(Class,S),!.
   infer_sex(G,'?'):-
   	error_out(['** could not infer sex for'-G]).

   get_sex(male,m):-!.
   get_sex(female,f):-!.
   /*

   		get_date: get the date if it was entered as the element "date"
   		========
   */
   get_date(DATE):-
   		  belement_aspect(core,date,[DATE]),
   		  number(DATE).



   /*
   		get_y_m_d: get the date if it was entered with three element (day/month/year).

   */

   get_y_m_d(DATE):-
   		  belement_aspect(core,day,[DAY]),
   		  belement_aspect(core,month,[MONTH]),
   		  belement_aspect(core,year,[YEAR]),
   		  number(DAY),number(MONTH),number(YEAR),
   		  DATE is YEAR*10000+MONTH*100+DAY.


   /*

   	get_group_id:  Rules for determinig the id of a group
     ===============

   */


   % if the element id exists, take it as the id
   get_group_id(Group,_BuiltinID,Id):-
   		get_aspect(core,id,ID),ID \= [],!,
   		list_to_a0(ID,Id0),
   		check_id_prefix(Id0,Id),
   		(used_id(Id)->error_out(['** ID already used in this file: ',Id]);asserta(used_id(Id))),
      put_value(Group,Id),!.   % store the Id for latter reference

   
   % no explicit id. Get the id of the ancestor and concatenate

   get_group_id(Group,_BuiltinID,Id):-
      	get_ancestor(_Anc,Aid),!,
   	 	clio_bclass(Group,Class),% by using the base class we allow group processors to reset the counters
   		sub_atom(Class,0,3,_,Seed),
   		  repeat,
        gensymbol(Seed,Gid),
        (atom(Aid)->AAid=Aid;term_to_atom(Aid,AAid)),
        list_to_a0([AAid,'-',Gid],Id), % we assumed that the ancestor's id is prefixed so we do not check
        put_value(Group,Id),
        \+ used_id(Id),
        assert(used_id(Id)),!.

   get_group_id(Group,Id,Id0) :- check_id_prefix(Id0,Id),put_value(Group,Id),\+ used_id(Id),asset(used_id(Id)),!. % If everything fails accept the kleio auto id

   /*
      cehck if there is a name space prefix to ad to the id.
      these prefixes are registered in "prefix" element of the kleio group.
     */

   check_id_prefix(ID,PID):-
    (get_value(useIdPrefix,yes)->
          (get_value(idPrefix,P), concat(P,'-',P2),concat(P2,ID,PID));
          PID=ID).
   check_id_prefix(I,I).
   
   remove_id_prefix(ID,NID):-
	(get_value(useIdPrefix,yes)->
		(get_value(idPrefix,P),
		concat(P,'-',P2),
		concat(P2,NID,ID))
		),!.
	remove_id_prefix(I,I):-!.

   /*
   	Get the ancestor ID
      ===================
   	Like clio_anc/2 but delivers the true ID (not the Kleio id)
   */
   get_ancestor(Anc,Aid) :-
   	clio_group(G,_), 			% get the current group
   	( clio_isdoc(G) -> (Anc = '', Aid = '');
   							 (clio_anc(Anc,_),
   							 (Anc = kleio -> Aid = 'root' ; get_value(Anc,Aid)))
      ),%writeln(clio_anc(Anc,_)),
   	!.

   /*

   	link_ancestor: Generate a link between the ancestor and the current group
      ===============
      DEPRECATED : now intergroup relations are handled directly in Group, by the elemet "inside".
   */
   link_ancestor(G,ID):-!,
   		gensymbol(rela,Rid),
   	   get_ancestor(_AGroup,Gid),			%writeln( get_ancestor(AGroup,Gid)),% get the ancestor
   	   xml_write(['<RELATION ID="',Gid-Rid,'" ORG="',ID,'" DEST="',Gid,'" TYPE="contained-in" VALUE="',G,'"/>']),
   		xml_nl.



   /*

      Automatic relation handling.

      From the gacto.str file:
      note Usage
      note 		Create a group with auto-relation in its source/fons parameter.
      note 		In the part parameter put the two group names for the persons(or any other entities)
      note			be automatically related.
      note		In the guaranteed parameter put the relation type for the automatic
      note			relation followed by the relation value.
      note
      note  Example:
      note 		part name=a_couple; source=auto-relation
      note          part=child,father;
      note          guaranteed=kinship,son-of
      note
      note  The translator will treat:
      note
      note     child$john/id=p001
      note       father$james/id=p003
      note
      note  generating:
      note      relation$kinship/son-of/origin=p001/destination=p003
      note
      note  Scope rules:
      note      Automatic relations are only inferred between groups that have one of two type of positional
      note      relationship: one group must be the ancestor of the other; the two groups must have a common direct ancestor
      note			in other words: they must be either "container-contained" or "inside same direct container".
      note	NOTE: when the groups involved in a auto-rel have a positional "container-conatined" relationship the rule
      note        must expressed from "container" to "contained".
      note
      note	if you have:
      note		part name=child; part=father
      note
      note   then the rule should be:
      note 		part name=a_father-son; source=auto-relation
      note          part=child,father; <-- note the order is from container to contained.
      note          guaranteed=kinship,son-of
      note
   */
   do_auto_rels:- get_value(autorelmode,M), M \= 1, !.
   do_auto_rels:-report([writeln('*** do_auto_rels entering at level 0')]),do_auto_rels(0).
   do_auto_rels:-!.
   do_auto_rels(_):-get_value(autorelmode,M), M \= 1, !.
   do_auto_rels(CutLevel):-  % this infers relations in the current act.
       %report([writeln('*** do_auto_rels entering at level '-CutLevel)]),
       get_prop(autorels,groups, Groups),
       member((Group, ID, Anc, Level),Groups),
       Level @>= CutLevel,
      % report([writeln('  ** Infering relations for '-Group-ID)]),
       member((Group2, ID2, Anc2, Level2),Groups),
       ID \= ID2,
       Level2 @> CutLevel,
       check_arel_scope((Group, ID, Anc),(Group2, ID2, Anc2)),
      % report([writeln('    ** Found '-Group2-ID2-' in scope')]),
       autor_rel_for_group(Group,Group2,Type,Value),
      % report([writeln('    ** autorel '-Type-Value-Group2-ID2-' found')]),
       export_auto_rel((Group,ID),(Group2,ID2),Type,Value),
       fail.
   do_auto_rels(CutLevel):-
       get_prop(autorels,groups, Groups),
       remove_from_autorels_groups(CutLevel,Groups,NewGroups),
       set_prop(autorels,groups,NewGroups),
       !.
   do_auto_rels(_) :-!.   % this catches the one when no person was processed yet

   remove_from_autorels_groups(_,[],[]):-!.

   remove_from_autorels_groups(CutLevel,[(_Group,_Id,_Anc,Level)|Rest],Final) :-
      Level @> CutLevel,  %report([writeln('  **removing '-(Group,Id,Anc,Level))]),
      remove_from_autorels_groups(CutLevel,Rest,Final).

   remove_from_autorels_groups(CutLevel,[(_Group,Id,Anc,Level)|Rest],[(_Group,Id,Anc,Level)| Final]) :-
      remove_from_autorels_groups(CutLevel,Rest,Final).

   /*
       the scope rules for automatic relations are:
            The related persons must have the same ancestor
            or one must be the ancestor of the other
   */
   check_arel_scope((_P1,_,ANC),(_P2,_,ANC)):-!.
   check_arel_scope((_P1,ID,_),(_P2,_,ID)):-!.
   check_arel_scope((_P1,_,ID),(_P2,ID,_)):-!.
   %check_arel_scope(P1,P2) :- writeln('** not in scope '-P1-P2),!,fail.

   /*
      This is an experiment in a new model for auto relations:

   Relations are defined based on paths
   A path is a list of groupName(ID) | sequence(C) | group(Name,ID) |extends(Class,ID) | clause(C).
   ID is usually a variable that is bound to actual IDs when the match is done
   sequence(C) matches a sequence of groups including a empty one
   group(Name,ID) matches Name(ID) it is usefull to extract a Group name.
   extends(Class,ID) will match any Name(ID) in Name is a group that extends Class.
   clause(C) will call the C predicate in Prolog

   example (using the sequence(P) place holder to signify the same context:
   group_path(P),
   path_matching(P,[sequence(Prefix),n(N),mulher(M)]),
   writeln(relation/kin/husband/N/M),fail.

   This generates functional relational for heads of households.
   group_path(PX),
   path_matching(PX,[kleio(K),fonte(F),rol(R),fogo(FG),n(N)]),
   writeln(relation/function/head-of-household/N/FG),fail.


   This generates functional relation for every direct member of a houshold.
   group_path(PX),
   path_matching(PX,[kleio(K),fonte(F),rol(R),fogo(FG),group(Func,N)]),writeln(relation-Func-N-FG),fail.

   This generates functional relation for every direct or indirect member of houshold
   group_path(X),
   path_matching(X,[kleio(_),fonte(F),rol(R),fogo(FG),sequence(C),group(Name,I),clause(Name \= ls), clause( Name \= rel)]),
   writeln(relation/Name/I/FG),fail.
   */

   /** this is the new experimental mechanism for inferencing relations.
   It is done at act scope level.
   */
   do_auto_rels2:-get_value(autorelmode,N), N \= 2,!.
   do_auto_rels2:-
      report([writeln('**** auto rels 2 STARTED.')]),
      apply_inference_rules,
      %clean_paths,
      !.
   do_auto_rels2:-
      report([writeln('**** auto rels 2 FINISHED.')]),!.

   clean_paths:-
      group_path(P),
      clean_paths(P).

   clean_paths(Pattern):-
      %report([writeln('***** auto rels 2 clean_paths for'-Pattern)]),
      group_path(P),
      path_matching(P,Pattern),
     % report([writeln('***** auto rels 2 removing path '-P)]),
      retract(group_path(P)),
      fail.
   clean_paths(_):-!.

   apply_inference_rules:-
      if Condition then Action,
      Action \= newscope,
      condition_test(Condition),
      %report([writeln('***** auto rels 2 true condition:'-Condition)]),
      do_actions(Action),
      fail.

   apply_inference_rules:-
      if Condition then newscope,
      %report([writeln('***** auto rels 2 new scope testing for:'-Condition)]),
      condition_test(Condition),
      do_action(newscope),
      fail.

   apply_inference_rules.

   condition_test(ConditionA and ConditionB):-
       condition_test(ConditionA),
       condition_test(ConditionB).

   condition_test(ConditionA or ConditionB):-
       condition_test(ConditionA);condition_test(ConditionB).

   condition_test(Condition):-
       group_path(P),
       path_matching(P,Condition).

   do_actions(ActionA and ActionB) :-
       do_actions(ActionA),do_actions(ActionB),!.

   do_actions(ActionA) :-
       do_action(ActionA),!.

   do_action(relation(Type,Value,Origin,Destination)):-
       export_auto_rel((na,Origin),(na,Destination),Type,Value),!.


   do_action(attribute(ID,Type,Value)):-
   	export_auto_attribute(ID,Type,Value),!.

   do_action(newscope):-
      report([writeln('***** auto rels 2 new scope')]),
      clean_paths([kleio(_),sequence(_S)]).


   path_matching(Path,Pattern):-
      path_matching(Path,Pattern,[],[]).

   path_matching([],[],[],[]):-!.

   path_matching(Path,[sequence(StartPath)|MorePattern], FinalPath, FinalPattern):-
      append(StartPath,MorePath,Path),
      path_matching(MorePath,MorePattern, FinalPath,FinalPattern),!.

   path_matching([Group|MorePath],[group(GroupName,ID)|MorePattern],FinalPath,FinalPattern):-
      Group =.. [GroupName,ID],!,
      path_matching(MorePath,MorePattern,FinalPath,FinalPattern).

   path_matching([Group|MorePath],[extends(BaseGroup,ID)|MorePattern],FinalPath,FinalPattern):-
      Group =.. [GroupName,ID],!,
      clio_extends(GroupName,BaseGroup),
      path_matching(MorePath,MorePattern,FinalPath,FinalPattern).

   path_matching(Path,[clause(C)|MorePattern],FinalPath,FinalPattern) :-
      call(C),
      path_matching(Path,MorePattern,FinalPath,FinalPattern).

   path_matching([Group|MorePath],[Group|MorePattern],FinalPath,FinalPattern):-!,
      path_matching(MorePath,MorePattern,FinalPath,FinalPattern).


   export_auto_rel((_Origin,OriginID),(_Destination,DestinationID),Type,Value) :-!,
   	rch_class(relation,Class,Super,Table,Mapping),
   	ensure_class(relation,Class,Super,Table,Mapping),
   	gensymbol(rela,Rid),
   	get_prop(act,id,AncID),
      get_prop(act,date,Date),xml_nl,
      inccount(group,GroupNumber),
      clio_path(P),
      length(P,CurrentLevel),
      ThisLevel is CurrentLevel+1,
      get_prop(line,number,N),
      xml_write(['<GROUP ID="',AncID-Rid,'" NAME="relation"  ORDER="',GroupNumber,'" LEVEL="',ThisLevel,'" CLASS="relation" LINE="',N,'">']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="line" CLASS="line"><core>',N,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="groupname" CLASS="groupname"><core>relation</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="inside" CLASS="inside"><core>',AncID,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="class" CLASS="class"><core>relation</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="order" CLASS="order"><core>',GroupNumber,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="level" CLASS="level"><core>',ThisLevel,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="type" CLASS="type"><core>',Type,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="value" CLASS="value"><core>',Value,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="destname" CLASS="destname"><core>n/a</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="origin" CLASS="origin"><core>',OriginID,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="destination" CLASS="destination"><core>',DestinationID,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="id" CLASS="id"><core>',AncID-Rid,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="date" CLASS="date"><core>',Date,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['</GROUP>']),
      report([writelist0ln(['   ** auto rel: from ',OriginID,
                            ' to ',DestinationID,' ',Type,'/',Value])]),
      !.

   export_auto_attribute(Entity,Type,Value) :- !,
   	rch_class(ls,Class,Super,Table,Mapping),
   	ensure_class(ls,Class,Super,Table,Mapping),
   	gensymbol(atra,Rid),
   	get_prop(act,id,AncID),
      get_prop(act,date,Date),xml_nl,
      inccount(group,GroupNumber),
      clio_path(P),
      length(P,CurrentLevel),
      ThisLevel is CurrentLevel+1,
      get_prop(line,number,N),
      xml_write(['<GROUP ID="',AncID-Rid,'" NAME="ls"  ORDER="',GroupNumber,'" LEVEL="',ThisLevel,'" CLASS="attribute" LINE="',N,'">']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="line" CLASS="line"><core>',N,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="groupname" CLASS="groupname"><core>ls</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="inside" CLASS="inside"><core>',AncID,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="class" CLASS="class"><core>attribute</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="order" CLASS="order"><core>',GroupNumber,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="level" CLASS="level"><core>',ThisLevel,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="type" CLASS="type"><core>',Type,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="value" CLASS="value"><core>',Value,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="entity" CLASS="entity"><core>',Entity,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="id" CLASS="id"><core>',AncID-Rid,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="date" CLASS="date"><core>',Date,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['</GROUP>']),
      report([writelist0ln(['   ** Automatic attribute: ',Entity,'/',Type,'/',Value])]),
      !.



   /* The Auto relation clauses as stored by the Kleio structure parse are not in a convenient format
      for rapid retrieval. We load them in another form to speed retreival during processing
      */
   index_auto_rel_clauses:-
      retractall(carel(_,_,_,_)),
      writeln('** Indexing automatic relation clauses'),
      clio_super('auto-relation',ARGroup),
      clio_group_param(ARGroup,pars,[Group,Group2]),
      clio_group_param(ARGroup,certe,[Type,Value]),
      assert(carel(Group,Group2,Type,Value),true),fail.

   index_auto_rel_clauses:-
      writeln('** Automatic relation clauses indexed.'),!.


   auto_relation(GroupA,GroupB,Type,Value) :-
      clause(carel(GroupA,GroupB,Type,Value),true).

   /*  This gives auto relation information for a given group (Group is taken as origin of the auto relation)
        Backtraks on all possibilities.

        Use the auto relation clauses in index format. Requires that index_auto_rel_clauses was executed first
   */
   autor_rel_for_group(Group,Group2,Type,Value) :-     % fetchs an auto rel for a given group as origin
      auto_relation(Group, Group2,Type,Value).

   autor_rel_for_group(Group, Group2,Type,Value) :-     % allow for inheritance in the first group
      clio_extends(Group,Class),
      auto_relation(Class, Group2,Type,Value).

   autor_rel_for_group(Group, Group2,Type,Value) :-     % allow for inheritance in the second group
      clio_extends(Group2,Class),
      auto_relation(Group,Class,Type,Value).

   autor_rel_for_group(Group, Group2,Type,Value) :-     % allow for inheritance in both. tuned for first group instantiated
      clio_extends(Group,Class),
      clio_extends(Group2,Class2),
      auto_relation(Class,Class2,Type,Value).


   /* caching auto rel clauses NOT IN USE */

   cache_arel(Person,Person2,Type,Value):-
      clause(carel(Person,Person2,Type,Value),true),!.
   cache_arel(Person,Person2,_Type,_Value):-
      clause(carelnot(Person,Person2),true),!,fail.
   cache_arel(Person,Person2,Type,Value):-
      autor_rel_for_group(Person,Person2,Type,Value),!,
      assert(carel(Person,Person2,Type,Value)).
   cache_arel(Person,Person2,_,_) :-!,
      assert(carelnot(Person,Person2)),fail.
   /*

            This is a utiltiy to generate auto-relation definitions for families for the gacto.str file.
            Usage: make_auto_relation_clauses(Actor,Sex,Levels).

            For different languages check the prefix while calling make_parent_clause and check inside that predicate
            the chars used for each sex and the kinship terms.
   */
   make_auto_relation_clauses(_Seed,_,0):-!.
   make_auto_relation_clauses(Seed,Sex,Level) :-
      make_begin_note(Seed,Level),
      make_parent_clause(Seed,Sex,p,Father),
      make_parent_clause(Seed,Sex,m,Mother),
      make_couple_clause(Seed,Father,Mother),
      Level2 is Level - 1,
      make_auto_relation_clauses(Father,m,Level2),
      make_auto_relation_clauses(Mother,f,Level2),
      make_end_note(Seed,Level),
      !.

   make_begin_note(Seed,Level):-
      nl,
      writelist0ln(['NOTE BEGIN Auto-relation ',Seed,' (levels ',Level,') ']),
      writelist0ln(['NOTE -----------------------------------------']),nl,!.
   make_end_note(Seed,Level):-nl,
      writelist0ln(['NOTE END Auto-relation ',Seed,' (levels ',Level,')']),
      writelist0ln(['NOTE -----------------------------------------']),nl,!.
   make_parent_clause(Seed,_Sex,Prefix,Parent):-
      concat(Prefix,Seed,Parent),
      ( Prefix = p -> Relation=pai; Relation=mae),
      writelist0ln(['part name=arel-',Seed,'-',Parent,'; source=auto-relation;']),
      writelist0ln(['    part=',Parent,', ',Seed,'; guaranteed=parentesco,',Relation]),!.

   make_couple_clause(Seed,Husband,Wife) :-
      writelist0ln(['part name=arel-',Seed,'-parents; source=auto-relation;']),
      writelist0ln(['    part=',Husband,', ',Wife,'; guaranteed=parentesco,marido']),!.

   /*
      make_auto_relation_clauses2: - same as before but in the new format
   */
   make_auto_relation_clauses2(_Seed,_,0):-!.
   make_auto_relation_clauses2(Seed,Sex,Level) :-
      make_begin_note2(Seed,Level),
      make_parent_clause2(Seed,Sex,p,Father),
      make_parent_clause2(Seed,Sex,m,Mother),
      make_couple_clause2(Seed,Father,Mother),
      Level2 is Level - 1,
      make_auto_relation_clauses2(Father,m,Level2),
      make_auto_relation_clauses2(Mother,f,Level2),
      make_end_note2(Seed,Level),
      !.

   make_begin_note2(Seed,Level):-
      nl,
      writelist0ln(['/* BEGIN Auto-relation ',Seed,' (levels ',Level,') v2.1 16-5-2005']),
      writelist0ln([' ----------------------------------------- */']),nl,!.

  make_end_note2(Seed,Level):-nl,
      writelist0ln(['/* END Auto-relation ',Seed,' (levels ',Level,') v2.1 16-5-2005']),
      writelist0ln([' ----------------------------------------- */']),nl,!.

  make_parent_clause2(Seed,_Sex,Prefix,Parent):-
      concat(Prefix,Seed,Parent),
      ( Prefix = p -> Relation=pai; Relation=mae),
      writelist0ln(['if [sequence(_),',Seed,'(Son),',Parent,'(Parent)]']),
      writelist0ln(['then relation(parentesco,',Relation,',Parent,Son).']),
      writelist0ln(['if [sequence(Path),',Seed,'(Son)] and [sequence(Path),',Parent,'(Parent)] ']),
      writelist0ln(['then relation(parentesco,',Relation,',Parent,Son).']),
      !.

   make_couple_clause2(_Seed,Husband,Wife) :-
      writelist0ln(['if [sequence(Path),',Husband,'(Husband)] and [sequence(Path),',Wife,'(Wife)] ']),
      writelist0ln(['then relation(parentesco,marido,Husband,Wife).']),!.

   /*
      ============================================================================
      Metadata access: predicates that use the meta contained in pseudo-groups
   							in the inheritance hierarchy, and rch-mappings.
      ============================================================================


    	rch_class(Group,Class,Super,Table,Mapping)
      =================================
   				Determines de RCH class for a kleio group.
   				Along with the class the rch super class, and table.
   				Mapping is the name of the mapping pseudo-group
   				This is how we know where kleio groups end up in the rch database.


   */

   rch_class(Group,Class,Super,Table,na):-
      isNewMappingMode,
      %writeln('** using new style mapping'),
      groupToClass(Group,Class,Super,Table).

   rch_class(Group,Class,Super,Table,Mapping):-
      %writeln('** using old style mapping'),
      rch_class1(Group,Class,Super,Table,Mapping),!.

   /** detects the new mapping mode that uses mappings and class operators **/
   isNewMappingMode:-
    clause(mapping _ to class _, _),!.

   /** DEPRECATED **/
   rch_class1(Group,Class,Super,Table,Mapping) :-
   		  clio_super('rch-mapping',Mapping),
   		  clio_group_param(Mapping,pars,[Group,Class,Super,Table]),!.
   rch_class1(Group,Class,Super,Table,Mapping) :-
   		  clio_extends(Group,SGroup),
   		  rch_class(SGroup,Class,Super,Table,Mapping),!.
   rch_class1(_,undef,undef,undef,undef) :-!.

   /* this is the new mapping scheme based in prolog files instead of kleio pseudo-groups
   */
   groupToClass(Group,Class,Super,Table) :-
            mapping Group to class Class,
            class Class super Super table Table with attributes _,!.
   groupToClass(Group,Class,Super,Table) :-
   		  clio_extends(Group,SGroup),
   		  groupToClass(SGroup,Class,Super,Table),!.
   groupToClass(_,undef,undef,undef) :-!.

   class_attributes(Class,Attributes):-
            class Class super _ table _ with attributes Attributes,!.

   /** returns the Attribute in a class that corresponds to an element in a group */
   elementMapping(GroupClass,Element,Attr):-
       class_attributes(GroupClass,Attributes),
       rch_get_attribute(Element,Attributes,Attr).
   elementMapping(GroupClass,Element,Attr):-
       class_attributes(GroupClass,Attributes),
       clio_element_extends(Element,SElement),
       rch_get_attribute(SElement,Attributes,Attr).
   elementMapping(GroupClass,Element,Attr):-
       rch_class(GroupClass,_,Super,_,_),
       class_attributes(Super,Attributes),
       rch_get_attribute(Element,Attributes,Attr).

   /** the class of an element in a group */
   elementClass(GroupClass,Element,Class):-
      elementMapping(GroupClass,Element,Attr),
      atr_select(baseclass,Attr,Class),!.

   rch_get_attribute(Name, Attr and _, Attr):-
      atr_select(name,Attr,Name) .
   rch_get_attribute(Name, _Attr and MoreAttr, Attr2):-
      rch_get_attribute(Name, MoreAttr,Attr2).
   rch_get_attribute(Name,Attr,Attr):-
   	atr_select(name,Attr,Name) .

   atr_select(name,Attribute column _ baseclass _ coltype _ colsize _ colprecision _ pkey _,Attribute):-!.
   atr_select(column,_ column Col baseclass _ coltype _ colsize _ colprecision _ pkey _, Col):-!.
   atr_select(baseclass,_ column _ baseclass BaseClass coltype _ colsize _ colprecision _ pkey _,BaseClass):-!.
   atr_select(coltype, _ column _ baseclass _ coltype Coltype colsize _ colprecision _ pkey _, Coltype):-!.
   atr_select(colsize, _ column _ baseclass _ coltype _ colsize ColSize colprecision _ pkey _, ColSize):-!.
   atr_select(colprecision, _ column _ baseclass _ coltype _ colsize _ colprecision Prec pkey _,Prec):-!.
   atr_select(pkey, _ column _ baseclass _ coltype _ colsize _ colprecision _ pkey PK,PK):-!.

   /* this converts old style mappings, included in the str files, to the new prolog sysntax mappins
       The mappings are output to the default console, where they can ben saved and included in a prolog file
       see mappings.pl
       */


   output_mappings:-
   		  clio_super('rch-mapping',Mapping),
   		  clio_group_param(Mapping,pars,[Group,Class,Super,Table]),
           nl,
           clio_group_param(Mapping,locus,Pkeys),
           write('mapping '),writeq(Group),write(' to class '),writeq(Class),write('.'),nl,
           clio_group_param(Mapping,certe,[FirstAtr|Attributes]),
           reverse([FirstAtr|Attributes],[Last|_]),
           write('class '),writeq(Class),write(' super '),writeq(Super),write(' table '),writeq(Table),nl,
            writeln('   with attributes'),
           member(Atr,[FirstAtr|Attributes]), %writeln('** '-Atr),
          (Atr = FirstAtr -> true; writeln('     and')),
           (member_nth(Atr,Pkeys,N) -> PK = N ; PK=0),
           rch_element_class(Atr,Base,Column,Type,Length,Precision,_EMapping),
           name(Precision,[_|PChars]),name(Prec,PChars),
           name(Length,[_|LChars]),name(Len,LChars),
           % writeln(clio_group_param(EMapping,pars,[Atr,Column,Type,Len,Prec])),
           (Atr = Last -> End = '.' ; End = ''),
           write('        '),
           writeq(Atr),
           write(' column '),
           writeq(Column),
           write(' baseclass '),
           writeq(Base),
           write(' coltype '),
           writeq(Type),
           writelistln([' colsize',Len,'colprecision',Prec,'pkey',PK,End]),
          fail.
     output_mappings:-!.

   %ensure class: outputs a class mapping element

   ensure_class(_,entity,_,_,_):-!.     % entity mapping is builtin
   ensure_class(_GroupName,RClass,Super,_Table,_Mapping):-
         ensure_super(Super),
   		get_prop(rclass,RClass,exported),
                   !.

   ensure_class(G,C,S,T,Mapping) :-
         ensure_super(S),
   		(isNewMappingMode -> rclass2_xml(G,C,S,T);rclass_xml(G,C,S,T,Mapping)),
   		set_prop(rclass,C,exported),!.
   ensure_class(G,C,S,T,M) :-
   		error_out(['** INTERNAL ERROR: problems exporting class mapping (ensure_class):',
   						G,' ',C,' ' , S, ' ', T,' ',M]).
   ensure_super(entity):-!.
   ensure_super(Class) :-
           rch_class(Group,Class,Super,Table,Mapping),!,
           ensure_super(Super),
           ensure_class(Group,Class,Super,Table,Mapping).

   rclass2_xml(Group,Class,Super,Table):-
   	xml_write(['<CLASS NAME="',Class,'" SUPER="',Super,'" TABLE="',Table,'" GROUP="',Group,'">']),
   	xml_nl,
      class_attributes(Class,Attributes),
      rattributes2_xml(Attributes),
   	xml_write(['</CLASS>']),
   	xml_nl,!.

   /** DEPRECATED: this is the old style mapping output predicate : new style mappings use the rclass2_xml/4 */

   rclass_xml(Group,Class,Super,Table,Mapping):-xml_nl,
   	xml_write(['<CLASS NAME="',Class,'" SUPER="',Super,'" TABLE="',Table,'" GROUP="',Group,'">']),
   	xml_nl,
   	rattributes_xml(Mapping),
   	xml_write(['</CLASS>']),
   	xml_nl,!.

   rclass_xml(G,C,S,T,M):-
   		error_out(['** INTERNAL ERROR: problems exporting class mapping (rclass_xml):',
   						G,' ',C,' ' , S, ' ', T, ' ',M]).

   rattributes2_xml(Atrs and MoreAtrs) :-!,
      rattribute2_xml(Atrs),
      rattributes2_xml(MoreAtrs).

   rattributes2_xml(Atr):-!,
      rattribute2_xml(Atr).

   rattribute2_xml(Attribute column Column baseclass Class coltype Type colsize Length1 colprecision Precision1 pkey PK) :-
   	xml_write(['     <ATTRIBUTE NAME="',Attribute,'" COLUMN="',Column,
   								'" CLASS="',Class,
   								'" TYPE="',Type,
   								'" SIZE="',Length1,
   								'" PRECISION="',Precision1,
   								'" PKEY="',PK,'" />']),
      xml_nl,!.

   /** DEPRECATED: see rattributes2_xml*/
   rattributes_xml(Mapping) :-
   	clio_group_param(Mapping,certe,Attributes),
   	clio_group_param(Mapping,locus,Pkey),
   	rattribute_xml(Attributes,Pkey),!.

   rattributes_xml(M) :-
   	error_out(['** INTERNAL ERROR: problems exporting attributes mappings (rattributes_xml)',M]).




   /*

   	Outputs element mappings, one at a time
      DEPREACTED see rattribute2_xml
   */
   rattribute_xml([Attribute | More],Pkeys) :-
   	rch_element_class(Attribute,Class,Column,Type,Length,Precision,_),
   	(member_nth(Column,Pkeys,N) -> PK = N ; PK=0),
   						% removing the first char to leave the number
   	string_to_list(Length,[_ | L1 ]),string_to_list(Length1,L1),
   	string_to_list(Precision,[_ | P1 ]),string_to_list(Precision1, P1),
   	xml_write(['     <ATTRIBUTE NAME="',Attribute,'" COLUMN="',Column,
   								'" CLASS="',Class,
   								'" TYPE="',Type,
   								'" SIZE="',Length1,
   								'" PRECISION="',Precision1,
   								'" PKEY="',PK,'" />']),
      xml_nl,
   	rattribute_xml(More,Pkeys).
   rattribute_xml([],_):-!.
   rattribute_xml(E,P):-
   	error_out('** INTERNAL ERROR - Exporting attribute mapping (rattribute_xml)'-E-P),!.

   /*
    	rch_element_class(Element,Class,Column,Type,Length,Precision)
   	=======================================================
   				Determines de RCH column name, and properties for a Kleio element

   */
   rch_element_class(Element,Element,Column,Type,Length,Precision,Mapping) :-
   		  clio_super('element-mapping',Mapping),
   		  clio_group_param(Mapping,pars,[Element,Column,Type,Length,Precision]),!.
   rch_element_class(Element,SElement,Column,Type,Length,Precision,Mapping) :-
   		  clio_element_extends(Element,SElement),
   		  rch_element_class(SElement,SElement,Column,Type,Length,Precision,Mapping),!.
   rch_element_class(_,_,undef,undef,undef,undef,undef) :-!.


   /*

     get_rel_type: determine-reltype of the groups included by this one.
     =============
   */

   /*
   	get_rel_type(Group,Type)
   	Determines the relation type for the relation between a group and the
      group that contains it (e.g. the type of the relation between an actor and
      an act but also the type of the relation between an attribute and a person.
      The relation value is always the name of the contained group.


      DEPRECATED: currently not used.

   	The rule is: if the contained group or a group that it extends is part
         of a group that extends relation-type then this group name is used as
   		the relation type.

   */
   get_rel_type(Group,Type):-
   	clio_extends(Type,'relation-type'),
   	((clio_partof(Type,Group));(clio_partof(Type,Class),clio_extends(Group,Class))).

   get_rel_type(_,contained-in):-!.
   /*

   	belement_aspect(Aspect,BaseElement,Content) get the aspect corresponding to a baseclass element
      =========================================
   		  The gacto.str allows the definition of elements that extend other elements
   		  using the fons/source parameter. In this way a particular historical source
   		  can give localized names to certain elements that are required by the
   		  RCH model, like dates, Ids, names and sex of persons, etc...

   		  In a more general way, the element inheritance mechanism allows
           for the translator to find an expected element even if the source used
   		  a different name for it.

   		  belement_aspect is similar do clio_aspect (external.pl) but uses
   		  clio_element_extends (external.pl) to find in the current broup elements
   		  the base element it is looking for.
   */
   belement_aspect(Aspect,Element,Content) :-
      		  clio_elements(Els),						% list current elements
      		 % writeln(clio_elements(Els)),
      		  member(Element,Els),	!,				% get one of them
      		  clio_aspect(Aspect,Element,Content).      % if it does return aspect

   belement_aspect(Aspect,BaseElement,Content) :-
   		  clio_elements(Els),						% list current elements
   		 % writeln(clio_elements(Els)),
   		  member(Element,Els),					% get one of them
   		  clio_element_extends(Element,BaseElement),!, % check if it extends the base el
   		  clio_aspect(Aspect,Element,Content).      % if it does return aspect

   belement_aspect(_,_,[]):- !.


   /*
     ===========================================================================
    	xml utils
     ===========================================================================
   */

   /*

   	group_to_xml: writes the current group and its elements to the xml file.
     ===============

   */
   group_to_xml(GroupName,GroupId,InferedElements):-!,
   	rch_class(GroupName,Class,Super,Table,Mapping),
   	ensure_class(GroupName,Class,Super,Table,Mapping),
   	get_ancestor(_Anc,AncestorID),
      inccount(group,GroupNumber),
      clio_path(P),
      length(P,CurrentLevel),
      get_prop(line,number,N),
      xml_nl,
   	xml_write(['<GROUP ID="',GroupId,'" NAME="',GroupName,'" CLASS="',Class,'" ORDER="',GroupNumber,'" LEVEL="',CurrentLevel,'" LINE="',N,'">']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="line" CLASS="line"><core>',N,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="id" CLASS="id"><core>',GroupId,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="groupname" CLASS="groupname"><core>',GroupName,'</core></ELEMENT>']),
   	xml_nl,
      xml_write(['   <ELEMENT NAME="inside" CLASS="inside"><core>',AncestorID,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['   <ELEMENT NAME="class" CLASS="class"><core>',Class,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="order" CLASS="order"><core>',GroupNumber,'</core></ELEMENT>']),
      xml_nl,
      xml_write(['    <ELEMENT NAME="level" CLASS="level"><core>',CurrentLevel,'</core></ELEMENT>']),
      xml_nl,
      elements_to_xml(Class),
   	  ielements_to_xml(Class,InferedElements),
      xml_write('</GROUP>'),xml_nl,
   	!.

   group_to_xml(G,I,X):-error_out(['*** failure group_to_xml',G,I,X]).


   /*
     elements_to_xml(XML) : returns a xml representation of the current elements
                            in the current group.
     ======================
   */

   elements_to_xml(Class):-
   	clio_elements(Els),
   	els_to_xml(Class,Els),!.

   elements_to_xml(_Class) :- writeln('** element_to_xml failed!'),!.


   ielements_to_xml(Class,[Element|Rest]):-
   	Element =.. [Name,Core,Original,Comment],
   	iel_to_xml(Class,Name,Core,Original,Comment),
   	ielements_to_xml(Class,Rest),!.
   ielements_to_xml(_Class,[]):-!.



   els_to_xml(Class,[ El | MoreEls ]) :-
   	el_to_xml(Class,El),
   	els_to_xml( Class,MoreEls),!.

   els_to_xml(_Class, []) :- !.

   el_to_xml(_Class,id):-!.  /* we skip id elements because they were exported previously and part of the builtin els */
   el_to_xml(GClass,El) :-
   	clio_aspect(core,El,Core0),
   	((GClass='relation',El='iddest')->
          (writelistln(['   ** checking rel dest for prefix '| Core0]),
            Core0 = IdDest,
           (is_list(IdDest) -> list_to_a0(IdDest,SID0); SID0 = IdDest),
         	 check_id_prefix(SID0,Core)
         	 )
        ;
         	Core = Core0
    ),
   	clio_aspect(original,El,Original),
   	clio_aspect(comment,El,Comment),
    aspect_to_xml(core,Core,CoreXML),
   	aspect_to_xml(original,Original,OriginalXML),
   	aspect_to_xml(comment,Comment,CommentXML),
      /* in the new scheme if a group element is not mapped to an attribute of the group class then
         it is not possible to find the elementClass of the group element. we use undef to mark those.
         Note that DBClass in idb only fetches the group element that are mapped to class attributes */
      (isNewMappingMode -> (elementClass(GClass,El,Class)->true;Class=undef)  ;
                           rch_element_class(El,Class,_col,_type,_length,_precision,_mapping)),
   	xml_write( [ '   <ELEMENT NAME="' , El , '" CLASS="',Class,'">' ]),
      xml_nl,
      xml_write(['   '|CoreXML]),
   	(OriginalXML \= [] -> (xml_write(['   '|OriginalXML]), xml_nl);true),
   	(CommentXML \= [] -> (xml_write(['   '|CommentXML]), xml_nl);true),
   	xml_write( [ '   </ELEMENT>' ]),
      xml_nl.

   iel_to_xml(GClass,Name,Core,Original,Comment) :-     %  used to output infered elements
      (isNewMappingMode -> elementClass(GClass,Name,Class)  ;
                           rch_element_class(Name,Class,_col,_type,_length,_precision,_mapping)),
   	aspect_to_xml(core,Core,CoreXML),
   	aspect_to_xml(original,Original,OriginalXML),
   	aspect_to_xml(comment,Comment,CommentXML),
   	xml_write( [ '   <ELEMENT NAME="' , Name , '" CLASS="',Class,'">' ]),
      xml_nl,
      xml_write(['   '|CoreXML]),
   	(OriginalXML \= [] -> (xml_write(['   '|OriginalXML]), xml_nl);true),
   	(CommentXML \= [] -> (xml_write(['   '|CommentXML]), xml_nl);true),
   	xml_write( [ '   </ELEMENT>' ]),
      xml_nl.

   iel_to_xml(GClass,Name,_Core,_Original,_Comment) :-
      error_out(('** INTERNAL ERROR: failed to export infered element '-GClass-Name)),!.

   aspect_to_xml(_Aspect,[],[]):-!.
   aspect_to_xml(_Aspect,mult([]),[]):-!.
   aspect_to_xml(Aspect,mult([Oc1|More]),MultXML):-
      %writeln(mult([Oc1|More])),
      aspect_to_xml(Aspect,Oc1,Xml1),
      aspect_to_xml(Aspect,mult(More),XmlMore),
      append(Xml1,XmlMore,MultXML),!.

   aspect_to_xml(Aspect,Content,XML):-
      %writeln('content '-Content),
      (is_list(Content) -> C = Content; C = [Content]),
      append(['      <',Aspect,'><![CDATA['],C,P1),
      append(P1,[']]></',Aspect,'>'],XML),!.

   /*
     xml_write(List) : writes a list of attom to the current xml output file
     ================
   */

   xml_write(List):-!,
   	get_value(xmlfile,XMLFILE),
   	telling(OUT),
   	tell(XMLFILE),
           xcleanwrite(List),
   	tell(OUT).
   xml_nl:-!,
   	get_value(xmlfile,XMLFILE),
   	telling(OUT),
   	tell(XMLFILE),
   	nl,tell(OUT).

   xml_close:-
           !,
   	get_value(xmlfile,XMLFILE),
   	close_file(XMLFILE).

   xcleanwrite([]):-!.
   xcleanwrite([A|B]):-!,
       xcleanwrite(A),xcleanwrite(B).
   xcleanwrite(Atomic):-
       atom(Atomic),!,
       name(Atomic,Chars),
       xclean_chars(Chars,CChars),
       name(CAtomic,CChars),
       write(CAtomic).
   xcleanwrite(Functor):-!,write(Functor).


   xclean_chars([],[]):-!.
   xclean_chars([C|More],[C|CMore]):-C > 19, C < 128, !, xclean_chars(More,CMore).
   xclean_chars([10|More],[10|CMore]):-!, xclean_chars(More,CMore).
   xclean_chars([13|More],[13|CMore]):-!, xclean_chars(More,CMore).
   xclean_chars([_|More],[46|CMore]):-!, xclean_chars(More,CMore).



   /*
   	xml_welement(Element): outputs to the current xml file
   	=====================
       a xml element. Elements are prolog structures like
   		 this:
   		 		element(attribute1(value),attribute2(value)).

   */
   xml_welement(Element) :-
   		  Element =.. [Name | Attributes],
   		  xml_write(['<',Name]),
   		  xml_wattribute(Attributes),
   		  xml_write(['/>']),
   		  xml_nl.
   xml_wattribute([]):-!.
   xml_wattribute([Attribute|More]):-
   		  Attribute =.. [Name,Value],
   		  (is_list(Value) -> list_to_a0(Value,String); String = Value),
   		  xml_write([' ',Name,'=',String]),
   		  xml_wattribute(More),!.

   % in the future we must escape the < and the &

   xml_escape(S,S):-
   	!.

