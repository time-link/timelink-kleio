:-module(gactoxml,[
          db_init/0,
          db_store/0,
          db_close/0]
        ).

/** <module> Data generator for sources in Kleio notation

---++ Overview

 This translator provides a bridge between a person oriented
 data model for Micro Historical studies and Kleio as a source
 transcription notation.

 The translator is based on a generic kleio structure (equivalent to a schema in XML) definition
 called *gacto2.str*.

 The generic structure describes a source as a document containing
 acts which in turn contain references to persons or objects.

 The structure of person related data like attributes and relations with other
 persons is  defined. A basic structure for acts is also provided.

 The schema file *gacto2.str* includes some "abstract" groups for persons. There
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

Clio export modules such as the current file are Prolog programs that define the way
 the Clio texts are converted.

 The interaction between the Clio translator and the export modules
 is as follows:

*  The translator processes a clio structure file and the clio data file.
* It does the syntactic analysis and error checking in what regards compliance
 		of the data to the structure definition.
 * Once a Clio group is read from the data file by the translator a call is made to the export module
 		signalling that data for a group is available.
* The export module can then call back the Clio Translator code to request the data itself.


Two sets of predicates are involved in this interchange:

* the export predicates that are called by the translator
* the Translator predicates that are called by the export modules.

The export module must provide three predicates that are to be called
  by the translator: db_init/0, db_store/0, db_close/0.

* db_init
Is called when a new data file begins processing.
The export module can set db_init to do initialization procedures like openning export files and setting up
default values.

* db_store
Is called when a new clio group has been read by the
translator. The export module can now call back the
translator module to obtain the data.

* db_close
Is called when the processing of the data file is finished
The export module can close the export files and do general
cleanup procedures.

The call back predicates are defined in externals.pl

 @tbd TODO: The XML related stuff should be isolated from data model stuff.
 Better to end each export predicate with the creation of dictionnary with the
 Data structure of the specific group and then pass it on to specific format exporters
 So this file would be kept, renamed, and new simpler group_to_xml.pl and group_to_json.pl created.

 @author Joaqum Carvalho
 @license MIT
*/

:-use_module(library('sgml')).
:-use_module(library(http/json)).
:-use_module(library(filesex)).

:-use_module(clioPP).
:-use_module(counters).
:-use_module(errors).
:-use_module(externals).
:-use_module(inference).
:-use_module(logging).
:-use_module(mappings).
:-use_module(persistence).
:-use_module(reports).
:-use_module(utilities).
:-use_module(linkedData).
:-use_module(jsonUtilities).

?-thread_local(group_path/1).
?-thread_local(carel/4).
?-thread_local(carelnot/2).
?-thread_local(used_id/1).
?-thread_local(attribute_cache/4).
?-thread_local(same_as_cached/8).
?-thread_local(xsame_as_cached/8).
?-thread_local(same_as_cached_id/1).

% TODO: here we need to consult user mappings and inference rules

/*
================================================================================
  db_init - initializes database
================================================================================
*/

%% db_init is det.
%  Initializes the data store.
%  Basically creates derived files (.rpt, .ids, .err)
%
% @tbd Should delete .err file
%
db_init:-
    report( [
        writeln('Generic Act translation module with geoentities (XML).'),
        writeln('     Joaquim Ramos de Carvalho (joaquim@uc.pt) ')
        ] ),
    get_value(data_file,D),
    break_fname(D,Path,File,Base,__Ext),
    path_sep(Sep),
    list_to_a0([Path,Sep,Base],SOURCE),
    log_debug('translate: path: ~w sep:~w base:~w~n',[Path,Sep,Base]),
    put_value(source_file,SOURCE),
    put_value(source_file_name,File),
    concat(SOURCE,'.xml',XMLFILE),put_value(xmlfile,XMLFILE),
    log_debug('translate: xmlfile --> ~w~n',[XMLFILE]),
    open_file_write(XMLFILE),
    catch(chmod(XMLFILE,+gw),E,log_error('Could not change permissions of ~w : ~w ',[XMLFILE,E])),
    % prolog_to_os_filename(PD,D), file_directory_name(PD,Dir).
    (concat(__X,'_ids',Base)
         -> put_value(clioPP,false) % this is a ids file, no PP necessary
         ; (
       concat(SOURCE,'.ids',PPFILE),
       open_file_write(PPFILE),
       catch(chmod(PPFILE,+gw),E2,log_error('Could not change permissions of ~w : ~w ',[PPFILE,E2])),
       put_value(clioppfile,PPFILE),
       put_value(clioPP,true)
         )
    ),
    xml_write('<?xml version=\'1.0\'?>'), xml_nl,
    del_props(rclass), % erase cache of class mappings
    del_props(act),    % erase any act context that may have survived last translation
    del_props(person),
    del_props(autorels), %erase context for auto relation generation, and reset autorel id counters
    setgensymbol_local(lsa,0),setgensymbol_local(atra,0),setgensymbol_local(rela,0),setcount(group,1),
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

%% db_close is det
%
% Closes the output file
%
db_close:-
  do_auto_rels,  do_auto_rels2,
  process_cached_same_as,
  (get_value(clioPP,true)->clioPP_close;true),
  get_prop(kleio,groups,Groups),
  report_translation,
  xml_write(['</KLEIO>']),
  report([perror_count]),
  report([write('Groups in this file:'), writeln(Groups)]),
  report([writeln('Translation finished.')]),
  xml_nl,
  xml_close.



 report_translation:-
  get_value(stru_file,StruFile),
  report([format('Structure file: ~w~n',[StruFile])]),
  prolog_to_os_filename(PrologFile, StruFile),
  file_directory_name(PrologFile,PrologPath),
  file_base_name(StruFile,BaseName),
  file_name_extension(BaseNameNoExt,_,BaseName),
  % create srpt file name
  file_name_extension(BaseNameNoExt,'.srpt',SrptFile),
  atomic_list_concat([PrologPath,'/',SrptFile],SrptPath),
  report([format('Structure processing report: ~w~n',[SrptPath])]),
  % create json file name
  file_name_extension(BaseNameNoExt,'.str.json',JsonFile),
  atomic_list_concat([PrologPath,'/',JsonFile],JsonPath),
  report([format('Structure in JSON: ~w~n',[JsonPath])]),
  % report on kleio source file
  get_value(data_file,D),
  report([format('~nKleio file: ~w~n',[D])]),
  get_value(source_file,SOURCE),
  concat(SOURCE,'.org',Original),
  report([format('Original file: ~w~n',[Original])]),
  concat(SOURCE,'.old',Last),
  report([format('Previous version: ~w~n',[Last])]),
  errors:error_count(ErrCount),
  errors:warning_count(WarnCount),
  ( ErrCount = 0 -> rename_files(D,SOURCE,Original,Last);true),
  persistence:get_value(report,ReportFile),
  % Generate a JSON file with information on the related files
  FileDict = files{stru:StruFile,
                   stru_rpt:SrptPath,
                   stru_json:JsonPath,
                   kleio_file:D,
                   kleio_original:Original,
                   kleio_previous: Last,
                   kleio_rpt: ReportFile,
                   errors:ErrCount,
                   warnings:WarnCount},
  concat(SOURCE,'.files.json',KleioFilesInfo),
  open(KleioFilesInfo,write,KFI_Stream,[]),
  json_write_dict(KFI_Stream,FileDict),
  close(KFI_Stream),
  !.
report_translation:-
   error_out('** Problem renaming files.'),
   report([writeln('** Problem renaming files.')]) .

/* we now rename the .ids (pretty printed .cli and ids) to .cli and the .cli to .org.
  If org already exists we rename .cli to .old. If .old exists we discard it and rename .cli to .old.

  The end result is: .cli is the last pretty printed translation with sucess.
  .old is the last .cli that was translated
  .org is the original .cli that was first translated with success.
  */
rename_files(D,SOURCE,Original,Last):-
  % report on kleio source file
  concat(SOURCE,'.ids',Ids),
  report([format('Temp file with ids: ~w~n',[Ids])]),
  /*
  (exists_file(Last) ->
    ((delete_file(Last),report([writeln('** Deleted previous version'-Last)]))
    ;
    (true,report([writeln('** No previous version'-Last)])))),
  (exists_file(Original)->
    (rename_file(D,Last),report([writeln('** '-D-'renamed to'-Last)]))
    ;
    (rename_file(D,Original),report([writeln('** '-D-'renamed to'-Original)])) ),
  rename_file(Ids,D),
  */
  /*
  Trying to replace the code above with direct shell calls because of a
  bug that occurs when kleio-server runs in a virtual box environment on windows
  See  https://bugzilla.gnome.org/show_bug.cgi?id=656225
  */
  (exists_file(Last) ->
    delete_file(Last) % delete last translated ".old"
    ;
    true
  ),
  (exists_file(Original)-> % rename original ".cli" to ".old"
    (

      rename_with_shell(D,Last),
      catch(
        chmod(D,+gw),
        E,
        log_error('Could not change permissions of ~w : ~w ',[D,E])
        ),
      report([writeln('** '-D-'renamed to'-Last)])
    )
    ;  % no ".org" file. Rename ".cli" to ".org"
      (
          rename_with_shell(D,Original),
          catch(
              chmod(Original,+gw),
              E2,
              log_error('Could not change permissions of ~w : ~w ',[Original,E2])
              )
          % , report([writeln('** '-D-'renamed to'-Original)])
      )
  ),
  rename_with_shell(Ids,D), % rename ".ids" to ".cli"
  catch(
      chmod(D,+gw),
      E3,
      log_error('Could not change permissions of ~w : ~w ',[D,E3])
      ),
  !.

rename_with_shell(Name1,Name2):-
  Command = ['rm -f ',Name2,'; cp -fp ',Name1,' ',Name2,'; rm -f ',Name1],
  atomic_list_concat(Command,'',S),
  shellUtil:shell_to_list(S,0,_),!.
rename_with_shell(Name1,Name2):-
  error_out('** Problem renaming files '-Name1-' to '-Name2),
  report([writeln('Could not rename'-Name1-' to '-Name2)]),!.

/*
================================================================================
  db_store  - stores the current group
================================================================================
*/

%% db_store is det.
%
% This is the top level predicate that stores
% the various types of predicates generated from
% the current data structure. We get the source (fons) parameter of the group
% and use that to determine the group handling code
% In this way the source can have their own vocabulary
% and by stating the source or fons get the right processor
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
  add_to_prop(kleio,groups,G),  % keep track of this group
  save_group_path(P,G,ID),
  process_linked_data(G,ID),
  (get_value(clioPP,true) -> %if necessary we produce a ids file
        (
        remove_id_prefix(ID,NID),
        clioPP(G,NID)
        )
      ;
        true
  ),
  !.

db_store:-
error_out(('** INTERNAL ERROR: Failure: db_store')),!.

/* this selects which autorel method is used.
   Method 1 uses special kleio groups, introduced in the kleio str file for generating  auto rels
   Method 2 uses a separete rule file -- inference.pl
   */

set_autorel_mode(1):-put_value(autorelmode,1),!.
set_autorel_mode(2):-put_value(autorelmode,2),!.
set_autorel_mode(''):-!.
set_autorel_mode(O):-
    warning_out(['bad autorel mode =',O]),!.

/* this saves the current path with the ids created by this translator module
   current path for each processed group is stored as a group_path(Path) clause
   that is available latter for auto rel processing */
save_group_path(Path,GroupName,GroupId):-
  getCurrentPath(CurrentPath),
  length(Path,LPath),
  first_n(CurrentPath,LPath,NewAncPath),
  G =.. [GroupName,GroupId],
  append(NewAncPath,[G],NewPath),
  setCurrentPath(NewPath),
  assert(group_path(NewPath)),% TODO group_path is dynamic
  !.

getCurrentPath(CurrentPath) :-
  get_prop(autorels,currentpath,CurrentPath),!.
getCurrentPath([]):-!.

setCurrentPath(CurrentPath):-
  set_prop(autorels,currentpath,CurrentPath),!.


%!  group_export(+GroupType,?GroupID) is det.
%  New groups, that do not extends the base group types
%  will need a clause here to connect to their export module.
%

group_export(kleio,_) :- !,
    report([
        writeln('========================='),
        writeln('kleio translation started'),
        writeln('=========================')
        ]),
    clio_aspects(core,[structure,translator,autorels,obs,prefix,translations],[S,___T,AR,O,SP,TC]),
    report([
        writelist0ln(['Structure: '|S]),
        writelist0ln(['Prefix: '|SP]),
        writelist0ln(['Autorel: '|SP]),
        writelist0ln(['Translation count: '|TC]),
        writelist0ln(['Obs: '|O])
        ]
        ),
      ([AutoMode] = AR; AutoMode = ''),
      set_autorel_mode(AutoMode),
    linkedData:clear_xlink_patterns,
    clio_stru_file(Stru),
    clio_data_file(Data),
    list_to_a0(O,OS),
    list_to_a0(SP,Space),
    list_to_a0(TC,TransCountString),
    (SP = [_H|_T] -> put_value(useIdPrefix,yes);  put_value(useIdPrefix,no)),
    put_value(idPrefix,Space),
    (atom_number(TransCountString,TransCount) ->
        put_value(transcount,TransCount);
        (report([
          writelist0ln(['Translation count not set or invalid, counting as zero: '|TC])
          ]),
          put_value(transcount,0)
        )
        ),
    get_value(transcount,OldTC),
    NewTC is OldTC+1,
    put_value(transcount,NewTC),
    now(Year,Month,Day,Hour,Minute,Seconds),
    Date=Year-Month-Day,
    Time=Hour:Minute:Seconds,
    % REFACTOR create dict for data and pass on to runtime mapper (XML, JSON, etc...)
    xml_quote_attribute_list(Stru, QStru,utf8),
    xml_quote_attribute_list(Data, QData,utf8),
    %xml_quote_attribute_list(Date, QDate,utf8),
    QDate = Date,
    %xml_quote_attribute_list(Time, QTime,utf8),
    QTime = Time,
    xml_quote_attribute_list(OS, QOS,utf8),
    xml_quote_attribute_list(Space, QSpace,utf8),
    xml_write(['<KLEIO STRUCTURE="',QStru,'" SOURCE="',QData,'" TRANSLATOR="gactoxml2.str" WHEN="',QDate,' ',QTime,'" OBS="',QOS,'" SPACE="',QSpace,'">']),
    xml_nl.

/*
Groups related to authority registers
*/
group_export(Register,ID):-
    group_derived(Register,'authority-register'),
    authority_register_export(Register,ID),!.
group_export(REntity,ID):-
   group_derived(REntity,'rentity'),
   rentity_export(REntity,ID),!.
group_export(RPerson,ID):-
    group_derived(RPerson,'rperson'),
    rperson_export(RPerson,ID),!.
group_export(ROject,ID):-
      group_derived(ROject,'robject'),
      robject_export(ROject,ID),!.
group_export(Occ,ID):-
        group_derived(Occ,'occ'),
        rentity_occ_export(Occ,ID),!.

% Dealing with linked data
group_export(Link,__ID):-
  group_derived(Link,link),
  clio_aspects(core,[shortname,urlpattern,obs],[ShortName,UrlPat,__obs]),
  atomic_list_concat(ShortName,'',SN),
  atomic_list_concat(UrlPat,'',UP),
  store_xlink_pattern(SN,UP).

% Dealing with properties

group_export(Property,__ID):-
  group_derived(Property,property),
  clio_aspects(core,[name,value,obs],[Name,Value,__obs]),
  atomic_list_concat(Name,'',SN),
  atomic_list_concat(Value,'',SV),
  get_value(data_file,DataFile),
  set_prop(DataFile,SN,SV),!.

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

group_export(Geoentity,ID) :-
  group_derived(Geoentity,geoentity),
  geoentity_export(Geoentity,ID),!.

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
  switchboard. They receive the actual group name used in
  the clio source file  and the auto-generated clio ID
  (which can be used or not).


*/

/*

Handling of authority registers
*/

authority_register_export(Register,Id):-
    report([writelist0ln(['** Processing authority register ',Register,'$',Id])]),
    (get_date(Date1) -> Date=Date1 ;  % we handle long dates and day/month/year dates
    (get_y_m_d(Date2) -> Date=Date2; Date=0)),
    set_prop(act,date,Date), % this is for attributes and relations in rentities
    set_prop(aregistry,date,Date),
    set_prop(aregistry,id,Id),
    %
    belement_aspect(core,user,User),
    atomic_list_concat(User,'',UserString),
    set_prop(aregistry,user,UserString),
    %
    belement_aspect(core,dbase,DBase),
    set_prop(aregistry,dbase,DBase),
    %
    belement_aspect(core,replace_mode,ReplaceMode),
    set_prop(aregistry,replace_mode,ReplaceMode),
    %
    belement_aspect(core,ignore_date,IgnoreDate),
    set_prop(aregistry,ignore_date,IgnoreDate),
    %
    % REFACTOR
    %
    group_to_xml(Register,Id,[

    ]),
    !.
rentity_export(Rentity,Id):-
    set_prop(rentity,id,Id),
    get_prop(aregistry,user,User),
    setgensymbol_local(occ,0), % we reset the occurrences counters so that it restarts from 1 at each rentity
    group_to_xml(Rentity,Id,[
        user([User],[],[]),
        the_class([rentity],[],[])
        ]),
    report([writelist0ln(['** Processing rentity record ',Rentity,'$',Id])]),
    !.
rperson_export(RPerson,Id):-
  set_prop(rentity,id,Id),
  belement_aspect(core,sname,Description),
  get_prop(aregistry,user,User),
  setgensymbol_local(occ,0), % we reset the occurrences counters so that it restarts from 1 at each rentity
  group_to_xml(RPerson,Id,[
      description([Description],[],[]),
      user([User],[],[]),
      the_class([rperson],[],[])

      ]),
  report([writelist0ln(['** Processing rperson record ',RPerson,'$',Id])]),
  !.
robject_export(RObject,Id):-
  set_prop(rentity,id,Id),
  belement_aspect(core,sname,Description),
  get_prop(aregistry,user,User),
  setgensymbol_local(occ,0), % we reset the occurrences counters so that it restarts from 1 at each rentity
  group_to_xml(RObject,Id,[
      description([Description],[],[]),
      user([User],[],[]),
      the_class([robject],[],[])
    ]),
  report([writelist0ln(['** Processing robject record ',RObject,'$',Id])]),
  !.
rentity_occ_export(_,Id):-
  get_prop(rentity,id,REId),
  get_prop(aregistry,user,User),
  get_prop(aregistry,id,ARId),
  belement_aspect(core,occurrence,Occurrence),
  export_attach_occ(xml,Id,ARId,User, REId,Occurrence),
  report([writelist0ln(['**     Processing rentity occurence ',Occurrence])]),
  !.

/* deals with values that are stored as list as well as atoms, lists break xml_quote_attribute */
xml_quote_attribute_list([H|T],Quoted,Encoding):-!,
  atomic_list_concat([H|T],' ',Atom),
  xml_quote_attribute(Atom,Quoted,Encoding),!.
xml_quote_attribute_list(Atom,Quoted,Encoding):-
  xml_quote_attribute(Atom,Quoted,Encoding),!.

export_attach_occ(xml,OccID,ARId,User,REId,Occurrence):-
  xml_nl,
  xml_quote_attribute_list(OccID,POccID,utf8),
  xml_quote_attribute_list(ARId,PARId,utf8),
  xml_quote_attribute_list(User,PUser,utf8),
  xml_quote_attribute_list(Occurrence,POccurrence,utf8),
  xml_quote_attribute_list(REId,PREId,utf8),
  xml_write(['<RELATION ID="',POccID,'" REGISTER="',PARId,'" USER="',PUser,'" ORG="',POccurrence,'" DEST="',PREId,'" TYPE="META" VALUE="attach_to_rentity"/>']),
  xml_nl,!.



/*


Hangling of historical sources


*/
historical_source_export(Source,Id):-
  do_auto_rels2,
  report([writelist0ln(['** Processing source ',Source,'$',Id])]),
  get_value(data_file,D),
  break_fname(D,_,_,Base,__Ext),
  % get_value(source_file_name,Name),
  %report([writelist0ln(['** Base name of file ',Base, ' in ',Name])]),
  check_id_prefix(Base,BaseAsId),
  (BaseAsId \= Id -> warning_out(['* Warning: Filename should match source Id to avoid errors. File: ',Base,' Id: ',Id,'.']);true),
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
  get_prop(line,number,N),

  report([writelist0ln([N,': ',Group,'$',Id])]),
  del_props(act),
  del_props(person),
  set_prop(act,id,Id),
  set_prop(act,group,Group),
  %log_debug('historical_source_export ~w$~w props set',[Group,Id]),
  (get_date(Date1) -> Date = Date1 ;
           (get_y_m_d(Date2) -> Date = Date2 ;
                    (get_prop(source,date,Date3) -> Date = Date3 ;
                          (Date = 0,
                                warning_out(['* Warning: No date in act nor in containing source ',
                                                     Group,'(',Id,')'
                                            ]
                                           )

                          )
                    )
              )
     ),
  set_prop(act,date,Date),
  %log_debug('historical_source_export ~w$~w infered date ~w',[Group,Id,Date]),
  setgensymbol_local(per,0), % we reset the counter so that auto ids are local to the act
  setgensymbol_local(obj,0),
  setgensymbol_local(good,0),
  setgensymbol_local(rel,0),
  setgensymbol_local(att,0),
  setgensymbol_local(gro,0),
  %log_debug('historical_source_export calling group_to_xml',[]),
  group_to_xml(Group,Id,[date([Date],[],[]),type([Group],[],[])]),
  %log_debug('historical_source_export ~w$~w DONE',[Group,Id]),
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
  assertz(same_as_cached_id(ID)),
  group_to_xml(G,ID,ExtraElements),
  process_function_in_act(G,ID),
  !.

object_export(G,ID) :-
  (belement_aspect(core,id,[]) ->  IdElement=[id([ID],[],[]),type([G],[],[]) ]; IdElement=[type([G],[],[])]),
  assertz(same_as_cached_id(ID)),
  group_to_xml(G,ID,IdElement),
  process_function_in_act(G,ID),
  % report(writeln(G-ID-'** was stored with function in ACT verify act')),
    !.

geoentity_export(G,ID) :-
  (belement_aspect(core,id,[])
              ->  IdElement=[id([ID],[],[]),type([G],[],[])]
              ;   IdElement=[type([G],[],[])]),
  assertz(same_as_cached_id(ID)),
  group_to_xml(G,ID,IdElement),
  process_function_in_act(G,ID),
  % report(writeln(G-ID-'** was stored with function in ACT verify act')),
  !.

attribute_export(G,ID) :-
    get_ancestor(__Anc,AncId),
    (get_date(Date,Extra) ->
      (DateType = explicit)
    ; %  TODO: check if this works for events
      (get_prop(act,date,Date), DateType = implicit,Extra=empty{})
    ),
    clio_belement_aspect(core,type,T),
    clio_belement_aspect(core,value,V),
    assert(attribute_cache(AncId,ID,T,V)),
    % report(writeln('** caching attribute info '-G-ID-T-V)),
    % Processing of linked data must go here
    clio_belement_aspect(comment,type, TC),
    atomic_list_concat(TC, '',TypeComment ),
    (generate_xlink(TypeComment,Uri,DataSource,Id) ->
        (
          (
          process_xlink_attribute_type(G,AncId,T,V,(DataSource,Id),TypeComment,Uri))
          ;
          warning_out(["Could not link data with ",
                      TypeComment,
                      "; is link$ definition for ",
                      DataSource," missing?"])
        )
        ;
        true),
        clio_belement_aspect(comment,value,VC),
        atomic_list_concat(VC, '',ValueComment ),
    (generate_xlink(ValueComment,Uri2,DataSource2,Id2) ->
        (
          process_xlink_attribute_value(G,AncId,T,V,(DataSource2,Id2),ValueComment,Uri2)
          ;
          warning_out(["Could not link data with ",
                      ValueComment,
                      "; is link$ definition for ",
                      DataSource2," missing?"])

          )
        ;
        true),
    dict_json_string(Extra,JsonString),
    Inferred = [id([ID],[],[]),entity([AncId],[],[]), date_extra_info([JsonString],[],[])],
    (DateType = implicit ->  % if the date was inferred add it to elements
        append(Inferred,[date([Date],[],[])],Elements)
        ;
        Elements = Inferred
        ),
    group_to_xml(G,ID, Elements).


relation_export(G,ID) :-
  get_ancestor(__Anc,AncId),
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
    This is used both in person and in objects, and geoentities?
*/
process_same_as(__Group,Id):-
  belement_aspect(core,same_as,[SameId]),
  log_debug('translate:    ** processing_same_as for ~w~n',[Id]),
  (is_list(SameId) -> list_to_a0(SameId,SID0); SID0 = SameId),
  check_id_prefix(SID0,SID),
  gensymbol_local(rela,Rid),
  get_ancestor(___anc,AncID),
  get_prop(act,date,Date),
  rch_class(relation,Class,Super,Table,Mapping),
  ensure_class(relation,Class,Super,Table,Mapping),
  inccount(group,GroupNumber),
  clio_path(P),
  length(P,CurrentLevel),
  ThisLevel is CurrentLevel+1,
  get_prop(line,number,N),
  assertz(same_as_cached(AncID,Rid,Id,SID,GroupNumber,ThisLevel,N,Date)),
  (clause(same_as_cached_id(SID),true)->true
    ;
    warning_out(['destination id of "same as" not found. Will check again at the end of file.'])
    ),
  !.

/* xsame_as elements differ in the checks made. The same type of relation is generated, but no test is made if the
   destination id exists in this file. Also the destination id does not receive the prefix of the kleio file if any.
   */
process_same_as(__Group,Id):-
  belement_aspect(core,xsame_as,[SameId]),
  log_debug('translate:    ** Processing EXTERNAL same_as for ~w~n',[Id]),
  (is_list(SameId) -> list_to_a0(SameId,SID); SID = SameId),
  gensymbol_local(rela,Rid),
  get_ancestor(___anc,AncID),
  get_prop(act,date,Date),
  rch_class(relation,Class,Super,Table,Mapping),
  ensure_class(relation,Class,Super,Table,Mapping),
  inccount(group,GroupNumber),
  clio_path(P),
  length(P,CurrentLevel),
  ThisLevel is CurrentLevel+1,
  get_prop(line,number,N),
  assertz(xsame_as_cached(AncID,Rid,Id,SID,GroupNumber,ThisLevel,N,Date)),
  !.
process_same_as(_,_).




/* Process the cached same as */

process_cached_same_as:-
  %report([writeln('**** SAME AS checks STARTED...')]),
  p_cached_same.


p_cached_same:-
    clause(same_as_cached(AncID,Rid,Id,SID,GroupNumber,ThisLevel,N,Date),true),
    p_export_cached_same_as(AncID,Rid,Id,SID,GroupNumber,ThisLevel,N,Date) ,
    %report([write('  ** Testing'-Id-same_as-SID)]),
    (clause(same_as_cached_id(Id),true)->
        true
        %report([write(' Origin OK')])
        ;
        error_out([' Could not find same_as id ',Id,'. If  original reference is on another file use xsame_as'],[line_number(N)])),
     (clause(same_as_cached_id(SID),true)->
         true %report([write(' Destination OK')])
         ;
         error_out([' Could not find same_as id ',SID,'. If  original reference is on another file use xsame_as'],[line_number(N)])),
      % report([nl]),
      fail.

      % and now the external references
p_cached_same:-
  report([nl]),
    clause(xsame_as_cached(AncID,Rid,Id,SID,GroupNumber,ThisLevel,N,Date),true),
    p_export_cached_same_as(AncID,Rid,Id,SID,GroupNumber,ThisLevel,N,Date) ,
    format(string(S),'Line ~w "SAME AS" TO EXTERNAL REFERENCE EXPORTED (~w) CHECK IF IT EXISTS BEFORE IMPORTING THIS FILE.',[N,SID]),
   report([writeln(S)]),
      fail.
  p_cached_same:-
    report([nl]),
    retractall(same_as_cached(_,_,_,_,_,_,_,_)),
    retractall(xsame_as_cached(_,_,_,_,_,_,_,_)),
    retractall(same_as_cached_id(_)).

p_export_cached_same_as(AncID,Rid,Id,SID,GroupNumber,ThisLevel,N,Date):-
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
  xml_write(['</GROUP>']),!.

process_function_in_act(Group,Id):-
  gensymbol_local(rela,Rid),
  get_ancestor(___anc,AncID),
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

/* Processing of special data types and linked data

Bellow is special processing for linked data comments and dates.
Currently this special processing is triggered at group processing level, i.e.,
group processor, like attribute_export, act_export, call predicates to process
dates and linked data.

But this does not scale and is not compatible with user defined groups.
So it should go to element processing level.

If a given element is of a given type then spcial processing shoud lbe triggered.
Currently elements have three aspectes: core, original, comment. In the future
special type of elements would have extra aspects. For instance, dates would have "date_vale"
or "data_range_value". Similarly element with linked data would have aspects "ldata_type" and
"ldata_uri".



*/

/**
 * processing_linked_data(+Group,+ID) is det.
*
* Processing linked data for the current group.

Linked data, e.g., references to entities in the Semantic Web,areintroduced
through comments. e.g.

```
n$Giulio Aleni/id=deh-giulio-aleni#@wikidata:Q2707504
```
To solve the notation into a URI it is necessary to introduce a new attribute in `kleio` group:

```
Kleio$...
      link$wikidata/"https://www.wikidata.org/wiki/$1"
      link$geonames/"https://www.geonames.org/$1"

     source$....
         act$....
             n$Giulio Aleni/id=deh-giulio-aleni#@wikidata:Q2707504    <-link on id (or name)
                 ls$nascimento/Brescia#@wikidata:Q6221/1582.              <-link on attribute value

      ...

    geo1$Chekiang#Tche-kiang, hoje:Zhejiang, 浙江, @wikidata:Q16967/província <-- link on name

```
## semantics

The handling of linked data notation differs according to the group/element combination in which the link is added.

### Link in generic group elements (except in attributes and relations)

For any group except those that extend `attribute` (atr, ls) the link generates a new attribute
in the form

 ```
    ls$<GROUP>-<ELEMENT>-<LINK_TYPE>/<LINK_URI>
```
So
```
   n$Giulio Aleni/id=deh-giulio-aleni#@wikidata:Q2707504

   generates:

       ls$wikidata-person-id/"http://wikidata.org/wiki/Q2707504"   <-- generated attribute (inferred date)

```

### Links in attribute values

Links in attribute values are assuming to refer to external representations of the value of the the attribubte.

So the attribute is duplicated with a qualification in the attribute type and with the explicit url link as value.

```
     ls$<LINK_TYPE>:<TYPE>/<LINK_URI># original value/LINK_URI
```
So
```
     ls$nascimento/Brescia#@wikidata:Q6221/1582

     generates:

    ls$wikidata:nascimento/"http://wikidata.org/wiki/Q6221"#Brescia/1582. <-- generated

```

### Links in attribute types

Links in attribute types are assumed to be links to external references to properties.
The attribute is duplicated with the type replaced with the external reference. The value is kept, except if it was also linked externally. Then the link to the external reference of the value is used.

```
    ls$<LINK_TYPE>:<LINK_TYPE_URI>#<LINK_URI>%original type/<VALUE> or <LINK_URI_VALUE>#original value
    ls$nacionalidade#@wikidata:Property:P27/República de Veneza#@wikidata:Q4948

    generates:

    ls$wikidata:Property_P27#"http://wikidata.org/wiki/Property:P27"%nacionalidade/"http://wikidata.org/wiki/Q4948"#República de Veneza
```
 */


process_linked_data(Group,Id):-
  clio_bclass(Group,GroupBClass),  % we use the base class of the group
  \+ memberchk(GroupBClass, [attribute,relation]),  % everything but attribute and relations (see next)
  clio_aspect(comment,Element, Comment),
  flatten_multiple_entry(Comment,FComment),
  atomic_list_concat(FComment,'',CommentString),
  generate_xlink(CommentString,Uri,DataSource,XId),
  clio_element_bclass(Element,ElementBClass), % and the base class of the element to generate the attribute type
  atomic_list_concat([GroupBClass,':',ElementBClass,'@',DataSource],'',LinkedAType),
  export_auto_attribute(Id,'atr', 'attribute',
                        LinkedAType, '','', % attribute type: core, comment, original
                        Uri,CommentString,XId % attr. value: core, comment, original
                        ),fail.

process_linked_data(_,_):-!.

% TODO: reimplement as the above
process_xlink_attribute_type(_Group,Id,Type, Value, (DatSource,XId),TypeComment,Uri):-
  % ls$wikidata.P551#"http://www.wikidata.org/wiki/Property:P551"/Macerata/15521006
  atomic_list_concat([DatSource,':',XId],'',LinkedAType),
  export_auto_attribute(Id,
                        'atr',
                        'attribute',
                        LinkedAType, Uri,TypeComment,
                        Value,'',Type),!.

process_xlink_attribute_type(Group,Id,_Type,_Value,_,TypeComment,_Uri):-
  error_out([' Could not generate linked data URI from ',TypeComment,' in ',Group-Id,'. Check if the data source was declared as link$shortname/URLPattern under the kleio$ group.']),!.

process_xlink_attribute_value(Group,Id,Type, Value, (DataSource,_XID), _ValueComment,Uri):-
   % ls$estadia.@/"http://www.wikidata.org/wiki/Q16572"/15830800
   (is_list(Type) -> atomic_list_concat(Type,'',TypeFlat); TypeFlat=Type),
   atomic_list_concat([TypeFlat,'@',DataSource],'',LinkedAType),
   export_auto_attribute(Id,Group,'attribute',LinkedAType,'','',Uri,'',Value),!.
process_xlink_attribute_value(Group,Id,Text, _Value, (_DataSource,_XID),_ValueComment,_Uri):-
  warning_out([' Could not generate linked data URI from ',Text,' in ',Group-Id,'. Check if the data source was declared as link$shortname/URLPattern under the kleio$ group.']),!.

/*
infer_sex(Group,Sex)
  ====================


*/
infer_sex(__G,S):-
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
    mplement https://github.com/time-link/timelink-kleio/issues/1

    Another alternative for the notation
- "<date"before date
+ ">date" after date
Date:  period starting in date
:Date  period ending in date
:date: date is part of period with start and end unknown TBD does not make sense
Dates can be expressed as
- yyyymmdd
- yyyy-mm-dd
- yyyy or yyyy0000 for date with month and day unknown
- yyyymm, yyyy-mm or yyyymm00 date with day unknown
- yyyy or yyyy0000 date with month and day unknown

So
- 1580:1640
- 1580:1640-01-01
- >1580:1641
- 1580: from 1580 onwards
- :>1580 ending after 1580

This would generate extra infered elements and kick in when date is not a number
date formats above would be processed to produce a yyyymmdd date a
get_date(DATE,EXTRA) where EXTRA is a dictionary
    and EXTRA_JSON a JSON string with the dictionary


*/
get_date(DATE):-
      clio_belement_aspect(core,date,[A|B]),
      match_date([A|B],DATE,_),!.

get_date(DATE,EXTRA):-
  clio_belement_aspect(core,date,[A|B]),
  match_date([A|B],DATE, EXTRA),
  !.

% get_date(DATE):-
%   clio_aspect(core,date,[A|B]), % avoid empty lists
%   match_date([A|B],DATE),!.

% get_date(DATE):-
%     clio_aspect(core,DATE_ELEMENT,[A|B]),
%     clio_element_bclass(DATE_ELEMENT,date),
%     match_date([A|B],DATE),!.

match_date(List,DateString, DateInfo):-
  atomic_list_concat(List,OriginalValue),
  match_single_date(List,DateString,DateValue),
  DateInfo=date{type:single,original:OriginalValue, date:DateValue, value: DateString},
  !.

match_date(List,DateString, DateInfo):-
  atomic_list_concat(List,OriginalValue),
  match_single_relative_date(List,DateString,DateValue),
  DateInfo=date{type:relative,original:OriginalValue, date:DateValue, value:DateString},
  !.

match_date(List,DateString, DateInfo):-
  atomic_list_concat(List,OriginalValue),
  match_range(List,DateString,DateValue),
  DateInfo=date{type:range,original:OriginalValue,  date:DateValue, value:DateString},
  !.

% failed
match_date(List,0,[error(List)]):-
  append(['Bad date format:'], List, M1),
  append(M1, ['. Use YYYYMMDD, YYYY-MM-DD, FROM_DATE:UNTIL_DATE'], M2),
  error_out(M2),
  fail,!.

% date expressed as "before date"
match_single_relative_date([<|List],Date, relative{subtype:before, value:DateExtra}):-
  match_single_date(List,_, DateExtra),
  option(value(DateValue),DateExtra),
  DateAdj is DateValue - 0.3,
  atom_number(Date,DateAdj),
  !.

% date expressed ad "after date"

match_single_relative_date([>|List],Date,relative{subtype:after, value:DateExtra}):-
  match_single_date(List,_, DateExtra),
  option(value(DateValue), DateExtra),
  DateAdj is DateValue + 0.3,
  atom_number(Date,DateAdj),
  !.

match_single_date([Y,'-',M,'-',D],DateString, single{subtype:ymd, value:DATE}):-
  is_a_number(Y,Y1),
  Y1 > 31,Y1 < 9999,
  is_a_number(M,M1),
  M1 >= 0, M1 < 13,
  is_a_number(D,D1),
  D1 >= 0, D1 < 32,
  DATE is Y1*10000+M1*100+D1,
  atom_number(DateString, DATE),
  !.
% Match Y-M day missing
match_single_date([Y,'-',M],DateString, single{subtype:ymd, value:DATE}):-
  is_a_number(Y,Y1),is_a_number(M,M1),
  Y1 >= 0,Y1 < 9999,
  is_a_number(M,M1),
  M1 >= 0, M1 < 13,
  DATE is Y1*10000+M1*100,
  atom_number(DateString, DATE),
  !.

% match D-M-Y
match_single_date([D,'-',M,'-',Y],DATE2, single{subtype: ymd, value:DATE1}):-
  is_a_number(Y,Y1),is_a_number(M,M1),is_a_number(D,D1),
  Y1 > 0,Y1 < 9999,
  is_a_number(M,M1),
  M1 > 0, M1 < 13,
  DATE1 is Y1*10000+M1*100+D1,
  % TT is Y1 + M1 + D1,
  % TM is Y1 + M1,
  % (T = 0 ->  % all zero
  %   PRECISION = missing
  % ;
  %   (TM = TT ->   % Day is zero
  %     PRECISION = ym
  %   ;
  %     TT = Y1 ->
  %       PRECISION = y
  %     ; % sould not happen
  %       PRECISION = undef
  %     )
  %   ),
  atom_number(DATE2,DATE1),!.

% match 0-0-0
match_single_date([D,'-',M,'-',Y],'0', zero{subtype:missing, value:0}):-
  is_a_number(Y,0),is_a_number(M,0),is_a_number(D,0),
  !.

% match M-Y
match_single_date([M,'-',Y],DATE2, single{subtype:ym, value:DATE1}):-
  is_a_number(Y,Y1),is_a_number(M,M1),
  Y1 >= 0,Y1 < 9999,
  is_a_number(M,M1),
  M1 >= 0, M1 < 13,
  DATE1 is Y1*10000+M1*100,
  atom_number(DATE2,DATE1),!.

% match 000000
match_single_date([A_DATE],'00000000', single{subtype:missing, value:0}):-
  is_a_number(A_DATE,0),!.
% match YYYY0000
match_single_date([A_DATE],DATE2, single{subtype:y, value:DATE1}):-
  is_a_number(A_DATE,DATE1),
  A is log10(DATE1),
  A>7,
  atom_number(DATE2,DATE1),
  sub_atom(DATE2,4,4,0,'0000'),
  !.
% match YYYYMM00
match_single_date([A_DATE],DATE2, single{subtype:ym, value:DATE1}):-
  is_a_number(A_DATE,DATE1),
  A is log10(DATE1),
  A>7,
  atom_number(DATE2,DATE1),
  sub_atom(DATE2,6,2,0,'00'),
  !.
% match YYYYMMDD
match_single_date([A_DATE],DATE2, single{subtype:ymd, value:DATE1}):-
  is_a_number(A_DATE,DATE1),
  A is log10(DATE1),
  A>7,
  atom_number(DATE2,DATE1),
  !.
% match YYYYMMD
match_single_date([A_DATE],DATE2, single{subtype:ymd, value:DATE1}):-
  is_a_number(A_DATE,DATE),
  A is log10(DATE),
  A>6,
  DATE1 is DATE*10,
  atom_number(DATE2,DATE1),

  !.
% match YYYYMM
match_single_date([A_DATE],DATE2, single{subtype:ym, value:DATE1}):-
  is_a_number(A_DATE,DATE),
  A is log10(DATE),
  A>5,
  DATE1 is DATE*100,
  atom_number(DATE2,DATE1),!.
% match YYYY
match_single_date([A_DATE],DATE2, single{subtype:y, value:DATE1}):-
  is_a_number(A_DATE,DATE),
  A is log10(DATE),
  A>3,
  DATE1 is DATE*10000,
  atom_number(DATE2,DATE1),!.

% range
match_range(List,DateRangeString, range{subtype: from_to, from:RDate1Extra, to:RDate2Extra}):-
  append(Date1,[':'|Date2],List),
  (
    (
      match_single_date(Date1, _, Date1Extra),
      RDate1Extra = Date1Extra
    )
  ;
    (
      match_single_relative_date(Date1, _, RDate1Extra),
      option(value(Date1Extra),RDate1Extra)
    )
  ),
  option(value(Value1), Date1Extra),
  (
    (
      match_single_date(Date2,_, Date2Extra),
      RDate2Extra = Date2Extra
    )
  ;
    (
      match_single_relative_date(Date2,_, RDate2Extra),
      option(value(Date2Extra),RDate2Extra)
    )
  ),
  option(value(Value2), Date2Extra),
  atomic_list_concat([Value1,'.',Value2],DateRangeString),
  !.

% open range on the left .e.g. ":1425-12-10"
match_range([':'|Date1],DateString, range{subtype: to_only, to: RDate1Extra}):-
(
  (
    match_single_date(Date1, _, Date1Extra),
    RDate1Extra = Date1Extra
  )
;
  (
    match_single_relative_date(Date1, _, RDate1Extra),
    option(value(Date1Extra),RDate1Extra)
  )
),
  option(value(Value1), Date1Extra),
  Date is Value1 - 0.1,
  atom_number(DateString,Date),
  !.

match_range(List,DateString, range{subtype: from_only, from: RDate1Extra}):-
  append(Date1,[':'],List),
  (
    (
      match_single_date(Date1, _, Date1Extra),
      RDate1Extra = Date1Extra
    )
 ;
  (
    match_single_relative_date(Date1, _, RDate1Extra),
    option(value(Date1Extra),RDate1Extra)
  )
),
  option(value(Value1), Date1Extra),
  Date is Value1 + 0.1,
  atom_number(DateString,Date),
  !.


/*
    get_y_m_d: get the date if it was entered with three elements (day/month/year).

*/

get_y_m_d(DATE):-
      belement_aspect(core,day,[A_DAY]),
      belement_aspect(core,month,[A_MONTH]),
      belement_aspect(core,year,[A_YEAR]),
      is_a_number(A_DAY,DAY),is_a_number(A_MONTH,MONTH),is_a_number(A_YEAR,YEAR),
      DATE is YEAR*10000+MONTH*100+DAY.


is_a_number(Number,Number):-number(Number),!.
is_a_number(Atom,Number):-atom_number(Atom,Number),!.

%!  get_group_id(+Group,+DefaultId,?GeneratedId) is det.
%   Generates a unique id for Group is the current context.
%   If the group contains an element specifying the id (/id=xpto), take that value
%   else
%   Generate a new id by taking the id of the ancestor (enclosing) group and a local
%   id for this group generated by taking the first 3 letters of the base class and a counter
%   Then add a prefix if it was specified in the starting kleio group and a postfix based on the
%   translation count. Prefixes work like namespaces and translation count postfix allow for the insertion
%   of new groups after the first translation without clashing with previously generated ids.
%
% TODO: is this gactoxml specific?

get_group_id(Group,__BuiltinID,Id):- % if the element id exists, take it as the id
  clio_aspect(core,id,ID),ID \= [],!,
  list_to_a0(ID,Id0),
  check_id_prefix(Id0,Id),
  get_prop(gline,number,L),
  get_prop(gline,text,Line),
  (used_id(Id)->error_out(['** ID already used in this file: ',Id],
                          [line_number(L),line_text(Line)]);asserta(used_id(Id))),
  put_value(Group,Id),!.   % store the Id for latter reference

get_group_id(Group,__BuiltinID,Id):- % no explicit id. Get the id of the ancestor and concatenate
    get_ancestor(__Anc,Aid),!,
    clio_bclass(Group,Class),% by using the base class we allow group processors to reset the counters[??]
    sub_atom(Class,0,3,_,Seed),
      repeat,
    gensymbol_local(Seed,Gid),
    (atom(Aid)->AAid=Aid;term_to_atom(Aid,AAid)),
     % we assumed that the ancestor''s id is prefixed so we do not check
    ((get_value(transcount,TransCount),TransCount>1) ->
        list_to_a0([AAid,'-',Gid,'-',TransCount],Id);
        list_to_a0([AAid,'-',Gid],Id)),
    put_value(Group,Id),
    \+ used_id(Id),
    assert(used_id(Id)),!.

get_group_id(Group,Id,Id0) :- % this takes the auto generated
  check_id_prefix(Id0,Id),
  put_value(Group,Id),
  \+ used_id(Id),
  assert(used_id(Id)),!. % If everything fails accept the kleio auto id

/*
  Check if there is a name space prefix to ad to the id.
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
  ),%writeln(clio_anc(Anc,Aid)),
!.

/*

link_ancestor: Generate a link between the ancestor and the current group
  ===============
  DEPRECATED : now intergroup relations are handled directly in Group, by the elemet "inside".
*/
link_ancestor(G,ID):-!,
    gensymbol_local(rela,Rid),
   get_ancestor(__AGroup,Gid),			%writeln( get_ancestor(AGroup,Gid)),% get the ancestor
   xml_write(['<RELATION ID="',Gid-Rid,'" ORG="',ID,'" DEST="',Gid,'" TYPE="contained-in" VALUE="',G,'"/>']),
    xml_nl.



/*

  Automatic relation handling DEPRECATED THIS IS NOT USED ANYMORE.

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

remove_from_autorels_groups(CutLevel,[(__Group,__Id,__Anc,Level)|Rest],Final) :-
  Level @> CutLevel,  %report([writeln('  **removing '-(Group,Id,Anc,Level))]),
  remove_from_autorels_groups(CutLevel,Rest,Final).

remove_from_autorels_groups(CutLevel,[(__,Id,Anc,Level)|Rest],[(_,Id,Anc,Level)| Final]) :-
  remove_from_autorels_groups(CutLevel,Rest,Final).

/*
   the scope rules for automatic relations are:
        The related persons must have the same ancestor
        or one must be the ancestor of the other
*/
check_arel_scope((__P1,_,ANC),(__P2,_,ANC)):-!.
check_arel_scope((__P1,ID,_),(__P2,_,ID)):-!.
check_arel_scope((__P1,_,ID),(__P2,ID,_)):-!.
%check_arel_scope(P1,P2) :- writeln('** not in scope '-P1-P2),!,fail.

/*

Relations are defined based on paths
A path is a list of groupName(ID) | sequence(C) | group(Name,ID) | extends(Class,ID) | clause(C).
ID is usually a variable that is bound to actual IDs when the match is done
sequence(C) matches a sequence of groups including a empty one
group(Name,ID) matches Name(ID) it is useful to extract a Group name.
extends(Class,ID) will match any Name(ID) if Name is a group that extends Class.
clause(C) will call the C predicate in Prolog

example (using the sequence(P) place holder to signify the same context:
group_path(P),
path_matching(P,[sequence(Prefix),n(N),mulher(M)]),
writeln(relation/kin/husband/N/M),fail.

This generates functional relation for heads of households.
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

/* this is the new  mechanism for inferencing relations.
It is done at act scope level.
*/
do_auto_rels2:-get_value(autorelmode,N), N \= 2,!.
do_auto_rels2:-
  %report([writeln('**** auto rels 2 STARTED.')]),
  apply_inference_rules,
  %clean_paths,
  !.
do_auto_rels2:-
  %report([writeln('**** auto rels 2 FINISHED.')]),
  !.

clean_paths:-
  group_path(P),
  clean_paths(P).

clean_paths(Pattern):-
  % report([writeln('***** auto rels 2 clean_paths for'-Pattern)]),
  group_path(P),
  path_matching(P,Pattern),
  % report([writeln('***** auto rels 2 removing path '-P)]),
  retract(group_path(P)),
  fail.
clean_paths(_):-!.

clean_attribute_cache:-
    retract(attribute_cache(__D,__LSID,__T,__V)),
    % report([writeln('***** removing attribute cache '-D-T-V)]),
    fail.
clean_attribute_cache:-!.


apply_inference_rules:-
  if Condition then Action,
  Action \= newscope,
  condition_test(Condition),
  %report([writeln('***** auto rels 2 true condition:'-Condition)]),
  do_actions(Action),
  fail.

apply_inference_rules:-
  if Condition then newscope,
  % report([writeln('***** auto rels 2 new scope testing for:'-Condition)]),
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
  % report([writeln('***** auto rels 2 new scope')]),
  clean_paths([kleio(_),sequence(__S)])
  ,clean_attribute_cache.


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

path_matching(Path,[attribute(Entity,Attribute,Value)|MorePattern],FinalPath,FinalPattern) :-
  clause(attribute_cache(Entity,_,[Attribute],[Value]),true),
  path_matching(Path,MorePattern,FinalPath,FinalPattern).

path_matching([Group|MorePath],[Group|MorePattern],FinalPath,FinalPattern):-!,
  path_matching(MorePath,MorePattern,FinalPath,FinalPattern).

export_auto_rel((__Origin,OriginID),(__Destination,DestinationID),Type,Value) :-!,
  rch_class(relation,Class,Super,Table,Mapping),
  ensure_class(relation,Class,Super,Table,Mapping),
  gensymbol_local(rela,Rid),
  % TODO: #29 AncID = OriginID
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
  xml_write(['    <ELEMENT NAME="inside" CLASS="inside"><core>',OriginID,'</core></ELEMENT>']),
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
  % report([writelist0ln(['   ** auto rel: ',Type,'/',Value,' from ',OriginID,  ' to ',DestinationID])]),
  !.

%! export_auto_attribute(+Entity,+Type,+Value) is det.
%   Export an automatic attribute for Entity with Type and Value.
%   This is used to export inferred attributes.
%
%   Shortned version of export_auto_attribute/9

export_auto_attribute(Entity,Type,Value) :-
  export_auto_attribute(Entity,
                        'ls',
                        'attribute',
                        Type,
                        '',
                        '',
                        Value,
                        '',
                        '').

%! export_auto_attribute(+Entity,+GroupName,+Class,+Type,+TypeComment,+TypeOriginal,+Value,+ValueComment,+ValueOriginal) is det.
%
% @arg Entity is the entity to which the attribute is attached
% @arg GroupName is the name of the group used for the attribute (e.g. ls,attr,...)
% @arg Class is the database (POM) class corresponding to the attribute group
% @arg Type is the type of the attribute
% @arg TypeComment is the comment for the type of the attribute
% @arg TypeOriginal is the original text for the type of the attribute
% @arg Value is the value of the attribute
% @arg ValueComment is the comment for the value of the attribute
% @arg ValueOriginal is the original text for the value of the attribute
%
%   Export an automatic attribute for Entity with Type and Value.
export_auto_attribute(Entity,
                        GroupName,
                        Class,
                        Type,
                        TypeComment,
                        TypeOriginal,
                        Value,
                        ValueComment,
                        ValueOriginal) :-
        rch_class(GroupName,Class,Super,Table,Mapping),
        ensure_class(GroupName,Class,Super,Table,Mapping),
        AncId = Entity,
        gensymbol_local(atra,Rid),
        AId = AncId-Rid,
        (get_date(Date1) -> Date = Date1 ; get_prop(act,date,Date); Date = '00000000'),
        xml_nl,
        inccount(group,GroupNumber),
        clio_path(P),
        length(P,CurrentLevel),
        ThisLevel is CurrentLevel+1,
        get_prop(line,number,N),
        AttrGname = GroupName,
        AClass = Class,
        Inside = Entity,
        % in the future use this dictionary to pass to different exporters
        _AInfo = attr{gid:AId,
                  name:AttrGname,
                  order:GroupNumber,
                  level:ThisLevel,
                  class: AClass,
                  line: N,
                  inside: Inside,
                  type: type{
                          core:Type,
                          comment:TypeComment,
                          original:TypeOriginal
                          },
                  value: value{
                    core:Type,
                    comment:ValueComment,
                    original:ValueOriginal
                    }
                  },
        xml_write(['<GROUP ID="',AId,'" NAME="',GroupName,'"  ORDER="',GroupNumber,'" LEVEL="',ThisLevel,'" CLASS="attribute" LINE="',N,'">']),
        xml_nl,
        xml_write(['    <ELEMENT NAME="line" CLASS="line"><core>',N,'</core></ELEMENT>']),
        xml_nl,
        xml_write(['    <ELEMENT NAME="groupname" CLASS="groupname"><core>',GroupName,'</core></ELEMENT>']),
        xml_nl,
        xml_write(['    <ELEMENT NAME="inside" CLASS="inside"><core>',Inside,'</core></ELEMENT>']),
        xml_nl,
        xml_write(['    <ELEMENT NAME="class" CLASS="class"><core>',AClass,'</core></ELEMENT>']),
        xml_nl,
        xml_write(['    <ELEMENT NAME="order" CLASS="order"><core>',GroupNumber,'</core></ELEMENT>']),
        xml_nl,
        xml_write(['    <ELEMENT NAME="level" CLASS="level"><core>',ThisLevel,'</core></ELEMENT>']),
        xml_nl,
        xml_write(['    <ELEMENT NAME="type" CLASS="type">']),
        aspect_to_xml(core,Type,XType),xml_write(XType),
        aspect_to_xml(comment,TypeComment,XTypeComment), xml_write(XTypeComment),
        aspect_to_xml(original,TypeOriginal,XTypeOriginal), xml_write(XTypeOriginal),
        xml_write(['</ELEMENT>']),

        xml_nl,
        xml_write(['    <ELEMENT NAME="value" CLASS="value">']),
        aspect_to_xml(core,Value,XValue), xml_write(XValue),
        aspect_to_xml(comment,ValueComment,XValueComment), xml_write(XValueComment),
        aspect_to_xml(original,ValueOriginal,XValueOriginal),xml_write(XValueOriginal),
        xml_write(['</ELEMENT>']),

        xml_nl,
        xml_write(['    <ELEMENT NAME="entity" CLASS="entity"><core>',Entity,'</core></ELEMENT>']),
        xml_nl,
        xml_write(['    <ELEMENT NAME="id" CLASS="id"><core>',AId,'</core></ELEMENT>']),
        xml_nl,
        xml_write(['    <ELEMENT NAME="date" CLASS="date"><core>',Date,'</core></ELEMENT>']),
        xml_nl,
        xml_write(['</GROUP>']),
        %report([writelist0ln(['   ** Automatic attribute: ',Entity,'/',Type,'/',Value])]),
        !.



/* The Auto relation clauses as stored by the Kleio structure parse are not in a convenient format
  for rapid retrieval. We load them in another form to speed retreival during processing
  */
index_auto_rel_clauses:-
  retractall(carel(_,_,_,_)),
  log_debug('translate: ** Indexing automatic relation clauses',[]),
  clio_super('auto-relation',ARGroup),
  clio_group_param(ARGroup,pars,[Group,Group2]),
  clio_group_param(ARGroup,certe,[Type,Value]),
  assert(carel(Group,Group2,Type,Value),true),fail.

index_auto_rel_clauses:-
  log_debug('translate: ** Automatic relation clauses indexed.',[]),!.


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
cache_arel(Person,Person2,__Type,__Value):-
  clause(carelnot(Person,Person2),true),!,fail.
cache_arel(Person,Person2,Type,Value):-
  autor_rel_for_group(Person,Person2,Type,Value),!,
  assert(carel(Person,Person2,Type,Value)).
cache_arel(Person,Person2,_,_) :-!,
  assert(carelnot(Person,Person2)),fail.
/*

        This is a utility to generate auto-relation definitions for families for the gacto.str file.
        Usage: make_auto_relation_clauses(Actor,Sex,Levels).

        For different languages check the prefix while calling make_parent_clause and check inside that predicate
        the chars used for each sex and the kinship terms.
*/
make_auto_relation_clauses(__Seed,_,0):-!.
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
make_parent_clause(Seed,__Sex,Prefix,Parent):-
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
make_auto_relation_clauses2(__Seed,_,0):-!.
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

make_parent_clause2(Seed,__Sex,Prefix,Parent):-
  concat(Prefix,Seed,Parent),
  ( Prefix = p -> Relation=pai; Relation=mae),
  writelist0ln(['if [sequence(_),',Seed,'(Son),',Parent,'(Parent)]']),
  writelist0ln(['then relation(parentesco,',Relation,',Parent,Son).']),
  writelist0ln(['if [sequence(Path),',Seed,'(Son)] and [sequence(Path),',Parent,'(Parent)] ']),
  writelist0ln(['then relation(parentesco,',Relation,',Parent,Son).']),
  !.

make_couple_clause2(__Seed,Husband,Wife) :-
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
  isNewMappingMode,!,
  %writeln('** using new style mapping'),
  groupToClass(Group,Class,Super,Table).

rch_class(Group,Class,Super,Table,Mapping):-
  %writeln('** using old style mapping'),
  rch_class1(Group,Class,Super,Table,Mapping),!.

/* detects the new mapping mode that uses mappings and class operators **/
isNewMappingMode:-
  clause(mapping _ to class _, _),!.

/* DEPRECATED **/
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

/* returns the Attribute in a class that corresponds to an element
   in a group

   This is where the very important mappig between Kleio Group Elements
   and Database attributes is done.
   */
elementMapping(GroupClass,Element,Attr):-
    % fetch the attribute list of the group class
    % this is the sequence after "with attributes"
    % in mappings.pl
    % Case 1:
    %   the element name matches an attribute
    %   name in the attribute list of the
    %   group class in mappings.pl
    class_attributes(GroupClass,Attributes),
    rch_get_attribute(Element,Attributes,Attr).
elementMapping(GroupClass,Element,Attr):-
    % Case 2:
    %   the element extends another element
    %   that matches an attribute name in
    %   in the attribute list of the group class
    %   in mappings.pl
    class_attributes(GroupClass,Attributes),
    clio_element_extends(Element,SElement),
    rch_get_attribute(SElement,Attributes,Attr).
elementMapping(GroupClass,Element,Attr):-
   rch_class(_,GroupClass,Super,_,_),
   class_attributes(Super,Attributes),
   rch_get_attribute(Element,Attributes,Attr).

/* the class of an element in a group (baseclass in the mapping) */
elementClass(relation,Element,undef):-  % optimizing shortcut for dest names in rel$ to avoid search to matching class which is undefined
   clio_element_extends(Element,destname),!.

elementClass(GroupClass,Element,Class):-
  elementMapping(GroupClass,Element,Attr),
  atr_select(baseclass,Attr,Class),!.

rch_get_attribute(Name, Attr and _, Attr):-
  % this matches the element name with the first
  % value of the attributes list in mappings.pl
  % (the value before "column")
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
       rch_element_class(Atr,Base,Column,Type,Length,Precision,__EMapping),
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
ensure_class(__GroupName,RClass,Super,__Table,__Mapping):-
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

/* DEPRECATED: this is the old style mapping output predicate : new style mappings use the rclass2_xml/4 */

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
                            '" PKEY="',PK,'" ></ATTRIBUTE>']),
  xml_nl,!.

/* DEPRECATED: see rattributes2_xml*/
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
                            '" PKEY="',PK,'" ></ATTRIBUTE>']),
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
/**
* belement_aspect(+Aspect:atom,+BaseElement:atom,-Content:list) is det
* @deprecated Use externals:clio_belement_aspect/3 instead.
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
  get_ancestor(__Anc,AncestorID),
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

elements_to_xml(__Class) :- writeln('** element_to_xml failed!'),!.


ielements_to_xml(Class,[Element|Rest]):-
  Element =.. [Name,Core,Original,Comment],
  iel_to_xml(Class,Name,Core,Original,Comment),
  ielements_to_xml(Class,Rest),!.

ielements_to_xml(__Class,[]):-!.

els_to_xml(Class,[ El | MoreEls ]) :-
  el_to_xml(Class,El),
  els_to_xml( Class,MoreEls),!.

els_to_xml(__Class, []) :- !.

el_to_xml(__Class,id):-!.  /* we skip id elements because they were exported previously and part of the builtin els */
el_to_xml(relation,iddest):-
  clio_aspect(core,iddest,Core0),
  !,
  log_debug('translate:    ** checking rel dest for prefix ~w~n', [Core0]),
  IdDest = Core0,
  (
      is_list(IdDest)
        -> list_to_a0(IdDest,SID0)
        ; SID0 = IdDest
  ),
  check_id_prefix(SID0,Core),
  el_to_xml2(relation,iddest,Core).

el_to_xml(GClass,El) :-
    clio_aspect(core,El,Core),!,
    el_to_xml2(GClass,El,Core).

el_to_xml2(GClass,El,Core):-
    %writeln(GClass-El-Core),
    clio_aspect(original,El,Original),
    clio_aspect(comment,El,Comment),
    aspect_to_xml(core,Core,CoreXML),
    aspect_to_xml(original,Original,OriginalXML),
    aspect_to_xml(comment,Comment,CommentXML),!,
     /* If a group element is not mapped to an attribute of the group class then
        it is not possible to find the elementClass of the group element.
        We use undef to mark those. This is not an error because we use elements that
        do not correspond to database fields. For instance in acts we have elements for
        day, month and year, but the database stores the information as a single column for
        date. The elements are combined and the date value computed during export.
        Note that DBClass in idb only fetches the group element that are mapped to class attributes */
      calc_length(Core,ACore,ALength),
      (elementClass(GClass,El,Class)  ->
          (
            (elementMapping(GClass,El,Attr), atr_select(colsize,Attr,Length))
          ;
            (Length=ALength)
        )
        ;
          (Class=undef,Length=ALength)
      ),
      (Class \= undef ->
        ((ALength @> Length) ->
          warning_out(['Element value exceeds database field length: ',
                      El,'=',ACore,'. Size:',ALength,' limit: ',Length])
          ;
          (true)
        )
      ; true
    ),
     xml_write( [ '   <ELEMENT NAME="' , El , '" CLASS="',Class,'">' ]),
     xml_nl,
     xml_write(['   '|CoreXML]),
    (OriginalXML \= [] -> (xml_write(['   '|OriginalXML]), xml_nl);true),
    (CommentXML \= [] -> (xml_write(['   '|CommentXML]), xml_nl);true),
    xml_write( [ '   </ELEMENT>' ]),
     xml_nl.

calc_length(Core,Core,Length):-
      atomic(Core),
      atom_length(Core,Length).
calc_length(Core,Atom,Length):-
  is_list(Core),
  atomic_list_concat(Core, Atom),
  atom_length(Atom,Length).

calc_length(mult(Core),Atom,Length):-
  mult_to_list(Core,List),
  atomic_list_concat(List, Atom),
  atom_length(Atom,Length).

% not sure if this is needed
 calc_length(mult(Core),Core,Length):-  % this is a hack to avoid the mult_to_list when the mult is a single element
  atomic(Core),
  atom_length(Core,Length).

%! flatten_multiple_entry(+Value,-List:list) is det.
%
%  If Value is a multiple entry, then List is unified with the list of value separated by ';'.
flatten_multiple_entry(mult(Multi),List):-!, % this makes the predicate more flexible
  mult_to_list(Multi,List).
flatten_multiple_entry(List,List):-
  is_list(List),!.

mult_to_list([O],O):-!.
mult_to_list([],[]):-!.
mult_to_list([Oc1|More],List):-
    mult_to_list(More,MoreList),
    append(Oc1,[';'|MoreList],List).



iel_to_xml(GClass,Name,Core,Original,Comment) :-     %  used to output infered elements
      (isNewMappingMode ->
        (elementClass(GClass,Name,Class); Class = undef)
      ;
        rch_element_class(Name,Class,__col,__type,__length,__precision,__mapping)
      ),
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

iel_to_xml(GClass,Name,__Core,__Original,__Comment) :-
  error_out(('** INTERNAL ERROR: failed to export infered element '-GClass-Name)),!.

aspect_to_xml(__Aspect,[],[]):-!.
aspect_to_xml(__Aspect,mult([]),[]):-!.
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

%!  xml_write(+List:list) is det.
%
%  Write a list of items to the current XML file.

xml_write(List):-!,
  get_value(xmlfile,XMLFILE),
  with_output_to(string(S),xcleanwrite(List)),
  write(XMLFILE,S).

xml_nl:-!,
get_value(xmlfile,XMLFILE),
telling(OUT),
tell(XMLFILE),
nl,tell(OUT).

xml_close:-
    !,
    get_value(xmlfile,XMLFILE),
    log_debug('translate: xml data file --> ~w~n',[XMLFILE]),
    close_file(XMLFILE).

xcleanwrite([]):-!.
xcleanwrite([A|B]):-!,
   xcleanwrite(A),xcleanwrite(B).
xcleanwrite(Atomic):-
   atom(Atomic),!,
   atom_codes(Atomic,Chars),
   xclean_chars(Chars,CChars),
   atom_codes(CAtomic,CChars),
   write_term(CAtomic,[quoted(false)]).
xcleanwrite(Functor):-!,write_term(Functor,[quoted(false)]).


xclean_chars([],[]):-!.
  xclean_chars([47|More],[47|CMore]):-!, xclean_chars(More,CMore). % escape the slash
xclean_chars([C|More],[C|CMore]):-C > 19, !, xclean_chars(More,CMore).
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

/*
## History before 2007 (after see git history):

 First version August 1997.
 _inverted cronological order._

 Revision 1.2  2006/05/16 10:52:46  Joaquim
 update dos ficheiros prolog

 Revision 1.6  2006/03/27 20:46:32  jrc
 Added functionality for prefixing ids. Also created hooks for pretty printing of clio files.

 Revision 1.5  2005/12/29 09:59:40  jrc
 process_function in object groups is now Ok.
 Also changed the way by which the mapping of a element of a group was determined.
 Previously the mapping was found directly or by trying to see if the element extended another
 element for which a mapping was known. Now another techinque is used: the mappings is also
 searched in the super class of the group's class.

 Revision 1.4  2005/05/16 11:43:17  jrc
 Change to make_auto_relations2 in order to fix a bug where parents where always assumed as dependent
  groups of their children, which is not the case, most of the parente stuff is
  at the same level as their children. This causes under generation of relations
  between parents and children. It also changes the relation from parent to child
  instead of the opposite, which caused odd expressions since in portuguese the child
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
