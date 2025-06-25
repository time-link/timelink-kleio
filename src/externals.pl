:-module(externals,[
		clio_isdoc/1,
		clio_path/1,
		clio_group/2,
		clio_anc/2,
		clio_elements/1,
		clio_aspect/3,
		clio_aspects/3,
		clio_data_file/1,
		clio_stru_file/1,
		clio_data_line/2,
		clio_report/1,
		clio_super/2,
		clio_extends/2,
		clio_bclass/2,
		clio_partof/2,
		clio_parts/2,
		clio_group_param/3,
		clio_element_param/3,
		clio_element_super/2,
		clio_element_extends/2,
		clio_element_bclass/2,
		clio_belement_aspect/3
	]).
/** <module> External interface (API) for the Clio export modules

The call back predicates made available to export modules by the Clio
translator are the following:

* Access to the current group
		* clio_isdoc(G)  G is the document group (top level group).
		* clio_path(P) returns the path of the current group,
                    in the form [doc(docId),group1(id1),....]
                    up to, but not including, the current group
		*  clio_group(G,ID) returns the current group
		* clio_anc(Group,ID) returns the ancestor of the current group
		* clio_elements(Els) returns the list of the names of available
                          elements in the current group
		* clio_aspect(Aspect,Element,Info) Aspect must be one of
                          (core, original, comment), Element the
                          name of an element. Info is a variable
                          that will be bound to the Aspect of Element
                          in the current group.
		* clio_aspects(Aspect,ElementList,InfoList) The same as clio_aspect
                         but works with a list of elements returning a list
                          of values.
		* clio_belement_aspect(Aspect,BaseElement,Content) get the aspect
						  corresponding to a baseclass element. This is
						  similar to clio_aspect but uses clio_element_extends
						  to find in the current group elements the base element
						  it is looking for. This is useful to find an expected
						  element even if the source used a different name for it.


* Processing information
		* clio_data_file(File) the current data file name.
		* clio_stru_file(File) the current stru file name.
		* clio_data_line(Number,Text) the number and text of the last data file
                             processed by the translator.
		* clio_report(R):- outputs to Clio current le. R is a list
                            standard prolog output predicates (write, nl,tab)
                            This list is executed by clio_report once sending
                            the output to the screen and a second time sending
                            the output to the current Clio report file.

*  Access to the data dictionary

		* clio_super(E,G) G extends directly E, e.g. E appears in fons/source parameter
								of G. E can be though as the super class of G.
		* clio_extends(G,E) Same as clio_super(E,G) but backtracks giving sources
                         further away from G.
		* clio_bclass(G,B) B is the base class of G, i.e. the root of the source/fons
									hierarchy for G. If G does not extend any other group
									then G is the base class of itself.
		* clio_partof(G,P) group P can be a part of G. Backtacks all possible
			groups.
		* clio_parts(G,List) List is the list of Groups that can be a part of G.
		* clio_group_param(Group,Param,Value) Returns Value given Group and Param
                            This allows inspection of the group definition as stored
                            during processing of the structure (added AUG 97)


		* clio_element_param(Element,Param,Value) Returns Value given Element and Param.
									 This allows inspection of the structure.
		* clio_element_super(E,G) G extends directly E, e.g. E appears in fons/source
                              parameter of G.
								of G. E can be though as the super class of G.
		* clio_element_extends(G,E) Same as clio_element_super(E,G) but backtracks giving sources
                         further away from G.
		* clio_element_bclass(G,B) B is the base class of G, i.e. the root of the source/fons
									hierarchy for G. If G that not extend any other group
									then G is the base class of itself.


 @tbd Get more consistent naming of predicates so that data dictionnary predicates are more easily
 		distinguished from current group predicates. Also this could be made available externally via rest.
*/

:-use_module(dataCDS).
:-use_module(dataDictionary).
:-use_module(utilities).
:-use_module(persistence).
:-use_module(reports).

% access to the current group
clio_isdoc(D):-isDoc(D).
clio_path(P):-getCDField(cpath,P).
clio_group(G,I):-getCDField(cgroup,G),getCDField(cgroupID,I).
clio_anc(A,ID):-getCDAnc(A,ID).
clio_elements(Els):-getCDElement_list(Els).
clio_aspect(A,E,I):-get_aspect(A,E,I).
clio_aspects(A,Es,Is):-get_aspects(A,Es,Is).
clio_data_line(N,L):- get_prop(line,number,N),
                      get_prop(line,text,L).

% processing information
clio_report(R):-report(R).
clio_data_file(File):-get_value(data_file,File).
clio_stru_file(File):-get_value(stru_file,File).

% Acessing the data dictionary
clio_group_param(G,P,V):-get_group_prop(G,P,V).
clio_super(Super,Group):-clio_group_param(Group,fons,Super).
clio_extends(Group,Super):-clio_group_param(Group,fons,Super).
clio_extends(Group,Super):-clio_group_param(Group,fons,F),clio_extends(F,Super).
% this defined the base class of a group, i.e.,
%   the root of the fons/source hierarchy for this group
% this means: get a super class of Group that has no super class.
clio_bclass(Group,Bclass) :-
		clio_extends(Group,Bclass),
		\+ clio_super(_,Bclass),!.
clio_bclass(Group,Group) :-!.

clio_partof(P,G) :- anc_of(G,P).
clio_parts(G,Ps) :- subgroups(G,Ps).

clio_element_param(E,P,V):-get_element_prop(E,P,V).
clio_element_super(Super,Element):-clio_element_param(Element,fons,Super).
clio_element_extends(Element,Super):-
		clio_element_param(Element,fons,Super).
clio_element_extends(Element,Super):-
		clio_element_param(Element,fons,F),
		clio_element_extends(F,Super).
							% this defined the base class of an element, i.e.,
							%   the root of the fons/source hierarchy for this element
clio_element_bclass(Element,Bclass) :-
		clio_element_extends(Element,Bclass),
		\+ clio_element_super(_,Bclass),!.
clio_element_bclass(Element,Element):-!.


/**
* clio_belement_aspect(+Aspect:atom,+BaseElement:atom,-Content:list) is det
*
* @param Aspect atom, one of (core, original, comment)
* @param BaseElement atom, the name of a base element, e.g. xsame_as
* @param Content list, the content of the aspect for the base element
*
* A base element is an element that is extended by other elements and
* is normally associated with specific semantics during translation.
* E.g. xsame_as (external same as) is assumed to contain the id of a
* group that refers to the same entity as the current group in another file.
*
* Structure files allow the definition of elements that extend other elements
* through the 'fons/source' parameter. This is used to make a version of an element
* localized to a certain language, or to improve legibility of the transcription
* so
* ==
* element name="xmesmo_que" source="xsame_as"
* ==
* means that the element 'xmesmo_que' is a version of 'xsame_as' in Portuguese.
*
* In other words, the element inheritance mechanism allows
* for the translator to find an expected element even if the source used
* a different name for it.
*
* `clio_belement_aspect/3` is similar do `clio_aspect/2` but uses
* `clio_element_extends/2` to find in the current group elements
*  the base element it is looking for.
*
*/
clio_belement_aspect(Aspect,Element,Content) :-
          clio_elements(Els),						% list current elements
         % writeln(clio_elements(Els)),
          member(Element,Els),	!,				% get one of them
          clio_aspect(Aspect,Element,Content).      % if it does return aspect

clio_belement_aspect(Aspect,BaseElement,Content) :-
	clio_elements(Els),						% list current elements
     % writeln(clio_elements(Els)),
    member(Element,Els),					% get one of them
    clio_element_extends(Element,BaseElement),!, % check if it extends the base el
    clio_aspect(Aspect,Element,Content).      % if it does return aspect

clio_belement_aspect(_,_,[]):- !.




% vim: filetype=prolog ts=3
% $Date$
% $Author$
% $Id$
% $Log: externals.pl,v $
% Revision 1.2  2006/05/16 10:52:44  Joaquim
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
% Revision 1.10  2001/03/04 02:18:01  jrc
% Typo corrected in comments.
%
% Revision 1.9  2001/02/07 14:27:50  jrc
% Interim release. Better compliance to the XML export specs.
%
% Revision 1.8  2001/01/18 22:26:45  jrc
% clio_bclass and clio_element_bclass added.
%
% Revision 1.7  2001/01/15 18:43:37  jrc
% Added predicates to deal with element hierarchies
% (clio_element_super, clio_element_extends)
%
% Revision 1.6  2001/01/14 23:06:16  jrc
% Small bug corrected in clio_stru_file and clio_data_file.
%
% Revision 1.5  2001/01/14 15:48:08  jrc
% clio_stru_file added.
%
% Revision 1.4  2000/12/17 15:52:18  jrc
% Extra CRs removed with :%s/^M//g in vim
%
% Revision 1.3  2000/12/17 15:45:32  jrc
% clio_super/2 added
%
% History:
%   This modules documents features in use before but not documented
%   previously. The call back predicates of the translator received
%   new names and were enumerated for the first time here. Before
%   now the export modules called the Clio translator add hoc
%   using the source listing to detect which predicates to call
%
% created January 1993.
% clio_isdoc(D), clio_extends(G,C) added Sept 2000.
% clio_super(E,G) added December 2000.
% Under source control from December 2000 (see $Log: externals.pl,v $
% Under source control from December 2000 (see Revision 1.2  2006/05/16 10:52:44  Joaquim
% Under source control from December 2000 (see update dos ficheiros prolog
% Under source control from December 2000 (see
% Under source control from December 2000 (see Revision 1.1  2004/04/08 14:45:24  ltiago
% Under source control from December 2000 (see Source code has passed from Joaquim to Tiago.
% Under source control from December 2000 (see Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
% Under source control from December 2000 (see File build.xml, web.xml and velocity.properties were changed
% Under source control from December 2000 (see
% Under source control from December 2000 (see Revision 1.1.1.1  2001/04/19 23:25:43  jrc
% Under source control from December 2000 (see New Repository.
% Under source control from December 2000 (see
% Under source control from December 2000 (see Revision 1.1.1.1  2001/04/18 23:34:39  jrc
% Under source control from December 2000 (see CVS repository moved from llinux to MacOSX.
% Under source control from December 2000 (see
% Under source control from December 2000 (see Revision 1.10  2001/03/04 02:18:01  jrc
% Under source control from December 2000 (see Typo corrected in comments.
% Under source control from December 2000 (see
% Under source control from December 2000 (see Revision 1.9  2001/02/07 14:27:50  jrc
% Under source control from December 2000 (see Interim release. Better compliance to the XML export specs.
% Under source control from December 2000 (see
% Under source control from December 2000 (see Revision 1.8  2001/01/18 22:26:45  jrc
% Under source control from December 2000 (see clio_bclass and clio_element_bclass added.
% Under source control from December 2000 (see for subsequent history %








