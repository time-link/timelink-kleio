
:-module(clioPP,[clioPP/2,clioPP_close/0]).
/** <module>  Kleio "pretty print".

This code outputs the source file with the ids expanded in a sort of pretty print.

It is used by the export module gactoxml to produce a copy of the input file with explicit
ids. This is important to allow safe reimport of kleio files after first import.

In typical situations initital kleio transcriptions do not have explicit ids in common entities.
like people and objects. Normally acts have an explicit id atributed by the person doing the
transcription. 

The kleio translator autogenerates ids during the translation process. Translated documents
are then imported to the database. 

In order to allow chenges in the transacription after the data is imported in the databse it is
necessary that the auto generated ids are the same for the same entities.

This is done by generating a copy of the transcription with explicit ids. So that

==
kleio$gacto2.str/alterada no broser/translations=25
   fonte$teste01/1700/miscelanea

      bap$b1752-1/19/4/1752/?

         n$rosa do espirito santo

            pn$antonio goncalves roxo
               atr$naturalidade/gatoes, nossa senhora das virtudes
               atr$local de baptismo/gatoes, nossa senhora das virtudes
               atr$local de casamento/gatoes, nossa senhora das virtudes

            mn$maria jorge
               atr$naturalidade/gatoes, nossa senhora das virtudes
               atr$local de baptismo/gatoes, nossa senhora das virtudes
			   atr$local de casamento/gatoes, nossa senhora das virtudes
==

becomes

==
kleio$gacto2.str/alterada no broser/translations=25
   fonte$teste01/1700/miscelanea

      bap$b1752-1/19/4/1752/?

         n$rosa do espirito santo/id=b1752-1-per1

            pn$antonio goncalves roxo/id=b1752-1-per1-per2
               atr$naturalidade/gatoes, nossa senhora das virtudes
               atr$local de baptismo/gatoes, nossa senhora das virtudes
               atr$local de casamento/gatoes, nossa senhora das virtudes

            mn$maria jorge/id=b1752-1-per1-per3
               atr$naturalidade/gatoes, nossa senhora das virtudes
               atr$local de baptismo/gatoes, nossa senhora das virtudes
			   atr$local de casamento/gatoes, nossa senhora das virtudes
			   
==

The predicate clioPP/2 outputs the current group making the ids explicit.

It is used by the gactoxml.pl module to produce a copy of the translation with 
ids. This copy goest to a file with "ids" extension. If the translation is sucessfull 
(no errors) then the file being translated is renamed with extension "org" and the
"ids" file is renamed with the extension "cli". 

The first translation is kept with extension "org" for ever, just in case something
is lost during the process of inserting the explicit ids. Further translations will
keep the previous version with "old" extension.

It does not handle multiple entries very well. For instance:
If the original has this:
        locf$torre de vilela%bilela;e mais nao disse%e al nao disse
it outputs
   locf$torre de vilela; e mais nao disse; %bilela; e al nao disse; 

*/
:-use_module(externals).
:-use_module(persistence).
:-use_module(utilities).

%% clioPP(+G,+ID) is det.
%
% Pretty prints group G with id ID
%
clioPP(G,ID):-
	get_value(clioppfile,CPP),
	with_output_to(string(S),clioPP2(G,ID)),
	write(CPP,S).
	
	
clioPP2(G,ID):-
	clio_path(P),length(P,PI),
	Ident is PI*3,
	clio_group_param(G,locus,Locus),
	clio_bclass(G,Class),
	(member(Class,[attribute,relation,kleio,'historical-source']) -> 
		true
		;  
		nl
	),
	put_value(idout,false),  % used to check if the id of the group will be output
	tab(Ident),writelist0([G,'$']),% TODO: localization would happen here.
	clioPP_locus(0,Locus,N), % this can set idout to true
	clioPP_elements(N,Locus), % this can set idout to true
	 % check if we need to print the id, except for groups not relevant
	(member(Class,[attribute,relation,'group-element',kleio]) ->
		true 
		;
		( 
			(get_value(idout,false) ->  % if the id was not output so far, output it now.
				writelist0(['/id=',ID])
			;
				true
			) % id was previously output
		)

	),
	(G=='kleio' ->
	    (
	        (get_value(transcount,Transcount)->true;Transcount=1),
	        writelist0(['/translations=',Transcount]))
	    ;
	        true),

	nl.

clioPP2(G,I):-
	writelistln(['*******- problems in clioPP2: unexpected fail',group,G,id,I]),!.
		
	
clioPP_locus(PreviousElements,[L|Locus],OutputedElements):-	!,
	clio_aspect(core,L,Core0),
	aspect_to_PP(Core0,Core),
	length(Core,NCore),
	(NCore > 0 -> (	
		clioPP_check_id_element(L),
		(PreviousElements > 0 -> 
			write('/')
			;
			true),
		writelist0(Core)
		);
		true),
	clio_aspect(original,L,Org0),
	aspect_to_PP(Org0,Org),
	length(Org,NOrg),
	(NOrg > 0 -> writelist0(['%'|Org]);true),
	clio_aspect(comment,L,Comment0),
	aspect_to_PP(Comment0,Comment),
	length(Comment,NComment),
	(NComment > 0  ->  writelist0(['#'|Comment]);true),
	Next is PreviousElements + 1,
	clioPP_locus(Next,Locus,OutputedElements).
	
clioPP_locus(N,[],N):-!.
	
clioPP_elements(PreviousElements,Locus):-
	(PreviousElements > 0 -> put_value(doslash,true);put_value(doslash,false)),
	clio_elements(Els),
	list_to_set(Els,SEls),
	member(L,SEls),
	\+ member(L,[translations|Locus]),%we don't show here the locus element nor the transcount pseudo element
	clio_aspect(core,L,Core0),
	(get_value(doslash,true)->write('/');true),
	writelist0([L,'=']), % TODO: localization would happend here too.
	aspect_to_PP(Core0,Core),
	length(Core,NCore),
	(NCore > 0 -> 
		(writelist0(Core),
		clioPP_check_id_element(L))
		;
		true),
	clio_aspect(original,L,Org0),
	aspect_to_PP(Org0,Org),
	length(Org,NOrg),
	(NOrg > 0 -> writelist0(['%'|Org]);true),
	clio_aspect(comment,L,Comment0),
	aspect_to_PP(Comment0,Comment),
	length(Comment,NComment),
	(NComment > 0 -> writelist0(['#'|Comment]);true),
	put_value(doslash,true),
	fail.
clioPP_elements(_,_):-!.
	
	

clioPP_check_id_element(L):-
	clio_element_bclass(L,id),
	put_value(idout,true),!.
clioPP_check_id_element(_):-!.

%% clioPP_close is det.
%
% close clio pretty print file.
%
clioPP_close:-
           !,
   	get_value(clioppfile,FILE),
   	close_file(FILE).
   	
aspect_to_PP([],[]):-!.

aspect_to_PP(mult([]),[]):-!.

aspect_to_PP(mult([[]|More]),Mult):-
	aspect_to_PP(mult(More),Mult),!.

aspect_to_PP(mult([Oc1]),Xml1):-!,
      aspect_to_PP(Oc1,Xml1).

aspect_to_PP(mult([Oc1|More]),MultXML):-!,
      aspect_to_PP(Oc1,Xml1),
      aspect_to_PP(mult(More),XmlMore),
      append(Xml1,[';'|XmlMore],MultXML).

aspect_to_PP(Content,C):-
      (is_list(Content) -> C = Content; C = [Content]),!.

