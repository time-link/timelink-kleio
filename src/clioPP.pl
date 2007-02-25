/*
clioPP.pl clio pretty print.

This predicate outputs the source file with the ids expanded in a sort of pretty print.

It does not handle multiple entries very well. For instance:
If the original has this:
        locf$torre de vilela%bilela;e mais nao disse%e al nao disse
it outputs
   locf$torre de vilela; e mais nao disse; %bilela; e al nao disse; 
   
 
*/

clioPP(G,ID):-
	telling(Out),
	get_value(clioppfile,CPP),
	tell(CPP),
	clioPP2(G,ID),
	tell(Out).
	
	
clioPP2(G,ID):-
	clio_path(P),length(P,PI),
	Ident is PI*3,
	clio_group_param(G,locus,Locus),
	clio_bclass(G,Class),
	(\+ member(Class,[attribute,relation,kleio,'historical-source'])->nl;true),
	put_value(idout,false),
	tab(Ident),writelist0([G,'$']),
	clioPP_locus(0,Locus,N),
	clioPP_elements(N,Locus), 
	(\+ member(Class,[attribute,relation,'group-element',kleio])
		->( 
			(get_value(idout,false)->
				(remove_id_prefix(ID,NID),
				writelist0(['/id=',NID]))
			;
				true)
			)
		;true
		),nl.

clioPP2(G,I):-
	writelistln(['*******- problems in clioPP2: unexpected fail',group,G,id,I]),!.
		
	
clioPP_locus(PreviousElements,[L|Locus],OutputedElements):-	!,
	clioPP_check_id_element(L),!,
	clio_aspect(core,L,Core0),
	aspect_to_PP(Core0,Core),
	length(Core,NCore),
	(NCore > 0 -> (	
		(PreviousElements > 0 -> write('/');true),
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
	(NComment > 0 -> writelist0(['#'|Comment]);true),
	Next is PreviousElements + 1,
	clioPP_locus(Next,Locus,OutputedElements).
	
clioPP_locus(N,[],N):-!.
	
clioPP_elements(PreviousElements,Locus):-
	(PreviousElements > 0 -> put_value(doslash,true);put_value(doslash,false)),
	clio_elements(Els),
	list_to_set(Els,SEls),
	member(L,SEls),
	\+ member(L,Locus),
	clioPP_check_id_element(L),
	clio_aspect(core,L,Core0),
	(get_value(doslash,true)->write('/');true),
	writelist0([L,'=']),
	aspect_to_PP(Core0,Core),
	length(Core,NCore),
	(NCore > 0 -> writelist0(Core);true),
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
clioPP_check_id_element(_L):-!.
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

		