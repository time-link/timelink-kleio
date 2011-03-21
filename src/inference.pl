:-op(230,fx,if).
:-op(220,xfy,then).
:-op(210,xfy,and).
:-op(210,xfy,or).
/*
    This file contains the inference rules for automatic relations and attributes.

    This corresponds to the new model of inferencing,
    based on path_matching predicate.

    $Id$
    $Log: inference.pl,v $
    Revision 1.2  2006/05/16 10:52:47  Joaquim
    update dos ficheiros prolog

    Revision 1.5  2006/03/27 20:43:39  jrc
    Added inference rules for "n" to account for female "n" actors.

    Revision 1.4  2005/06/05 22:10:20  jrc
    changed to make baptisms of type "b" capable of automatic processing of "procurador-pad" and "procurador-mad". corrected a bug regarding this in baptisms type "bap".

    Revision 1.3  2005/05/16 11:36:07  jrc
    New rules handle bug in the generation of sutmatic kinship in marriages and also cleanup parent son relations, that are now recorded on the parent side, and not on the child side.

    Revision 1.2  2005/03/10 14:42:16  joaquim
    snapshot commit for purpose of moving the cvs directory.

    Revision 1.1  2004/04/08 14:45:24  ltiago
    Source code has passed from Joaquim to Tiago.
    Since that, source was recofigured to work on a windows platform under Apache Tomcat 5.0.18
    File build.xml, web.xml and velocity.properties were changed

    Revision 1.2  2002/09/22 18:08:13  jrc
    Rules updated.

    Revision 1.1  2002/06/01 12:08:43  jrc
    new style inference rules.

    $Author$

    an inference rule:
     if PATH then ACTION
     or
     if PATH and PATH or PATH then ACTION and ACTION

     where
        PATH is a list of with the following elements:

        sequence(C)
            matches a sequence of groups including an empty one
        group(Name,ID)
            matches Name(ID) it is useful to extract a Group name.
        extends(Class,ID)
            will match any Name(ID) if Name is a group that extends Class.
        clause(C)
            will call the C predicate in Prolog
            
        ACTION is 
        relation(type,value,idorigin,iddestinhation) -- generate a relation
        attribute(id,type,value). -- generate an attribute
        newscope.  -- clean current scope (forgets previous actor and objects).
   */
   % male actor and direct parents
   if [sequence(_),extends(actorm,N),pai(P)]
   then
        relation(parentesco,pai,P,N).
        
   if [sequence(_),extends(actorm,N),mae(M)]
   then
        relation(parentesco,mae,M,N). 


   % female actor and direct parents
   if [sequence(_),extends(actorf,N),pai(P)]
   then
        relation(parentesco,pai,P,N).
        
   if [sequence(_),extends(actorf,N),mae(M)]
   then
        relation(parentesco,mae,M,N).            
   
   %direct parents as a couple
   if [sequence(Path),pai(P)] and [sequence(Path),mae(M)]
   then
        relation(parentesco,marido,P,M) and 
        attribute(P,ec,c) and
        attribute(M,ec,c).
        
    % sons and daughters
    if [sequence(_Path),extends(person,N),filho(F)]
    then
        relation(parentesco,filho,F,N).
        
    if [sequence(_Path),extends(person,N),filha(F)]
    then
        relation(parentesco,filha,F,N).

   % male actor and wife (up to three previous marriages.)

    if [sequence(_),extends(actorm,N),mulher(M)]
    then
        relation(parentesco,marido,N,M) and
        attribute(N,ec,c) and
        attribute(M,ec,c).

    if [sequence(_),extends(actorm,N),mulher1(M)]
    then
        relation(parentesco,foi-marido,N,M) and
        attribute(M,morta,antes).

    if [sequence(_),extends(actorm,N),mulher2(M)]
    then
        relation(parentesco,foi-marido,N,M) and
        attribute(M,morta,antes).

    if [sequence(_),extends(actorm,N),mulher3(M)]
    then
        relation(parentesco,foi-marido,N,M) and
        attribute(M,morta,antes).

    % in marriages we have to do it differently because there are all at the same level.

    if [sequence(Path),cas(C),noivo(N)] and [sequence(Path),cas(C),mulher1(M)]
    then
        relation(parentesco,foi-marido,N,M) and
        attribute(M,morta,antes).

    if [sequence(Path),cas(C),noivo(N)] and [sequence(Path),cas(C),mulher2(M)]
    then
        relation(parentesco,foi-marido,N,M) and
        attribute(M,morta,antes).

    if [sequence(Path),cas(C),noivo(N)] and [sequence(Path),cas(C),mulher3(M)]
    then
        relation(parentesco,foi-marido,N,M) and
        attribute(M,morta,antes).




    % female actor and husband (up to four previous marriages)
    if [sequence(_),extends(actorf,N),marido(M)]
    then relation(parentesco,mulher,N,M) and
                    attribute(N,ec,c) and
                    attribute(M,ec,c).

    if [sequence(_),extends(actorf,N),marido1(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    if [sequence(_),extends(actorf,N),marido2(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    if [sequence(_),extends(actorf,N),marido3(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    if [sequence(_),extends(actorf,N),marido4(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    % special case where n is used as a female we map husband (up to four previous marriages)
    if [sequence(_),n(N),marido(M)]
    then relation(parentesco,mulher,N,M) and
                    attribute(N,ec,c) and
                    attribute(M,ec,c).

    if [sequence(_),n(N),marido1(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    if [sequence(_),n(N),marido2(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    if [sequence(_),n(N),marido3(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    if [sequence(_),n(N),marido4(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    % in marriages we have to do it differently because there are all at the same level.
     if [sequence(Path),cas(C),noiva(N)] and [sequence(Path),cas(C),marido1(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    if [sequence(Path),cas(C),noiva(N)] and [sequence(Path),cas(C),marido2(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    if [sequence(Path),cas(C),noiva(N)] and [sequence(Path),cas(C),marido3(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    if [sequence(Path),cas(C),noiva(N)] and [sequence(Path),cas(C),marido4(M)]
    then
        relation(parentesco,foi-mulher,N,M) and
        attribute(M,morto,antes).

    % noiva e noiva (groom and bride in portuguese marriages)
    if [sequence(X),cas(C),noivo(Noivo)] and
       [sequence(X),cas(C),noiva(Noiva)]
    then
        relation(parentesco,marido,Noivo,Noiva) and
        attribute(Noivo,ec,c) and
        attribute(Noiva,ec,c).

    % pnoiva,mnoiva, pnoivo,mnoivo.
        
 /* ========================================== */

 /* BEGIN Auto-relation pai (levels 4) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pai(Son),ppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pai(Son)] and [sequence(Path),ppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pai(Son),mpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pai(Son)] and [sequence(Path),mpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppai(Husband)] and [sequence(Path),mpai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation ppai (levels 3) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),ppai(Son),pppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),ppai(Son)] and [sequence(Path),pppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),ppai(Son),mppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppai(Son)] and [sequence(Path),mppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pppai(Husband)] and [sequence(Path),mppai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation pppai (levels 2) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pppai(Son),ppppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pppai(Son)] and [sequence(Path),ppppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pppai(Son),mpppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pppai(Son)] and [sequence(Path),mpppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppppai(Husband)] and [sequence(Path),mpppai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation ppppai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),ppppai(Son),pppppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),ppppai(Son)] and [sequence(Path),pppppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),ppppai(Son),mppppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppppai(Son)] and [sequence(Path),mppppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pppppai(Husband)] and [sequence(Path),mppppai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation ppppai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mpppai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mpppai(Son),pmpppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mpppai(Son)] and [sequence(Path),pmpppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mpppai(Son),mmpppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mpppai(Son)] and [sequence(Path),mmpppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmpppai(Husband)] and [sequence(Path),mmpppai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation mpppai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation pppai (levels 2) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mppai (levels 2) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mppai(Son),pmppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mppai(Son)] and [sequence(Path),pmppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mppai(Son),mmppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mppai(Son)] and [sequence(Path),mmppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmppai(Husband)] and [sequence(Path),mmppai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation pmppai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pmppai(Son),ppmppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pmppai(Son)] and [sequence(Path),ppmppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pmppai(Son),mpmppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmppai(Son)] and [sequence(Path),mpmppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmppai(Husband)] and [sequence(Path),mpmppai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation pmppai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mmppai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mmppai(Son),pmmppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mmppai(Son)] and [sequence(Path),pmmppai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mmppai(Son),mmmppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mmppai(Son)] and [sequence(Path),mmmppai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmppai(Husband)] and [sequence(Path),mmmppai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation mmppai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation mppai (levels 2) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation ppai (levels 3) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mpai (levels 3) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mpai(Son),pmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mpai(Son)] and [sequence(Path),pmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mpai(Son),mmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mpai(Son)] and [sequence(Path),mmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmpai(Husband)] and [sequence(Path),mmpai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation pmpai (levels 2) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pmpai(Son),ppmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pmpai(Son)] and [sequence(Path),ppmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pmpai(Son),mpmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmpai(Son)] and [sequence(Path),mpmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmpai(Husband)] and [sequence(Path),mpmpai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation ppmpai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),ppmpai(Son),pppmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),ppmpai(Son)] and [sequence(Path),pppmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),ppmpai(Son),mppmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmpai(Son)] and [sequence(Path),mppmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pppmpai(Husband)] and [sequence(Path),mppmpai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation ppmpai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mpmpai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mpmpai(Son),pmpmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mpmpai(Son)] and [sequence(Path),pmpmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mpmpai(Son),mmpmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mpmpai(Son)] and [sequence(Path),mmpmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmpmpai(Husband)] and [sequence(Path),mmpmpai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation mpmpai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation pmpai (levels 2) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mmpai (levels 2) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mmpai(Son),pmmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mmpai(Son)] and [sequence(Path),pmmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mmpai(Son),mmmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mmpai(Son)] and [sequence(Path),mmmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmpai(Husband)] and [sequence(Path),mmmpai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation pmmpai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pmmpai(Son),ppmmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pmmpai(Son)] and [sequence(Path),ppmmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pmmpai(Son),mpmmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmpai(Son)] and [sequence(Path),mpmmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmmpai(Husband)] and [sequence(Path),mpmmpai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation pmmpai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mmmpai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mmmpai(Son),pmmmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mmmpai(Son)] and [sequence(Path),pmmmpai(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mmmpai(Son),mmmmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mmmpai(Son)] and [sequence(Path),mmmmpai(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmmpai(Husband)] and [sequence(Path),mmmmpai(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation mmmpai (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation mmpai (levels 2) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation mpai (levels 3) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation pai (levels 4) v2.1 16-5-2005
  ----------------------------------------- */





 /* ========================================== */

 /* BEGIN Auto-relation mae (levels 4) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mae(Son),pmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mae(Son)] and [sequence(Path),pmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mae(Son),mmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mae(Son)] and [sequence(Path),mmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmae(Husband)] and [sequence(Path),mmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation pmae (levels 3) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pmae(Son),ppmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pmae(Son)] and [sequence(Path),ppmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pmae(Son),mpmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmae(Son)] and [sequence(Path),mpmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmae(Husband)] and [sequence(Path),mpmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation ppmae (levels 2) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),ppmae(Son),pppmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),ppmae(Son)] and [sequence(Path),pppmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),ppmae(Son),mppmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmae(Son)] and [sequence(Path),mppmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pppmae(Husband)] and [sequence(Path),mppmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation pppmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pppmae(Son),ppppmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pppmae(Son)] and [sequence(Path),ppppmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pppmae(Son),mpppmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pppmae(Son)] and [sequence(Path),mpppmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppppmae(Husband)] and [sequence(Path),mpppmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation pppmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mppmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mppmae(Son),pmppmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mppmae(Son)] and [sequence(Path),pmppmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mppmae(Son),mmppmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mppmae(Son)] and [sequence(Path),mmppmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmppmae(Husband)] and [sequence(Path),mmppmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation mppmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation ppmae (levels 2) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mpmae (levels 2) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mpmae(Son),pmpmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mpmae(Son)] and [sequence(Path),pmpmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mpmae(Son),mmpmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mpmae(Son)] and [sequence(Path),mmpmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmpmae(Husband)] and [sequence(Path),mmpmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation pmpmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pmpmae(Son),ppmpmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pmpmae(Son)] and [sequence(Path),ppmpmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pmpmae(Son),mpmpmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmpmae(Son)] and [sequence(Path),mpmpmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmpmae(Husband)] and [sequence(Path),mpmpmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation pmpmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mmpmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mmpmae(Son),pmmpmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mmpmae(Son)] and [sequence(Path),pmmpmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mmpmae(Son),mmmpmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mmpmae(Son)] and [sequence(Path),mmmpmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmpmae(Husband)] and [sequence(Path),mmmpmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation mmpmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation mpmae (levels 2) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation pmae (levels 3) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mmae (levels 3) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mmae(Son),pmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mmae(Son)] and [sequence(Path),pmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mmae(Son),mmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mmae(Son)] and [sequence(Path),mmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmae(Husband)] and [sequence(Path),mmmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation pmmae (levels 2) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pmmae(Son),ppmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pmmae(Son)] and [sequence(Path),ppmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pmmae(Son),mpmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmae(Son)] and [sequence(Path),mpmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmmae(Husband)] and [sequence(Path),mpmmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation ppmmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),ppmmae(Son),pppmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),ppmmae(Son)] and [sequence(Path),pppmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),ppmmae(Son),mppmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmmae(Son)] and [sequence(Path),mppmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pppmmae(Husband)] and [sequence(Path),mppmmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation ppmmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mpmmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mpmmae(Son),pmpmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mpmmae(Son)] and [sequence(Path),pmpmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mpmmae(Son),mmpmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mpmmae(Son)] and [sequence(Path),mmpmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmpmmae(Husband)] and [sequence(Path),mmpmmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation mpmmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation pmmae (levels 2) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mmmae (levels 2) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mmmae(Son),pmmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mmmae(Son)] and [sequence(Path),pmmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mmmae(Son),mmmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mmmae(Son)] and [sequence(Path),mmmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmmae(Husband)] and [sequence(Path),mmmmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation pmmmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pmmmae(Son),ppmmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pmmmae(Son)] and [sequence(Path),ppmmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pmmmae(Son),mpmmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmmae(Son)] and [sequence(Path),mpmmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmmmae(Husband)] and [sequence(Path),mpmmmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation pmmmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mmmmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mmmmae(Son),pmmmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mmmmae(Son)] and [sequence(Path),pmmmmae(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mmmmae(Son),mmmmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mmmmae(Son)] and [sequence(Path),mmmmmae(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmmmae(Husband)] and [sequence(Path),mmmmmae(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation mmmmae (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation mmmae (levels 2) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation mmae (levels 3) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation mae (levels 4) v2.1 16-5-2005
  ----------------------------------------- */



/* ========================================== */

/* BEGIN Auto-relation mulher (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mulher(Son),pmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mulher(Son)] and [sequence(Path),pmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mulher(Son),mmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mulher(Son)] and [sequence(Path),mmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmulher(Husband)] and [sequence(Path),mmulher(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmulher (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmulher(Son),ppmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmulher(Son)] and [sequence(Path),ppmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmulher(Son),mpmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmulher(Son)] and [sequence(Path),mpmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmulher(Husband)] and [sequence(Path),mpmulher(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation ppmulher (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),ppmulher(Son),pppmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),ppmulher(Son)] and [sequence(Path),pppmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),ppmulher(Son),mppmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmulher(Son)] and [sequence(Path),mppmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pppmulher(Husband)] and [sequence(Path),mppmulher(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation ppmulher (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mpmulher (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mpmulher(Son),pmpmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mpmulher(Son)] and [sequence(Path),pmpmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mpmulher(Son),mmpmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mpmulher(Son)] and [sequence(Path),mmpmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmpmulher(Husband)] and [sequence(Path),mmpmulher(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mpmulher (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation pmulher (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmulher (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmulher(Son),pmmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmulher(Son)] and [sequence(Path),pmmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmulher(Son),mmmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmulher(Son)] and [sequence(Path),mmmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmulher(Husband)] and [sequence(Path),mmmulher(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmmulher (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmmulher(Son),ppmmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmmulher(Son)] and [sequence(Path),ppmmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmmulher(Son),mpmmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmulher(Son)] and [sequence(Path),mpmmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmmulher(Husband)] and [sequence(Path),mpmmulher(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation pmmulher (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmmulher (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmmulher(Son),pmmmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmmulher(Son)] and [sequence(Path),pmmmulher(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmmulher(Son),mmmmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmmulher(Son)] and [sequence(Path),mmmmulher(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmmulher(Husband)] and [sequence(Path),mmmmulher(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mmmulher (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mmulher (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mulher (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

/* ========================================== */

/* BEGIN Auto-relation mulher1 (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mulher1(Son),pmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mulher1(Son)] and [sequence(Path),pmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mulher1(Son),mmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mulher1(Son)] and [sequence(Path),mmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmulher1(Husband)] and [sequence(Path),mmulher1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmulher1 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmulher1(Son),ppmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmulher1(Son)] and [sequence(Path),ppmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmulher1(Son),mpmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmulher1(Son)] and [sequence(Path),mpmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmulher1(Husband)] and [sequence(Path),mpmulher1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation ppmulher1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),ppmulher1(Son),pppmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),ppmulher1(Son)] and [sequence(Path),pppmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),ppmulher1(Son),mppmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmulher1(Son)] and [sequence(Path),mppmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pppmulher1(Husband)] and [sequence(Path),mppmulher1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation ppmulher1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mpmulher1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mpmulher1(Son),pmpmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mpmulher1(Son)] and [sequence(Path),pmpmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mpmulher1(Son),mmpmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mpmulher1(Son)] and [sequence(Path),mmpmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmpmulher1(Husband)] and [sequence(Path),mmpmulher1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mpmulher1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation pmulher1 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmulher1 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmulher1(Son),pmmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmulher1(Son)] and [sequence(Path),pmmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmulher1(Son),mmmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmulher1(Son)] and [sequence(Path),mmmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmulher1(Husband)] and [sequence(Path),mmmulher1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmmulher1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmmulher1(Son),ppmmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmmulher1(Son)] and [sequence(Path),ppmmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmmulher1(Son),mpmmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmulher1(Son)] and [sequence(Path),mpmmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmmulher1(Husband)] and [sequence(Path),mpmmulher1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation pmmulher1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmmulher1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmmulher1(Son),pmmmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmmulher1(Son)] and [sequence(Path),pmmmulher1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmmulher1(Son),mmmmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmmulher1(Son)] and [sequence(Path),mmmmulher1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmmulher1(Husband)] and [sequence(Path),mmmmulher1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mmmulher1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mmulher1 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mulher1 (levels 3) v2.1 16-5-2005
 ----------------------------------------- */



/* ========================================== */

/* BEGIN Auto-relation mulher2 (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mulher2(Son),pmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mulher2(Son)] and [sequence(Path),pmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mulher2(Son),mmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mulher2(Son)] and [sequence(Path),mmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmulher2(Husband)] and [sequence(Path),mmulher2(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmulher2 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmulher2(Son),ppmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmulher2(Son)] and [sequence(Path),ppmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmulher2(Son),mpmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmulher2(Son)] and [sequence(Path),mpmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmulher2(Husband)] and [sequence(Path),mpmulher2(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation ppmulher2 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),ppmulher2(Son),pppmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),ppmulher2(Son)] and [sequence(Path),pppmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),ppmulher2(Son),mppmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmulher2(Son)] and [sequence(Path),mppmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pppmulher2(Husband)] and [sequence(Path),mppmulher2(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation ppmulher2 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mpmulher2 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mpmulher2(Son),pmpmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mpmulher2(Son)] and [sequence(Path),pmpmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mpmulher2(Son),mmpmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mpmulher2(Son)] and [sequence(Path),mmpmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmpmulher2(Husband)] and [sequence(Path),mmpmulher2(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mpmulher2 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation pmulher2 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmulher2 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmulher2(Son),pmmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmulher2(Son)] and [sequence(Path),pmmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmulher2(Son),mmmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmulher2(Son)] and [sequence(Path),mmmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmulher2(Husband)] and [sequence(Path),mmmulher2(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmmulher2 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmmulher2(Son),ppmmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmmulher2(Son)] and [sequence(Path),ppmmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmmulher2(Son),mpmmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmulher2(Son)] and [sequence(Path),mpmmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmmulher2(Husband)] and [sequence(Path),mpmmulher2(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation pmmulher2 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmmulher2 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmmulher2(Son),pmmmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmmulher2(Son)] and [sequence(Path),pmmmulher2(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmmulher2(Son),mmmmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmmulher2(Son)] and [sequence(Path),mmmmulher2(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmmulher2(Husband)] and [sequence(Path),mmmmulher2(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mmmulher2 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mmulher2 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mulher2 (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

/* ========================================== */

/* BEGIN Auto-relation mulher3 (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mulher3(Son),pmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mulher3(Son)] and [sequence(Path),pmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mulher3(Son),mmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mulher3(Son)] and [sequence(Path),mmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmulher3(Husband)] and [sequence(Path),mmulher3(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmulher3 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmulher3(Son),ppmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmulher3(Son)] and [sequence(Path),ppmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmulher3(Son),mpmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmulher3(Son)] and [sequence(Path),mpmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmulher3(Husband)] and [sequence(Path),mpmulher3(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation ppmulher3 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),ppmulher3(Son),pppmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),ppmulher3(Son)] and [sequence(Path),pppmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),ppmulher3(Son),mppmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmulher3(Son)] and [sequence(Path),mppmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pppmulher3(Husband)] and [sequence(Path),mppmulher3(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation ppmulher3 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mpmulher3 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mpmulher3(Son),pmpmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mpmulher3(Son)] and [sequence(Path),pmpmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mpmulher3(Son),mmpmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mpmulher3(Son)] and [sequence(Path),mmpmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmpmulher3(Husband)] and [sequence(Path),mmpmulher3(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mpmulher3 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation pmulher3 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmulher3 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmulher3(Son),pmmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmulher3(Son)] and [sequence(Path),pmmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmulher3(Son),mmmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmulher3(Son)] and [sequence(Path),mmmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmulher3(Husband)] and [sequence(Path),mmmulher3(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmmulher3 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmmulher3(Son),ppmmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmmulher3(Son)] and [sequence(Path),ppmmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmmulher3(Son),mpmmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmulher3(Son)] and [sequence(Path),mpmmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmmulher3(Husband)] and [sequence(Path),mpmmulher3(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation pmmulher3 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmmulher3 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmmulher3(Son),pmmmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmmulher3(Son)] and [sequence(Path),pmmmulher3(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmmulher3(Son),mmmmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmmulher3(Son)] and [sequence(Path),mmmmulher3(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmmulher3(Husband)] and [sequence(Path),mmmmulher3(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mmmulher3 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mmulher3 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mulher3 (levels 3) v2.1 16-5-2005
 ----------------------------------------- */



/* ========================================== */

/* BEGIN Auto-relation marido1 (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),marido1(Son),pmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),marido1(Son)] and [sequence(Path),pmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),marido1(Son),mmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),marido1(Son)] and [sequence(Path),mmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmarido1(Husband)] and [sequence(Path),mmarido1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmarido1 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmarido1(Son),ppmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmarido1(Son)] and [sequence(Path),ppmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmarido1(Son),mpmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmarido1(Son)] and [sequence(Path),mpmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmarido1(Husband)] and [sequence(Path),mpmarido1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation ppmarido1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),ppmarido1(Son),pppmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),ppmarido1(Son)] and [sequence(Path),pppmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),ppmarido1(Son),mppmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmarido1(Son)] and [sequence(Path),mppmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pppmarido1(Husband)] and [sequence(Path),mppmarido1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation ppmarido1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mpmarido1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mpmarido1(Son),pmpmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mpmarido1(Son)] and [sequence(Path),pmpmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mpmarido1(Son),mmpmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mpmarido1(Son)] and [sequence(Path),mmpmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmpmarido1(Husband)] and [sequence(Path),mmpmarido1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mpmarido1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation pmarido1 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmarido1 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmarido1(Son),pmmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmarido1(Son)] and [sequence(Path),pmmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmarido1(Son),mmmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmarido1(Son)] and [sequence(Path),mmmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmarido1(Husband)] and [sequence(Path),mmmarido1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmmarido1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmmarido1(Son),ppmmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmmarido1(Son)] and [sequence(Path),ppmmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmmarido1(Son),mpmmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmarido1(Son)] and [sequence(Path),mpmmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmmarido1(Husband)] and [sequence(Path),mpmmarido1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation pmmarido1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmmarido1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmmarido1(Son),pmmmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmmarido1(Son)] and [sequence(Path),pmmmarido1(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmmarido1(Son),mmmmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmmarido1(Son)] and [sequence(Path),mmmmarido1(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmmarido1(Husband)] and [sequence(Path),mmmmarido1(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mmmarido1 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mmarido1 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation marido1 (levels 3) v2.1 16-5-2005
 ----------------------------------------- */



 
  /* ========================================== */

  /* BEGIN Auto-relation marido2 (levels 3) v2.1 16-5-2005
   ----------------------------------------- */

  if [sequence(_),marido2(Son),pmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(Path),marido2(Son)] and [sequence(Path),pmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(_),marido2(Son),mmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),marido2(Son)] and [sequence(Path),mmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),pmarido2(Husband)] and [sequence(Path),mmarido2(Wife)]
  then relation(parentesco,marido,Husband,Wife).

  /* BEGIN Auto-relation pmarido2 (levels 2) v2.1 16-5-2005
   ----------------------------------------- */

  if [sequence(_),pmarido2(Son),ppmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(Path),pmarido2(Son)] and [sequence(Path),ppmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(_),pmarido2(Son),mpmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),pmarido2(Son)] and [sequence(Path),mpmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),ppmarido2(Husband)] and [sequence(Path),mpmarido2(Wife)]
  then relation(parentesco,marido,Husband,Wife).

  /* BEGIN Auto-relation ppmarido2 (levels 1) v2.1 16-5-2005
   ----------------------------------------- */

  if [sequence(_),ppmarido2(Son),pppmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(Path),ppmarido2(Son)] and [sequence(Path),pppmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(_),ppmarido2(Son),mppmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),ppmarido2(Son)] and [sequence(Path),mppmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),pppmarido2(Husband)] and [sequence(Path),mppmarido2(Wife)]
  then relation(parentesco,marido,Husband,Wife).

  /* END Auto-relation ppmarido2 (levels 1) v2.1 16-5-2005
   ----------------------------------------- */


  /* BEGIN Auto-relation mpmarido2 (levels 1) v2.1 16-5-2005
   ----------------------------------------- */

  if [sequence(_),mpmarido2(Son),pmpmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(Path),mpmarido2(Son)] and [sequence(Path),pmpmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(_),mpmarido2(Son),mmpmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),mpmarido2(Son)] and [sequence(Path),mmpmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),pmpmarido2(Husband)] and [sequence(Path),mmpmarido2(Wife)]
  then relation(parentesco,marido,Husband,Wife).

  /* END Auto-relation mpmarido2 (levels 1) v2.1 16-5-2005
   ----------------------------------------- */


  /* END Auto-relation pmarido2 (levels 2) v2.1 16-5-2005
   ----------------------------------------- */


  /* BEGIN Auto-relation mmarido2 (levels 2) v2.1 16-5-2005
   ----------------------------------------- */

  if [sequence(_),mmarido2(Son),pmmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(Path),mmarido2(Son)] and [sequence(Path),pmmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(_),mmarido2(Son),mmmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),mmarido2(Son)] and [sequence(Path),mmmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),pmmarido2(Husband)] and [sequence(Path),mmmarido2(Wife)]
  then relation(parentesco,marido,Husband,Wife).

  /* BEGIN Auto-relation pmmarido2 (levels 1) v2.1 16-5-2005
   ----------------------------------------- */

  if [sequence(_),pmmarido2(Son),ppmmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(Path),pmmarido2(Son)] and [sequence(Path),ppmmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(_),pmmarido2(Son),mpmmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),pmmarido2(Son)] and [sequence(Path),mpmmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),ppmmarido2(Husband)] and [sequence(Path),mpmmarido2(Wife)]
  then relation(parentesco,marido,Husband,Wife).

  /* END Auto-relation pmmarido2 (levels 1) v2.1 16-5-2005
   ----------------------------------------- */


  /* BEGIN Auto-relation mmmarido2 (levels 1) v2.1 16-5-2005
   ----------------------------------------- */

  if [sequence(_),mmmarido2(Son),pmmmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(Path),mmmarido2(Son)] and [sequence(Path),pmmmarido2(Parent)]
  then relation(parentesco,pai,Parent,Son).
  if [sequence(_),mmmarido2(Son),mmmmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),mmmarido2(Son)] and [sequence(Path),mmmmarido2(Parent)]
  then relation(parentesco,mae,Parent,Son).
  if [sequence(Path),pmmmarido2(Husband)] and [sequence(Path),mmmmarido2(Wife)]
  then relation(parentesco,marido,Husband,Wife).

  /* END Auto-relation mmmarido2 (levels 1) v2.1 16-5-2005
   ----------------------------------------- */


  /* END Auto-relation mmarido2 (levels 2) v2.1 16-5-2005
   ----------------------------------------- */


  /* END Auto-relation marido2 (levels 3) v2.1 16-5-2005
   ----------------------------------------- */

 /* ========================================== */

 /* BEGIN Auto-relation marido3 (levels 3) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),marido3(Son),pmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),marido3(Son)] and [sequence(Path),pmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),marido3(Son),mmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),marido3(Son)] and [sequence(Path),mmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmarido3(Husband)] and [sequence(Path),mmarido3(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation pmarido3 (levels 2) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pmarido3(Son),ppmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pmarido3(Son)] and [sequence(Path),ppmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pmarido3(Son),mpmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmarido3(Son)] and [sequence(Path),mpmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmarido3(Husband)] and [sequence(Path),mpmarido3(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation ppmarido3 (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),ppmarido3(Son),pppmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),ppmarido3(Son)] and [sequence(Path),pppmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),ppmarido3(Son),mppmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmarido3(Son)] and [sequence(Path),mppmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pppmarido3(Husband)] and [sequence(Path),mppmarido3(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation ppmarido3 (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mpmarido3 (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mpmarido3(Son),pmpmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mpmarido3(Son)] and [sequence(Path),pmpmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mpmarido3(Son),mmpmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mpmarido3(Son)] and [sequence(Path),mmpmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmpmarido3(Husband)] and [sequence(Path),mmpmarido3(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation mpmarido3 (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation pmarido3 (levels 2) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mmarido3 (levels 2) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mmarido3(Son),pmmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mmarido3(Son)] and [sequence(Path),pmmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mmarido3(Son),mmmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mmarido3(Son)] and [sequence(Path),mmmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmarido3(Husband)] and [sequence(Path),mmmarido3(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* BEGIN Auto-relation pmmarido3 (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),pmmarido3(Son),ppmmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),pmmarido3(Son)] and [sequence(Path),ppmmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),pmmarido3(Son),mpmmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmarido3(Son)] and [sequence(Path),mpmmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),ppmmarido3(Husband)] and [sequence(Path),mpmmarido3(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation pmmarido3 (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* BEGIN Auto-relation mmmarido3 (levels 1) v2.1 16-5-2005
  ----------------------------------------- */

 if [sequence(_),mmmarido3(Son),pmmmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(Path),mmmarido3(Son)] and [sequence(Path),pmmmarido3(Parent)]
 then relation(parentesco,pai,Parent,Son).
 if [sequence(_),mmmarido3(Son),mmmmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),mmmarido3(Son)] and [sequence(Path),mmmmarido3(Parent)]
 then relation(parentesco,mae,Parent,Son).
 if [sequence(Path),pmmmarido3(Husband)] and [sequence(Path),mmmmarido3(Wife)]
 then relation(parentesco,marido,Husband,Wife).

 /* END Auto-relation mmmarido3 (levels 1) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation mmarido3 (levels 2) v2.1 16-5-2005
  ----------------------------------------- */


 /* END Auto-relation marido3 (levels 3) v2.1 16-5-2005
  ----------------------------------------- */


/* ========================================== */

/* BEGIN Auto-relation marido4 (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),marido4(Son),pmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),marido4(Son)] and [sequence(Path),pmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),marido4(Son),mmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),marido4(Son)] and [sequence(Path),mmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmarido4(Husband)] and [sequence(Path),mmarido4(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmarido4 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmarido4(Son),ppmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmarido4(Son)] and [sequence(Path),ppmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmarido4(Son),mpmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmarido4(Son)] and [sequence(Path),mpmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmarido4(Husband)] and [sequence(Path),mpmarido4(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation ppmarido4 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),ppmarido4(Son),pppmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),ppmarido4(Son)] and [sequence(Path),pppmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),ppmarido4(Son),mppmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmarido4(Son)] and [sequence(Path),mppmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pppmarido4(Husband)] and [sequence(Path),mppmarido4(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation ppmarido4 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mpmarido4 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mpmarido4(Son),pmpmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mpmarido4(Son)] and [sequence(Path),pmpmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mpmarido4(Son),mmpmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mpmarido4(Son)] and [sequence(Path),mmpmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmpmarido4(Husband)] and [sequence(Path),mmpmarido4(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mpmarido4 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation pmarido4 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmarido4 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmarido4(Son),pmmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmarido4(Son)] and [sequence(Path),pmmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmarido4(Son),mmmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmarido4(Son)] and [sequence(Path),mmmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmarido4(Husband)] and [sequence(Path),mmmarido4(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmmarido4 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmmarido4(Son),ppmmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmmarido4(Son)] and [sequence(Path),ppmmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmmarido4(Son),mpmmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmarido4(Son)] and [sequence(Path),mpmmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmmarido4(Husband)] and [sequence(Path),mpmmarido4(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation pmmarido4 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmmarido4 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmmarido4(Son),pmmmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmmarido4(Son)] and [sequence(Path),pmmmarido4(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmmarido4(Son),mmmmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmmarido4(Son)] and [sequence(Path),mmmmarido4(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmmarido4(Husband)] and [sequence(Path),mmmmarido4(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mmmarido4 (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mmarido4 (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation marido4 (levels 3) v2.1 16-5-2005
 ----------------------------------------- */


/* ========================================== */

/* BEGIN Auto-relation noivo (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),noivo(Son),pnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),noivo(Son)] and [sequence(Path),pnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),noivo(Son),mnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),noivo(Son)] and [sequence(Path),mnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pnoivo(Husband)] and [sequence(Path),mnoivo(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pnoivo (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pnoivo(Son),ppnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pnoivo(Son)] and [sequence(Path),ppnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pnoivo(Son),mpnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pnoivo(Son)] and [sequence(Path),mpnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppnoivo(Husband)] and [sequence(Path),mpnoivo(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation ppnoivo (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),ppnoivo(Son),pppnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),ppnoivo(Son)] and [sequence(Path),pppnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),ppnoivo(Son),mppnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppnoivo(Son)] and [sequence(Path),mppnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pppnoivo(Husband)] and [sequence(Path),mppnoivo(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation ppnoivo (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mpnoivo (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mpnoivo(Son),pmpnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mpnoivo(Son)] and [sequence(Path),pmpnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mpnoivo(Son),mmpnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mpnoivo(Son)] and [sequence(Path),mmpnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmpnoivo(Husband)] and [sequence(Path),mmpnoivo(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mpnoivo (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation pnoivo (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mnoivo (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mnoivo(Son),pmnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mnoivo(Son)] and [sequence(Path),pmnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mnoivo(Son),mmnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mnoivo(Son)] and [sequence(Path),mmnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmnoivo(Husband)] and [sequence(Path),mmnoivo(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmnoivo (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmnoivo(Son),ppmnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmnoivo(Son)] and [sequence(Path),ppmnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmnoivo(Son),mpmnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmnoivo(Son)] and [sequence(Path),mpmnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmnoivo(Husband)] and [sequence(Path),mpmnoivo(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation pmnoivo (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmnoivo (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmnoivo(Son),pmmnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmnoivo(Son)] and [sequence(Path),pmmnoivo(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmnoivo(Son),mmmnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmnoivo(Son)] and [sequence(Path),mmmnoivo(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmnoivo(Husband)] and [sequence(Path),mmmnoivo(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mmnoivo (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mnoivo (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation noivo (levels 3) v2.1 16-5-2005
 ----------------------------------------- */


/* ========================================== */

/* BEGIN Auto-relation noiva (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),noiva(Son),pnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),noiva(Son)] and [sequence(Path),pnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),noiva(Son),mnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),noiva(Son)] and [sequence(Path),mnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pnoiva(Husband)] and [sequence(Path),mnoiva(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pnoiva (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pnoiva(Son),ppnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pnoiva(Son)] and [sequence(Path),ppnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pnoiva(Son),mpnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pnoiva(Son)] and [sequence(Path),mpnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppnoiva(Husband)] and [sequence(Path),mpnoiva(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation ppnoiva (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),ppnoiva(Son),pppnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),ppnoiva(Son)] and [sequence(Path),pppnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),ppnoiva(Son),mppnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppnoiva(Son)] and [sequence(Path),mppnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pppnoiva(Husband)] and [sequence(Path),mppnoiva(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation ppnoiva (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mpnoiva (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mpnoiva(Son),pmpnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mpnoiva(Son)] and [sequence(Path),pmpnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mpnoiva(Son),mmpnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mpnoiva(Son)] and [sequence(Path),mmpnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmpnoiva(Husband)] and [sequence(Path),mmpnoiva(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mpnoiva (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation pnoiva (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mnoiva (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mnoiva(Son),pmnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mnoiva(Son)] and [sequence(Path),pmnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mnoiva(Son),mmnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mnoiva(Son)] and [sequence(Path),mmnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmnoiva(Husband)] and [sequence(Path),mmnoiva(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmnoiva (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmnoiva(Son),ppmnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmnoiva(Son)] and [sequence(Path),ppmnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmnoiva(Son),mpmnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmnoiva(Son)] and [sequence(Path),mpmnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmnoiva(Husband)] and [sequence(Path),mpmnoiva(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation pmnoiva (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmnoiva (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmnoiva(Son),pmmnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmnoiva(Son)] and [sequence(Path),pmmnoiva(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmnoiva(Son),mmmnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmnoiva(Son)] and [sequence(Path),mmmnoiva(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmnoiva(Husband)] and [sequence(Path),mmmnoiva(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mmnoiva (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mnoiva (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation noiva (levels 3) v2.1 16-5-2005
 ----------------------------------------- */


/* ========================================== */

/* BEGIN Auto-relation n (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),n(Son),pn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),n(Son)] and [sequence(Path),pn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),n(Son),mn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),n(Son)] and [sequence(Path),mn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pn(Husband)] and [sequence(Path),mn(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pn (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pn(Son),ppn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pn(Son)] and [sequence(Path),ppn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pn(Son),mpn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pn(Son)] and [sequence(Path),mpn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppn(Husband)] and [sequence(Path),mpn(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation ppn (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),ppn(Son),pppn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),ppn(Son)] and [sequence(Path),pppn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),ppn(Son),mppn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppn(Son)] and [sequence(Path),mppn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pppn(Husband)] and [sequence(Path),mppn(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation ppn (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mpn (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mpn(Son),pmpn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mpn(Son)] and [sequence(Path),pmpn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mpn(Son),mmpn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mpn(Son)] and [sequence(Path),mmpn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmpn(Husband)] and [sequence(Path),mmpn(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mpn (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation pn (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mn (levels 2) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mn(Son),pmn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mn(Son)] and [sequence(Path),pmn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mn(Son),mmn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mn(Son)] and [sequence(Path),mmn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmn(Husband)] and [sequence(Path),mmn(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* BEGIN Auto-relation pmn (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),pmn(Son),ppmn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),pmn(Son)] and [sequence(Path),ppmn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),pmn(Son),mpmn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmn(Son)] and [sequence(Path),mpmn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),ppmn(Husband)] and [sequence(Path),mpmn(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation pmn (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* BEGIN Auto-relation mmn (levels 1) v2.1 16-5-2005
 ----------------------------------------- */

if [sequence(_),mmn(Son),pmmn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(Path),mmn(Son)] and [sequence(Path),pmmn(Parent)]
then relation(parentesco,pai,Parent,Son).
if [sequence(_),mmn(Son),mmmn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),mmn(Son)] and [sequence(Path),mmmn(Parent)]
then relation(parentesco,mae,Parent,Son).
if [sequence(Path),pmmn(Husband)] and [sequence(Path),mmmn(Wife)]
then relation(parentesco,marido,Husband,Wife).

/* END Auto-relation mmn (levels 1) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation mn (levels 2) v2.1 16-5-2005
 ----------------------------------------- */


/* END Auto-relation n (levels 3) v2.1 16-5-2005
 ----------------------------------------- */

/*
    special cases
    
    godfathers and godmothers in baptisms
    */
if [sequence(Path),mad(Mad)]   and [sequence(Path),pmad(PMad)]   then relation(parentesco,pai,PMad,Mad).
if [sequence(Path),mad(Mad)]   and [sequence(Path),mmad(MMad)]   then relation(parentesco,mae,MMad,Mad).
if [sequence(Path),pad(Pad)]   and [sequence(Path),mpad(MPad)]   then relation(parentesco,mae,MPad,Pad).
if [sequence(Path),ppad(PPad)] and [sequence(Path),mpad(MPad)]   then relation(parentesco,marido,PPad,MPad).
if [sequence(Path),pmad(PMad)] and [sequence(Path),mmad(MMad)]   then relation(parentesco,marido,PMad,MMad).
if [sequence(Path),mad(Mad)]   and [sequence(Path),mrmad(MrMad)] then relation(parentesco,marido,MrMad,Mad).
if [sequence(Path),pad(Pad)]   and [sequence(Path),ppad(PPad)]   then relation(parentesco,pai,PPad,Pad).

if [sequence(Path),mad1(Mad)] and [sequence(Path),pmad1(PMad)] then relation(parentesco,pai,PMad,Mad).
if [sequence(Path),mad1(Mad)] and [sequence(Path),mmad1(MMad)] then relation(parentesco,mae,MMad,Mad).
if [sequence(Path),mad1(Mad)] and [sequence(Path),mmad1(MMad)] then relation(parentesco,mae,MMad,Mad).
if [sequence(Path),mad1(Mad)] and [sequence(Path),mrmad1(MrMad)] then relation(parentesco,marido,MrMad,Mad).
if [sequence(Path),pad1(Pad)] and [sequence(Path),ppad1(PPad)] then relation(parentesco,pai,PPad,Pad).

if [sequence(Path),mad2(Mad)] and [sequence(Path),pmad2(PMad)] then relation(parentesco,pai,PMad,Mad).
if [sequence(Path),mad2(Mad)] and [sequence(Path),mmad2(MMad)] then relation(parentesco,mae,MMad,Mad).
if [sequence(Path),mad2(Mad)] and [sequence(Path),mrmad2(MrMad)] then relation(parentesco,marido,MrMad,Mad).
if [sequence(Path),pad2(Pad)] and [sequence(Path),ppad2(PPad)] then relation(parentesco,pai,PPad,Pad).

if [sequence(Path),mad3(Mad)] and [sequence(Path),pmad3(PMad)] then relation(parentesco,pai,PMad,Mad).
if [sequence(Path),mad3(Mad)] and [sequence(Path),mmad3(MMad)] then relation(parentesco,mae,MMad,Mad).
if [sequence(Path),mad3(Mad)] and [sequence(Path),mrmad3(MrMad)] then relation(parentesco,marido,MrMad,Mad).
if [sequence(Path),pad3(Pad)] and [sequence(Path),ppad3(PPad)] then relation(parentesco,pai,PPad,Pad).

if [sequence(Path),mad4(Mad)] and [sequence(Path),pmad4(PMad)] then relation(parentesco,pai,PMad,Mad).
if [sequence(Path),mad4(Mad)] and [sequence(Path),mmad4(MMad)] then relation(parentesco,mae,MMad,Mad).
if [sequence(Path),mad4(Mad)] and [sequence(Path),mrmad4(MrMad)] then relation(parentesco,marido,MrMad,Mad).
if [sequence(Path),pad4(Pad)] and [sequence(Path),ppad4(PPad)] then relation(parentesco,pai,PPad,Pad).

/*    rois (household lists) */
if [kleio(_K),fonte(_F),rol(_R),fogo(FG),n(N)] then
  relation(function,'has-head-of-household',FG,N).

if [kleio(K),fonte(F),rol(R),fogo(FG),n(N)] and
   [kleio(K),fonte(F),rol(R),fogo(FG),criado(C)] then
 relation(profissional,criado,C,N).


if [kleio(K),fonte(F),rol(R),fogo(FG),n(N)] and
   [kleio(K),fonte(F),rol(R),fogo(FG),criada(C)] then
 relation(profissional,criada,C,N).

 /*
    procuradores in baptisms
    sometimes godfathers and godmothers are represented by procuradores
    we generate a sociability relation
    */

 if [kleio(K),fonte(F),bap(B),n(N),mad(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procuradora-mad'(PM)] then
    relation(sociabilidade,procuradora,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),mad(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procurador-mad'(PM)] then
    relation(sociabilidade,procurador,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),pad(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procurador-pad'(PM)] then
    relation(sociabilidade,procurador,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),pad(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procuradora-pad'(PM)] then
    relation(sociabilidade,procuradora,PM,M).

  if [kleio(K),fonte(F),bap(B),n(N),mad1(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procuradora-mad1'(PM)] then
    relation(sociabilidade,procuradora,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),mad1(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procurador-mad1'(PM)] then
    relation(sociabilidade,procurador,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),pad1(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procurador-pad1'(PM)] then
    relation(sociabilidade,procurador,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),pad1(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procuradora-pad1'(PM)] then
    relation(sociabilidade,procuradora,PM,M).

  if [kleio(K),fonte(F),bap(B),n(N),mad2(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procuradora-mad2'(PM)] then
    relation(sociabilidade,procuradora,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),mad2(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procurador-mad2'(PM)] then
    relation(sociabilidade,procurador,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),pad2(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procurador-pad2'(PM)] then
    relation(sociabilidade,procurador,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),pad2(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procuradora-pad2'(PM)] then
    relation(sociabilidade,procuradora,PM,M).

  if [kleio(K),fonte(F),bap(B),n(N),mad3(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procuradora-mad3'(PM)] then
    relation(sociabilidade,procuradora,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),mad3(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procurador-mad3'(PM)] then
    relation(sociabilidade,procurador,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),pad3(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procurador-pad3'(PM)] then
    relation(sociabilidade,procurador,PM,M).

 if [kleio(K),fonte(F),bap(B),n(N),pad3(M)] and
    [kleio(K),fonte(F),bap(B),n(N),'procuradora-pad3'(PM)] then
    relation(sociabilidade,procuradora,PM,M).

 if [kleio(K),fonte(F),b(B),n(N),mad(M)] and
    [kleio(K),fonte(F),b(B),n(N),'procuradora-mad'(PM)] then
    relation(sociabilidade,procuradora,PM,M).

 if [kleio(K),fonte(F),b(B),n(N),mad(M)] and
    [kleio(K),fonte(F),b(B),n(N),'procurador-mad'(PM)] then
    relation(sociabilidade,procurador,PM,M).

 if [kleio(K),fonte(F),b(B),n(N),pad(M)] and
    [kleio(K),fonte(F),b(B),n(N),'procurador-pad'(PM)] then
    relation(sociabilidade,procurador,PM,M).

 if [kleio(K),fonte(F),b(B),n(N),pad(M)] and
    [kleio(K),fonte(F),b(B),n(N),'procuradora-pad4'(PM)] then
    relation(sociabilidade,procuradora,PM,M).

 /*
  vereacoes
 */
if [sequence(_Path),acto(_P),presente(M)]
   then

        attribute(M,assina,sim).
if [sequence(_Path),acto(_P),'presente-f'(M)]
   then

        attribute(M,assina,sim).


/*
  Obitos
*/
  % regra de excepcao para os pais, maes, maridos e mulheres nos obitos
   if [sequence(Path),obito(O),n(N)] and [sequence(Path),obito(O),pai(P)]
   then
        relation(parentesco,pai,P,N).

   if [sequence(Path),obito(O),n(N)] and [sequence(Path),obito(O),mae(M)]
   then
        relation(parentesco,mae,M,N).
   if [sequence(Path),obito(O),n(N)] and [sequence(Path),obito(O),marido(Ma)]
   then
        relation(parentesco,marido,Ma,N).
   if [sequence(Path),obito(O),n(N)] and [sequence(Path),obito(O),mulher(Me)]
   then
        relation(parentesco,marido,N,Me).
 	if [sequence(_),obito(_),n(N)] 
	then
		attribute(N,morto,agora).

/* Sister - inferences related to geoentities in portuguese

    nas relacoes de inclusao registar do lado da entidade incluida
    e.g. relation(geografica,pertence-civil,concelho,distrito)

*/
% memorias paroquiais de 1758
if [sequence(Path),extends(memoria58,Mid),freguesia(F),provincia(P)]
then
    relation(geografica,'pertence-civil',F,P).
if [sequence(Path),extends(memoria58,Mid),freguesia(F),comarca(P)]
then
    relation(geografica,'pertence-civil',F,P).
if [sequence(Path),extends(memoria58,Mid),freguesia(F),termoc(P)]
then
    relation(geografica,'pertence-civil',F,P).
if [sequence(Path),extends(memoria58,Mid),freguesia(F),bispado(P)]
then
    relation(geografica,'pertence-eccle',F,P).
if [sequence(Path),extends(memoria58,Mid),freguesia(F),lugar(P)]
then
    relation(geografica,'contem',F,P).
if [sequence(Path),extends(memoria58,Mid),freguesia(F),igreja(P)]
then
    relation(geografica,'contem',F,P).
if [sequence(Path),extends(memoria58,Mid),freguesia(F),irmandade(P)]
then
    relation(geografica,'contem',F,P).
if [sequence(Path),extends(memoria58,Mid),freguesia(F),confraria(P)]
then
    relation(geografica,'contem',F,P).
/*
--- nova regra
*/
if [kleio(K),fonte(F),juramento(J),abonador(A)] and
   [kleio(K),fonte(F),juramento(J),n(N)]
then
   relation(sociabilidade,abonador,A,N).
	  
	
/*===================================
	lc Letras de cambio
=====================================*/
if [kleio(K),fonte(F),lc(L),beneficiario(R)] and
	[kleio(K),fonte(F),lc(L),'beneficiario-representante'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),lc(L),beneficiario1(R)] and
	[kleio(K),fonte(F),lc(L),'beneficiario1-representante'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),lc(L),beneficiario2(R)] and
	[kleio(K),fonte(F),lc(L),'beneficiario2-representante'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),lc(L),beneficiario3(R)] and
	[kleio(K),fonte(F),lc(L),'beneficiario3-representante'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),lc(L),beneficiario4(R)] and
	[kleio(K),fonte(F),lc(L),'beneficiario4-representante'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),lc(L),beneficiario(R)] and
	[kleio(K),fonte(F),lc(L),'beneficiario-procurador'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),lc(L),dador(R)] and
	[kleio(K),fonte(F),lc(L),'dador-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),dador1(R)] and
	[kleio(K),fonte(F),lc(L),'dador1-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),dador2(R)] and
	[kleio(K),fonte(F),lc(L),'dador2-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),dador3(R)] and
	[kleio(K),fonte(F),lc(L),'dador3-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),dador4(R)] and
	[kleio(K),fonte(F),lc(L),'dador4-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),pagador(R)] and
	[kleio(K),fonte(F),lc(L),'pagador-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),pagador1(R)] and
	[kleio(K),fonte(F),lc(L),'pagador1-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),pagador2(R)] and
	[kleio(K),fonte(F),lc(L),'pagador2-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),pagador3(R)] and
	[kleio(K),fonte(F),lc(L),'pagador3-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),pagador4(R)] and
	[kleio(K),fonte(F),lc(L),'pagador4-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),recebedor(R)] and
	[kleio(K),fonte(F),lc(L),'recebedor-proc'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),lc(L),recebedor1(R)] and
	[kleio(K),fonte(F),lc(L),'recebedor1-proc'(P)]
then
	relation(sociabilidade,procurador,P,R).	
if [kleio(K),fonte(F),lc(L),recebedor2(R)] and
	[kleio(K),fonte(F),lc(L),'recebedor2-proc'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),lc(L),recebedor3(R)] and
	[kleio(K),fonte(F),lc(L),'recebedor3-proc'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),lc(L),recebedor4(R)] and
	[kleio(K),fonte(F),lc(L),'recebedor4-proc'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),lc(L),tomador(R)] and
	[kleio(K),fonte(F),lc(L),'tomador-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),tomador1(R)] and
	[kleio(K),fonte(F),lc(L),'tomador1-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),tomador2(R)] and
	[kleio(K),fonte(F),lc(L),'tomador2-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),tomador3(R)] and
	[kleio(K),fonte(F),lc(L),'tomador3-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),tomador4(R)] and
	[kleio(K),fonte(F),lc(L),'tomador4-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),lc(L),beneficiario(R)] and
	[kleio(K),fonte(F),lc(L),dador(P)]
then
	relation(financeira,'dador-beneficiario',P,R).
if [kleio(K),fonte(F),lc(L),beneficiario1(R)] and
	[kleio(K),fonte(F),lc(L),dador1(P)]
then
	relation(financeira,'dador1-beneficiario1',P,R).
if [kleio(K),fonte(F),lc(L),beneficiario2(R)] and
	[kleio(K),fonte(F),lc(L),dador2(P)]
then
	relation(financeira,'dador2-beneficiario1',P,R).
if [kleio(K),fonte(F),lc(L),beneficiario3(R)] and
	[kleio(K),fonte(F),lc(L),dador3(P)]
then
	relation(financeira,'dador3-beneficiario3',P,R).
if [kleio(K),fonte(F),lc(L),beneficiario4(R)] and
	[kleio(K),fonte(F),lc(L),dador4(P)]
then
	relation(financeira,'dador4-beneficiario4',P,R).
if [kleio(K),fonte(F),lc(L),pagador(R)] and
	[kleio(K),fonte(F),lc(L),tomador(P)]
then
	relation(financeira,'tomador-pagador',P,R).
if [kleio(K),fonte(F),lc(L),pagador1(R)] and
	[kleio(K),fonte(F),lc(L),tomador1(P)]
then
	relation(financeira,'tomador1-pagador1',P,R).
if [kleio(K),fonte(F),lc(L),pagador2(R)] and
	[kleio(K),fonte(F),lc(L),tomador2(P)]
then
	relation(financeira,'tomador2-pagador3',P,R).
if [kleio(K),fonte(F),lc(L),pagador3(R)] and
	[kleio(K),fonte(F),lc(L),tomador3(P)]
then
	relation(financeira,'tomador3-pagador3',P,R).
if [kleio(K),fonte(F),lc(L),pagador4(R)] and
	[kleio(K),fonte(F),lc(L),tomador4(P)]
then
	relation(financeira,'tomador4-pagador4',P,R).
if [kleio(K),fonte(F),lc(L),pagador(R)] and
	[kleio(K),fonte(F),lc(L),dador(P)]
then
	relation(financeira,'dador-tomador',P,R).
if [kleio(K),fonte(F),lc(L),pagador1(R)] and
	[kleio(K),fonte(F),lc(L),dador1(P)]
then
	relation(financeira,'dador1-tomador1',P,R).
if [kleio(K),fonte(F),lc(L),pagador2(R)] and
	[kleio(K),fonte(F),lc(L),dador2(P)]                    
then
	relation(financeira,'dador2-tomador2',P,R).
if [kleio(K),fonte(F),lc(L),pagador3(R)] and
	[kleio(K),fonte(F),lc(L),dador3(P)]
then
	relation(financeira,'dador3-tomador3',P,R).
if [kleio(K),fonte(F),lc(L),pagador4(R)] and
	[kleio(K),fonte(F),lc(L),dador4(P)]
then
	relation(financeira,'dador4-tomador4',P,R).
		
/*===================================
	carta Correspondencia Comercial (Dyncoopnet)
=====================================*/
if [kleio(K),fonte(F),carta(L),comprador(R)] and
	[kleio(K),fonte(F),carta(L),vendedor(P)]
then
	relation(comercial,'vendedor-comprador',P,R).

if [kleio(K),fonte(F),carta(L),beneficiario(R)] and
	[kleio(K),fonte(F),carta(L),'beneficiario-representante'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),carta(L),beneficiario1(R)] and
	[kleio(K),fonte(F),carta(L),'beneficiario1-representante'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),carta(L),beneficiario2(R)] and
	[kleio(K),fonte(F),carta(L),'beneficiario2-representante'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),carta(L),beneficiario3(R)] and
	[kleio(K),fonte(F),carta(L),'beneficiario3-representante'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),carta(L),beneficiario4(R)] and
	[kleio(K),fonte(F),carta(L),'beneficiario4-representante'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),carta(L),beneficiario(R)] and
	[kleio(K),fonte(F),carta(L),'beneficiario-procurador'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),carta(L),dador(R)] and
	[kleio(K),fonte(F),carta(L),'dador-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),dador1(R)] and
	[kleio(K),fonte(F),carta(L),'dador1-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),dador2(R)] and
	[kleio(K),fonte(F),carta(L),'dador2-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),dador3(R)] and
	[kleio(K),fonte(F),carta(L),'dador3-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),dador4(R)] and
	[kleio(K),fonte(F),carta(L),'dador4-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),pagador(R)] and
	[kleio(K),fonte(F),carta(L),'pagador-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),pagador1(R)] and
	[kleio(K),fonte(F),carta(L),'pagador1-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),pagador2(R)] and
	[kleio(K),fonte(F),carta(L),'pagador2-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),pagador3(R)] and
	[kleio(K),fonte(F),carta(L),'pagador3-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),pagador4(R)] and
	[kleio(K),fonte(F),carta(L),'pagador4-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),recebedor(R)] and
	[kleio(K),fonte(F),carta(L),'recebedor-proc'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),carta(L),recebedor1(R)] and
	[kleio(K),fonte(F),carta(L),'recebedor1-proc'(P)]
then
	relation(sociabilidade,procurador,P,R).	
if [kleio(K),fonte(F),carta(L),recebedor2(R)] and
	[kleio(K),fonte(F),carta(L),'recebedor2-proc'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),carta(L),recebedor3(R)] and
	[kleio(K),fonte(F),carta(L),'recebedor3-proc'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),carta(L),recebedor4(R)] and
	[kleio(K),fonte(F),carta(L),'recebedor4-proc'(P)]
then
	relation(sociabilidade,procurador,P,R).
if [kleio(K),fonte(F),carta(L),tomador(R)] and
	[kleio(K),fonte(F),carta(L),'tomador-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),tomador1(R)] and
	[kleio(K),fonte(F),carta(L),'tomador1-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),tomador2(R)] and
	[kleio(K),fonte(F),carta(L),'tomador2-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),tomador3(R)] and
	[kleio(K),fonte(F),carta(L),'tomador3-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),tomador4(R)] and
	[kleio(K),fonte(F),carta(L),'tomador4-representante'(P)]
then
	relation(confianca,procurador,P,R).
if [kleio(K),fonte(F),carta(L),beneficiario(R)] and
	[kleio(K),fonte(F),carta(L),dador(P)]
then
	relation(financeira,'dador-beneficiario',P,R).
if [kleio(K),fonte(F),carta(L),beneficiario1(R)] and
	[kleio(K),fonte(F),carta(L),dador1(P)]
then
	relation(financeira,'dador1-beneficiario1',P,R).
if [kleio(K),fonte(F),carta(L),beneficiario2(R)] and
	[kleio(K),fonte(F),carta(L),dador2(P)]
then
	relation(financeira,'dador2-beneficiario1',P,R).
if [kleio(K),fonte(F),carta(L),beneficiario3(R)] and
	[kleio(K),fonte(F),carta(L),dador3(P)]
then
	relation(financeira,'dador3-beneficiario3',P,R).
if [kleio(K),fonte(F),carta(L),beneficiario4(R)] and
	[kleio(K),fonte(F),carta(L),dador4(P)]
then
	relation(financeira,'dador4-beneficiario4',P,R).
if [kleio(K),fonte(F),carta(L),pagador(R)] and
	[kleio(K),fonte(F),carta(L),tomador(P)]
then
	relation(financeira,'tomador-pagador',P,R).
if [kleio(K),fonte(F),carta(L),pagador1(R)] and
	[kleio(K),fonte(F),carta(L),tomador1(P)]
then
	relation(financeira,'tomador1-pagador1',P,R).
if [kleio(K),fonte(F),carta(L),pagador2(R)] and
	[kleio(K),fonte(F),carta(L),tomador2(P)]
then
	relation(financeira,'tomador2-pagador3',P,R).
if [kleio(K),fonte(F),carta(L),pagador3(R)] and
	[kleio(K),fonte(F),carta(L),tomador3(P)]
then
	relation(financeira,'tomador3-pagador3',P,R).
if [kleio(K),fonte(F),carta(L),pagador4(R)] and
	[kleio(K),fonte(F),carta(L),tomador4(P)]
then
	relation(financeira,'tomador4-pagador4',P,R).
if [kleio(K),fonte(F),carta(L),pagador(R)] and
	[kleio(K),fonte(F),carta(L),dador(P)]
then
	relation(financeira,'dador-tomador',P,R).
if [kleio(K),fonte(F),carta(L),pagador1(R)] and
	[kleio(K),fonte(F),carta(L),dador1(P)]
then
	relation(financeira,'dador1-tomador1',P,R).
if [kleio(K),fonte(F),carta(L),pagador2(R)] and
	[kleio(K),fonte(F),carta(L),dador2(P)]                    
then
	relation(financeira,'dador2-tomador2',P,R).
if [kleio(K),fonte(F),carta(L),pagador3(R)] and
	[kleio(K),fonte(F),carta(L),dador3(P)]
then
	relation(financeira,'dador3-tomador3',P,R).
if [kleio(K),fonte(F),carta(L),pagador4(R)] and
	[kleio(K),fonte(F),carta(L),dador4(P)]
then
	relation(financeira,'dador4-tomador4',P,R).
		
 /************************************************************

    scope rules
***************************************************************/
if [sequence(_path),extends('historical-act',_act)]
then newscope.

if [kleio(_K),fonte(_F),rol(_R),fogo(_FG)]
then newscope.

if [kleio(_K),fonte(_F),crisma(_R),n(_N)]
then newscope.
