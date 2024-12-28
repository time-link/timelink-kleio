:- module(linkedData,
        [
            store_xlink_pattern/2,
            clear_xlink_patterns/0,
            clear_xlink_data/0,
            detect_xlink/3,
            generate_xlink/4,
            xlink_pattern/2,
            xlink_data/2
        ]).
/** <module> Predicates to handle linked data.

Linked data allows to map kleio data items to external
data. This is described in issue:#6 of

Generating data links envolves two steps:

1. Declare the external source, with an attribute of the kleio
   document:
      kleio$...
         link$wikidata/"http://wikidata.org/wiki/$1"

    The format of the link$ grupo is:
        link$short-name/url-pattern


    where short-name is a show name for an external sources
    url-pattern should be a url containing a place
    holder ($1) for a specific id of the data item to be linked

2. Anotate element values with external ids

    ls$jesuita-entrada/Goa, Índia# @wikidata:Q1171/15791200


    The format of an external link annotation is:

        @short-name/id


*/
:-use_module(library(pcre)).
:-use_module(errors).
:-dynamic xlink_pattern/2.
:-dynamic xlink_data/2.

?-thread_local(xlink_pattern/2).
?-thread_local(xlink_data/2).


%% store_xlink_url(?ShortName,?UrlPattern) is not det.
%
%  Stores UrlPattern associated with ShortName
%  If shortName existed before it is replaced.
%  At a given moment only one ShortName entry exists
%
store_xlink_pattern(ShortName,UrlPattern):-
    (retractall(xlink_pattern(ShortName,_));true),
    assertz(xlink_pattern(ShortName,UrlPattern)),!.


%% clean_xlinks is det.
%
%  removes all store Linked data URL patterns
clear_xlink_patterns:-
    retractall(xlink_pattern(_,_)),!.

%% detect_xlink(+Text,ShortName,Id) is det.
%
%  Detect a linked data annotation in the form
%   @shortName:Id

detect_xlink(Text,ShortName,Id):-
    re_split("@[a-z]*:\\s*[a-zA-Z0-9\\:\\-]*",Text,List),
    member(Sequence,List),
    re_match("@",Sequence),
    re_matchsub(".*@([a-z]*):[\\s]*([\\ 0-9a-zA-Z\\:\\-]*)", Sequence,S,[]),
    S=_{0:_,1:ShortName,2:Id}.

%% replace_xid(+Pattern,+Id,-Link) is det.
%
%  Replace $1 in a Pattern with an Id.
%  Fails if no $1 in string.
%
replace_xid(Pattern,Id,Link) :-
    atom_string(Pattern,PatternS),
    string_codes(PatternS,Codes),
    delete(Codes,34,CodesClean),
    string_codes(PS,CodesClean),
    re_replace("\\$1",Id, PS, Link).

%% generate_xlink(+Text,-Uri) is det.
%
%  Given a linked data annotation generate
%  the URI of the linked data item.
generate_xlink(Text,Uri,S,I):-
    detect_xlink(Text,S,I),
    atom_string(SA,S),
    (clause(linkedData:xlink_pattern(SA,P),true)->
        (replace_xid(P,I,Uri)  % ,
         % assertz(xlink_data(Uri,Text))
        )
        ;
        warning_out(["Could not link data with ",
            Text,
            "; is link$ definition for ",
            S," missing?"])
        ).



%% clear_xlink_data is det.
%
%  Clear the stored linked data links
clear_xlink_data:-
    retractall(xlink_data(_,_)).