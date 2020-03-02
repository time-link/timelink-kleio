:-module(apiIdentifications,[
    identifications/5,
    identifications_get/3
    ]).

:-use_module(apiCommon).

/** <module> Api operations dealing with report files (rpt files)

Retrieve report files

**/

identifications(get,Path,Mode,Id,Params):-
sources(get,Path,Mode,Id,Params).

identifications_get(json,Id,Params):-
option(path(Path),Params,''),
reports(get,Path,json,Id,Params).

