:-module(apiExports,[
        exports/5,
        exports_get/3
        ]).

:-use_module(apiCommon).

/** <module> Api operations dealing with Export files (xml files)
  
Retrieve export files

**/

exports(get,Path,Mode,Id,Params):-
    sources(get,Path,Mode,Id,Params).

exports_get(json,Id,Params):-
    option(path(Path),Params,''),
    exports(get,Path,json,Id,Params).

