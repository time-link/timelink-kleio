:-module(apiReports,[
        reports/5,
        reports_get/3
        ]).

:-use_module(apiCommon).

/** <module> Api operations dealing with report files (rpt files)
  
Retrieve report files

**/

reports(get,Path,Mode,Id,Params):-
    sources(get,Path,Mode,Id,Params).

reports_get(json,Id,Params):-
    option(path(Path),Params,''),
    reports(get,Path,json,Id,Params).

