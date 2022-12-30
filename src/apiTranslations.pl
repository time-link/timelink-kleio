:-module(apiTranslations,[
    translations/5,
    translations_translate/3,
    translations_delete/3,
    translations_get/3,
    translate/3,
    file_queued/2,
    file_queued/3,
    files_queued/2,
    file_processing/2,
    file_processing/3,
    files_processing/2,
    kleio_processing_status/3,
    get_stru/3                  %get_stru(+Params,+Id,-StruFile) is det.
        ]).
/** <module> Api operations dealing with translation
      
    Start a translation, delete translation results
    
    */ 
:-use_module(apiSources).
:-use_module(restServer).
:-use_module(logging).
:-use_module(kleioFiles).
:-use_module(tokens).
:-use_module(threadSupport).
:-use_module(reports).
:-use_module(utilities).
:-use_module(persistence).
:-use_module(topLevel).
:-use_module(errors).
:-use_module(counters).

%! translations(+Method,+Object,+Mode,+Id,+Params) is det.
% ### Method = post or json translations_translate
%
% Starts a translation. 
% 
% If Object points to a directory translates files in the directory
% 
% Parameters:
% $ structure: structure file to be used in the translation
% $ echo     : if yes the "rpt" file will include the source lines
% $ recurse  : descend into tsub directories (default no)
% $ status: filter files by translation status.
% $ spawn    : if yes the files are distributed to different workers and translated in parallell.
%               if no (default) the files will be translated by a single worker, with the stru file being processed 
%               only once, at the start of the translation. In multi-user environment spawn=no should be used, so that
%               different users can accesss freee workers more easily.
%

translations(post,Path,Mode,Id,Params):-
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (tokens:is_api_allowed(Token,translations) -> 
        true
    ;
        (
        option(request(Request),Params,[]),
        option(path_info(Url),Request, translations_post),   
        throw(http_reply(forbidden(Url),['Request-id'(Id)])) 
        ) 
    ),
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    logging:log_debug('API translations absolute: ~w',[AbsPath]),
    (exists_file(AbsPath) -> 
        Files = [Path]
        ;
        apiSources:sources_in_dir(Path,Params,Files)
    ),
    % TODO: if there is status(S) a filter is necessary, must do kleio_translation_status and filter_translations
    get_absolute_paths(Files,AbsFiles,TokenInfo),
    option(echo(Echo),Params,no),
    option(spawn(Spawn),Params,no),
    % get_stru(Params,Id,StruFile),
    % TBD replace with get_strus(Files,Params,Id,StruFiles) and pass to spawn_work
    get_strus(AbsFiles,Params,Id,StruFiles),
    spawn_work(Spawn,AbsFiles,StruFiles,Echo,Jobs),
    convert_jobs_to_relative_paths(Jobs,RJobs,TokenInfo),
    translations_results(Mode,Id,Params,RJobs).

% Returns the kleio_set of translated files
% 
translations(get,Path,Mode,Id,Params):-
    option(token_info(TokenInfo),Params),
    option(token(Token),Params),
    (tokens:is_api_allowed(Token,translations) -> 
        true
    ;
        (
        option(request(Request),Params,[]),
        option(path_info(Url),Request, translations_get),   
        throw(http_reply(forbidden(Url),['Request-id'(Id)])) 
        ) 
    ),
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    logging:log_debug('API translations absolute: ~w',[AbsPath]),
 
    % get status from cache if many files and young cache
    % this is fragile, because it depends on store_status_cache to reproduce the handling of
    % parameters in Params  by sources_in_dir and get_translation_status.
    % but any caching needs to be tied to the params of the call that can determine the files retrieved and
    % and the absolute to relative path mapping,
    CacheOptions=[max_age(30),max_age_large_set(180),large_set(1000),min_set(100)],
    (   %If cached return from cache
        get_status_from_cache(AbsPath,Params,RSets,CacheOptions)
    ; % Else compute status
        (
            (exists_file(AbsPath) -> 
                Files = [Path]
        ;
            apiSources:sources_in_dir(Path,Params,Files)
        ),
        get_translation_status(Files,RSets,TokenInfo),
        store_status_cache(AbsPath,Params,Files,RSets,CacheOptions)
    )
    ),    
    %% store in cache store_cache(AbsPath,Files,Filtered)
    filter_translations_by_status(Params,RSets,Filtered),
    translations_get_results(Mode,Id,Params,Filtered).

translations(delete,Path,Mode,Id,Params):-
    option(token(Token),Params),
    (is_api_allowed(Token,translations) -> 
        true
    ;
        (
        option(path_info(Url),Params, translations_delete),   
        throw(http_reply(forbidden(Url),['Request-id'(Id)])) 
        ) 
    ),
    option(token_info(TokenInfo),Params),
    kleio_resolve_source_file(Path,AbsPath,TokenInfo),
    path_type(AbsPath,Type),
    clean_translation(Path,Type,Id,Params,Results),
    clean_translation_results(Mode, Id, [type(Type)],Results).


%! translations_translate(+Mode,+Id,+Params) is det.
%
% This is the json-rpc entry point for REST POST translations
%
translations_translate(json,Id,Params):-
    option(path(Path),Params,''),
    translations(post,Path,json,Id,Params).

%! translations_get(+Mode,+Id,+Params) is det.
%
% This is the json-rpc entry point for REST get translations
%
translations_get(json,Id,Params):-
    option(path(Path),Params,''),
    translations(get,Path,json,Id,Params).

%! translations_delete(+Mode,+Id,+Params) is det.
%
% This is the json-rpc entry point for REST delete translations
%
translations_delete(json,Id,Params):-
    option(path(Path),Params,''),
    translations(delete,Path,json,Id,Params).

% Predicates assisting in transaltion


%% get_status_from_cache(+AbsPath,+Params,-RSets,+Options) is det.
% 
% returns kleio sets for AbsPath from cache if cache age is less than stored MaxAge property
%   
% Fails if there is no previously store cache on if conditions are not met.
%
% This prevents overburdening the server with consecutive calls to translations_get, with can be expensive.
% 
get_status_from_cache(AbsPath,Params,RSets,_):-
    log_debug('KleioSET cache CHECKING token ~w path: ~w ~n',[Params,AbsPath]),
    get_time(Now),
    option(recurse(Recurse),Params,no),
    option(token(Token),Params,Token),
    atomic_list_concat([AbsPath,Recurse,Token], '-',Key),
    get_shared_prop(Key,status_cache_time,T0),
    get_shared_prop(Key,max_cache_age,MaxAge),
    Age is Now-T0,
    Age < MaxAge,
    get_shared_prop(Key,status_cache_rsets,RSets),
    log_debug('KleioSET cache FETCHING ~w for ~w age: ~w ~n',[Key,AbsPath,Age]),!.
get_status_from_cache(AbsPath,Params,_,_):- % cache invalid, erase
    get_time(Now),
    option(recurse(Recurse),Params,no),
    option(token(Token),Params,Token),
    atomic_list_concat([AbsPath,Recurse,Token], '-',Key),
    get_shared_prop(Key,status_cache_time,T0),
    get_shared_prop(Key,max_cache_age,MaxAge),
    Age is Now-T0,
    log_debug('KleioSET cache INVALIDATING ~w for ~w Age: ~w max:~w~n',[Key,AbsPath,Age,MaxAge]),
    del_shared_prop(Key,status_cache_time),
    del_shared_prop(Key,status_cache_rsets),
    del_shared_prop(Key,max_cache_age),
    fail.
get_status_from_cache(AbsPath,Params,_,_):- 
    log_debug('KleioSET cache NO_RECORD for ~w params: ~w ~n',[AbsPath,Params]),
    fail.

%% store_status_cache(+AbsPath,+Params,+RSets,+Options) is det.
%
%  Stores RSets in cache associated with AbsPath.
%
%  Options
% CacheOptions=[max_age(30),max_age_large_set(180),large_set(1000),min_set(100)],
%   min_set(N) - only store if number of files is > N
%   max_age(A) - store cache for maximum of A seconds
%   max_age_large_set(O) - store cache for maximum of O seconds if set is large
%   large_set(L) - consider large set if number of files > L
%

store_status_cache(AbsPath,Params,Files,RSets,Options):-
    option(recurse(Recurse),Params,no),
    option(token(Token),Params,Token),
    atomic_list_concat([AbsPath,Recurse,Token], '-',Key),
    option(min_set(N),Options,100),
    option(large_set(L),Options,1000),
    option(max_age(A),Options,30),
    option(max_age_large_set(O),Options,180),
    length(Files,FN),
    FN > N, % if small set of files ingore
    (FN > L -> MaxAge=O;MaxAge=A), % set age acording to size
    get_time(Now),
    set_shared_prop(Key,status_cache_time,Now),
    set_shared_prop(Key,max_cache_age,MaxAge),
    set_shared_prop(Key,status_cache_rsets,RSets).   
store_status_cache(_,_,_,_,_):-!.


get_translation_status(Files,RSets,TokenInfo):-
    get_absolute_paths(Files,AbsFiles,TokenInfo),
    (setof(Result,AbsFile^(member(AbsFile,AbsFiles),kleio_translation_status(AbsFile,Result,TokenInfo)),RSets)
    ;
    RSets=[]),!.

spawn_work(yes,[File|AbsFiles],[Stru|StruFiles],Echo,[JobId|Jobs]):-
    set_prop(translations,jobs,[]),
    spawn_work2(yes,[File|AbsFiles],[Stru|StruFiles],Echo,[JobId|Jobs]),
    fail.
spawn_work(yes,_,_,_,Jobs):-
    get_prop(translations,jobs,Jobs).

% no span processing only available for single stru set
spawn_work(no,AbsFiles,[StruFile],Echo,[job(JobId,AbsFiles)]):-
    post_job(translate(AbsFiles,StruFile,Echo),JobId),!.
% if more than one stru file is given, use span processing
spawn_work(no,AbsFiles,StruFiles,Echo,Jobs):-
    spawn_work(yes,AbsFiles,StruFiles,Echo,Jobs).

spawn_work2(yes,[File|AbsFiles],[StruFile|StruFiles],Echo,[JobId|Jobs]):-
    post_job(translate(File,StruFile,Echo),JobId),
    spawn_work2(yes,AbsFiles,StruFiles,Echo,Jobs),
    add_to_prop(translations,jobs,job(JobId,[File])).
spawn_work2(yes,[],[],_,[]):-!.



%! get_stru(+Params,+Id,-StruFile) is det.
% Get the path to the structure file associated with a translation request
% Parameters:
%  structure: structure file to be used in the translation
%
get_stru(Params,Id,StruFile):-
    option(token_info(TokenInfo),Params),
    kleiofiles:kleio_default_stru(DefaultStru),
    (option(structure(SFile),Params) ->
        (
            kleio_resolve_structure_file(SFile,StruFile,TokenInfo),
            (
                exists_file(StruFile) -> true
                ; 
                throw(error(Id,-32602,'Structure file does not exist'-StruFile))
            )
        )
        ;
        (
            StruFile=DefaultStru,
            (
                exists_file(StruFile) -> true
                ; 
                throw(error(Id,-32602,'Default structure file does not exist'-StruFile))
            )
        )
    ).

%! get_strus(+Files,+Params,+Id,-StruFiles) is det.
% Get the path to the structure file associated with a translation request
% Parameters:
%  Files: list of files to be translated
%  Params: parameters of the request
%  Id: request id
%  StruFiles: structure files to be used in the translation
%
%  TBD - should be a list of structure files
get_strus(Files,Params,Id,StruFiles):-
    get_stru(Params,Id,DefaultStruFile),
    get_stru_for_files(Files,DefaultStruFile,StruFiles).

get_stru_for_files([F|Fs],DefaultStruFile,[S|Ss]):-
    get_stru_for_file(F,DefaultStruFile,S),
    get_stru_for_files(Fs,DefaultStruFile,Ss).

get_stru_for_files([],_,[]).

% TBD - implement https://github.com/time-link/timelink-kleio/issues/7
get_stru_for_file(File,__DefaultStruFile,StruFile):-
    % get directories in path
    prolog_to_os_filename(PrologFile, File),
    file_directory_name(PrologFile,PrologPath),
    % break path into directories
    atomic_list_concat(Dirs,'/',PrologPath),
    file_base_name(File,BaseName),
    file_name_extension(BaseNameNoExt,_,BaseName),
    match_stru_to_file(Dirs,BaseNameNoExt,StruFile),
    !.
get_stru_for_file(__,DefaultStruFile,DefaultStruFile):-!.

% match cli file to stru with same name in structures directory
match_stru_to_file(Dirs,BaseNameNoExt,StruFile):-
    select('sources',Dirs,'structures',Dirs1),
    atomic_list_concat(Dirs1,'/',Path),
    atomic_list_concat([Path,'/',BaseNameNoExt,'.str'],'',StruFile),
    exists_file(StruFile),!.

% match cli file sources/.../dir/xpto.cli with structures/dir.str 
match_stru_to_file(Dirs,_,StruFile):-
    last(Dirs,LastDir), % get last directory 
    select('sources',Dirs,'structures',StruPath),
    reverse(StruPath,RStruPath),
    % get path to structures directory depth first
    append(_,[structures|PathToStructures],RStruPath),
    reverse(PathToStructures,SubPath),
    % add file with lastdir name and extension str
    file_name_extension(LastDir,'str',DirStru),
    append(SubPath,[structures,DirStru],Path),
    atomic_list_concat(Path,'/',StruFile),
    exists_file(StruFile),!.

% match cli file with gacto2.str in structures directory depth first
match_stru_to_file(Dirs,_,StruFile):-
    select('sources',Dirs,'structures',StruPath),
    % remove last element of Dirs1
    reverse(StruPath,RStruPath),
    append(_,RSubPath,RStruPath),
    reverse(RSubPath,SubPath),
    atomic_list_concat(SubPath,'/',Path),
    (atomic_list_concat([Path,'/','gacto2.str'],'',StruFile)
    ;
    atomic_list_concat([Path,'/','sources.str'],'',StruFile)),
    exists_file(StruFile),!.



extract_file_names(FileStatus,Names):-
    bagof(N,F^N^(member(F,FileStatus),extract_file_name(F,N)),Names),!.
extract_file_names(_,[]).

extract_file_name((File,FTStatus,_,_),File):-FTStatus \= 'D'.

% TODO: should go to kleioFiles and made public
%
get_absolute_paths([F|Fs],[A|As],TokenInfo):-
    kleio_resolve_source_file(F,A,TokenInfo),
    get_absolute_paths(Fs,As,TokenInfo).
get_absolute_paths([],[],_):-!.

%% translate(+KleioFiles,+StruFiles,+Options) is det.
%
% translates KleioFiles using the definitions on StruFiles.
% * echo: if yes echoes in the "rpt" file the source lines
% * 
%
translate(DatFile,StruFile,Echo):-
    (is_list(DatFile)->Files=DatFile;Files=[DatFile]),
    log_info('Translations, dat:~w, str:~w, echo:~w',[Files,StruFile,Echo]),
    clio_init,
    put_value(echo,Echo),
    log_info('starting processing of stru file ~w~n',[StruFile]),
    with_mutex(StruFile,sync_stru(StruFile)),
    %sync_stru(StruFile),
    log_info('Finished processing of stru file ~w~n',[StruFile]),
    member(File,Files),
    log_info('starting processing of dat file ~w~n',[File]),
    with_mutex(File,sync_dat(File)),
    %sync_dat(DatFile),
    log_info('Finished processing of dat file ~w~n',[File]),
    fail,
    !.
translate(_,_,_):-!.


%% sync_stru(+StruFile) is det.
%  Encapsulates processing of a stru file in a way that can be synchronized on the
%  stru file name with a mutex, i.e., with_mutex(StruFile,sync_stru(StruFile))
sync_stru(StruFile):-
    put_value(stru_file,StruFile),
    path_sep(Sep),
    break_fname(StruFile,SPath,_SFile,SBase,_),
    list_to_a0([SPath,Sep,SBase,'.','srpt'],Report),
    prepare_report(Report,[type(noconsole)]),
    catch(stru(StruFile),E,error_out(['Unexpected error during structure procesing ',E])),
    close_report_file,!.

sync_dat(DatFile):-
    put_value(data_file,DatFile),
    path_sep(Sep),
    break_fname(DatFile,PD,_FileD,BaseD,_),
    put_value(data_dir,PD),
    list_to_a0([PD,Sep,BaseD,'.','rpt'],ReportD),
    list_to_a0([PD,Sep,BaseD,'.','err'],ReportErrors),
    (exists_file(ReportErrors) ->delete_file(ReportErrors);true),
    prepare_report(ReportD,[type(noconsole)]),
    catch(dat(DatFile),E,error_out(['Unexpected error during translation',E])),
    prepare_report(ReportErrors,[type(noconsole)]),
    report([errors:perror_count]),
    close_report_file,!.

%% convert_jobs_to_relative_paths(+Jobs,-RJobs,+Options) is det.
% convert path references in job terms from absolute to relative.
%
convert_jobs_to_relative_paths([],[],_):-!.
convert_jobs_to_relative_paths([job(J,F)|Js],[job(J,R)|Rs],Options):-
    kleiofiles:kleio_resolve_source_list(R,F,Options),
    convert_jobs_to_relative_paths(Js,Rs,Options).

   
%% kleio_translation_status(+KleioFile,-KleioTranslationStatus,+Options) is det.
%
% KleioTranslationStatus is a list with information about the translation results of a source.
% It merges information form the KleioSet (see kleio_file_set/3) in a single option list with the following information:
%
%       * name(N) the name of the file in File, without the Directory
%       * path(P) the full path to the file.
%       * status(T) translation status V,T,E or W for translated files, P (processing) Q(queued) for files in the translation pipeline
%       * modified(T) the time of last modification , as a float
%       * modified_string(FT) the formated time of last modification using format '%Y-%m-%d %H:%M:%S'
%       * modified_rfc1123(RFC) modification time in rfc1123 (used in http)
%       * modified_iso(ISO) modification time in ISO8601.
%       * size(S) the size of file in bytes.
%
%   If the file is being translated
%
%       * ttime(TT) the time the file started to translate
%       * ttime_string(STT) string version of ttime.
%
%  If the file is waiting in queue to be translated
%
%       * qtime(QT) the time when the file was queued
%       * qtime_string(SQT) string version de qtime
%
%   If the file was translated:
%
%       * errors(E) number of errors in translation.
%       * warnings(W) number of warnings in translation.
%       * version(V) ClioInput version string
%       * translated(T) the time of the last translation as a float
%       * translated_string(S) S is the time of last translation in format "day-month-year hour-minutes"
%       * rpt_url(RU) is the URL to fetch the translation report
%       * xml_url(XU) is the URL to fetch the xml file
%       * more_url(MU) is the URL to fetch a properties file with extra info about the source
% 
kleio_translation_status(KleioFile,TranslationStatus, Options) :-
    kleio_file_set_relative(KleioFile,
            [kleio(KleioAttrs),
            rpt(RptAttrs),
            err(ErrAttrs),
            xml(XMLAttrs),
            org(_),  
            old(_),
            ids(_)],Options),
    option(name(Name),KleioAttrs),
    option(path(Path),KleioAttrs),
    option(directory(Directory),KleioAttrs),
    kleio_processing_status(KleioFile,P-PTime,Q-QTime),
    time_string(QTime,QTimeString),time_string(PTime,PTimeString),
    option(tstatus(TStatus),KleioAttrs,'?'),
    (P='P' -> Status=P; (Q='Q' -> Status=Q ; Status=TStatus)),
    option(modified(Modified),KleioAttrs),   
    option(modified_string(ModifiedS),KleioAttrs),
    option(modified_rfc1123(ModifiedRFC),KleioAttrs),
    option(modified_iso(ModifiedISO),KleioAttrs),
    option(size(Size),KleioAttrs),
    make_rest_url(Path,'sources/',SU),
    (ErrAttrs\=[] 
        -> % file was translated?
        (
        option(errors(Errors),ErrAttrs),
        option(warnings(Warnings),ErrAttrs),
        option(version(Version),ErrAttrs),
        option(translated(TTime),ErrAttrs),
        option(translated_string(S),ErrAttrs),
        option(path(RptPath),RptAttrs,''),
        option(path(XmlPath),XMLAttrs,''),
    
        make_rest_url(RptPath,'reports/',RU),
    
        make_rest_url(XmlPath,'exports/',XU),
        TAttributes=[errors(Errors),warnings(Warnings),
                    version(Version),
                    translated(TTime),translated_string(S),
                    rpt_url(RU),xml_url(XU)]
        )
        ;
        (TAttributes=[])
        ),
    TranslationStatus=[name(Name),path(Path),source_url(SU),status(Status),
                modified(Modified),modified_string(ModifiedS),modified_rfc1123(ModifiedRFC),modified_iso(ModifiedISO),
                size(Size),directory(Directory),ttime(PTime),ttime_string(PTimeString),qtime(QTime),qtime_string(QTimeString)|TAttributes],
    !.

time_string(T,S):- format_time(string(S),'%Y-%m-%d %H:%M:%S',T),!.

%% kleio_processing_status(+File, ?PStatus,?QStatus) is det.
% If file is currently translating PStatus=P-StartTime if it is queued QStatus = Q-QueueTime (can be both).
%
kleio_processing_status(File, PStatus-PTime,QStatus-QTime):-
    get_queued(Queued),get_processing(Processing),
    (file_processing(Processing,File,TimeP) ->
        (PStatus = 'P',PTime=TimeP)
    ;
        (PStatus = ' ',PTime=0)
    ),
    (file_queued(Queued,File,TimeQ) ->
        (QStatus = 'Q',QTime=TimeQ)
    ;
        (QStatus = ' ',QTime=0)
    )
    ,!.

filter_translations_by_status(Params,Translations,Filtered):-
    option(status(TStatus),Params,no),
    filter_translations(TStatus,Translations,Filtered).

filter_translations(_,[],[]):-!.
filter_translations(no,T,T):-!.
filter_translations(TStatus,[Translation|More],[Translation|MoreFiltered]):-
    option(status(Status),Translation),
    (TStatus=no;TStatus=Status),!,
    filter_translations(TStatus,More,MoreFiltered).

filter_translations(Filters,[_|More],MoreFiltered):-
    filter_translations(Filters,More,MoreFiltered),
    !.


%! translation_results(+Mode,+Id,+Params,+Jobs) is det.
%
% Outputs the result of the translations call which is a list of job ids and files associated with the job.
translations_results(rest,Id,_,[]):- % we put the jobs in the format JobId(File) for easy display
    default_results(rest,Id,_,[]).
 translations_results(rest,Id,_,Jobs):- % we put the jobs in the format JobId(File) for easy display
    bagof(J,Job^(member(Job,Jobs),make_term(Job,J)),Jobs2),
    default_results(rest,Id,_,Jobs2).
translations_results(json,null,_,_):-!. % "notification" in jsonrpc spec. Id = null
translations_results(json,Id,_,Jobs):- % TODO: ????return link to download file
    Id \= null,
    convert_jobs_to_dicts(Jobs,Result),
    default_results(json,Id,_,Result).

make_term(job(N,F),T):-
    atom_number(A, N),
    T =..[A,F].
    

convert_jobs_to_dicts([],[]):-!.
convert_jobs_to_dicts([job(JobId,Sources)|Jobs],[job{job:JobId,sources:Sources}|DJobs]):-
    convert_jobs_to_dicts(Jobs,DJobs).

translations_get_results(Mode,Id,_,[]):-
    default_results(Mode,Id,_,[]),
    !.
translations_get_results(Mode,Id,_,RSets):-
    bagof(DK,K^(member(K,RSets),dict_options(DK,K)),Result),
    default_results(Mode,Id,_,Result),
    !.

%% 
%% file_queued(+File,-Time) is non det.
%
%  Checks if File is in  waiting queue, and fetches the time on entry in queue.
%
file_queued(File,Time):- 
    threadSupport:get_queued(Queued),
    member(queued(_Q,JobInfo),Queued),
    option(time(Time),JobInfo),
    option(job(Job),JobInfo),
    Job = translate(Sources,_Stru,_Echo),
    (is_list(Sources) ->member(File,Sources);File=Sources).

%% 
%% file_queued(+Queue,+File,-Time) is non det.
%
%  Given a Queue list checks if File is in  waiting queue, and fetches the time on entry in queue.
%  
file_queued(Queued, File,Time):- 
    member(queued(_Q,JobInfo),Queued),
    option(time(Time),JobInfo),
    option(job(Job),JobInfo),
    Job = translate(Sources,_Stru,_Echo),
    (string(File) -> atom_string(AFile, File); AFile=File),
    (is_list(Sources) ->member(AFile,Sources);AFile=Sources).

%% files_queued(+Files,?Queued) is det.
%
% Queued is the list of members of Files currently in queue for translation.
%
files_queued(Files,QueuedFiles):-
    threadSupport:get_queued(Queue),
    (setof(F,T^file_queued(Queue,F,T),FilesInQueue)->
        true
        ;
        FilesInQueue=[]
    ),
    list_to_set(Files,FSet),
    intersection(FilesInQueue,FSet,QueuedFiles),!.
    

%% files_processing(+Files,?Processing) is det.
%
% Processing is subset of Files currently being translated.
%
files_processing(Files,ProcessingFiles):-
    threadSupport:get_processing(Processing),
    (setof(F,T^file_processing(Processing,F,T),InProcessFiles)->
        true
        ;
        InProcessFiles=[]
        ),
    list_to_set(Files,FSet),
    intersection(InProcessFiles,FSet,ProcessingFiles),!.

%% file_processing(+File,-Time) is det.
%
%  Checks if File is currently being processed, and fetches the time when processing started.
%
file_processing(File,Time):-
    threadSupport:get_processing(Processing),
    member(processing(_P,JobInfo),Processing),
    option(time(Time),JobInfo),
    option(job(Job),JobInfo),
    Job = translate(Sources,_Stru,_Echo),
    (string(File) -> atom_string(AFile, File); AFile=File),
    (is_list(Sources) ->member(AFile,Sources);AFile=Sources).

%% file_processing(+Queue,+File,-Time) is det.
%
%  Given a job list checks if File is currently being processed, and fetches the time when processing started.
%
file_processing(Processing,File,Time):-
    member(processing(_P,JobInfo),Processing),
    option(time(Time),JobInfo),
    option(job(Job),JobInfo),
    Job = translate(Sources,_Stru,_Echo),
    (is_list(Sources) ->member(File,Sources);File=Sources).

%% clean_translation(+Path,+Type,+Mode,+Id,+Params) is det.
%
% If Type=file clean translation at Path (resolving Path to absolute) . 
% If Type = directory, clean all translation in Path . 
%
% All files derivated from the translation process are 
% deleted.
% 
clean_translation(Path,directory,_Id,Params,_Results):-
    sources_in_dir(Path,Params,Files),
    files_processing(Files,Processing),
    files_queued(Files,Queued),
    subtract(Files,Processing,F1),
    subtract(F1,Queued,Result),
    set_prop(kleio,clean,[]),
    option(token_info(TokenInfo),Params),
    member(File,Result),
    log_debug('calling ~w~n',[kleio_file_delete(File)]),
    kleio_resolve_source_file(File,AbsFile,TokenInfo),
    kleio_file_clean(AbsFile),
    add_to_prop(kleio,clean,File),
    fail.

clean_translation(Path,file,_Id,Params,_Results):-
    set_prop(kleio,clean,[]),
    option(token_info(TokenInfo),Params),
    log_debug('delete file candidate:~w~n',[Path]),
    kleio_resolve_source_file(Path,AbsFile,TokenInfo),
    log_debug('calling ~w~n',[kleio_file_delete(Path)]),
    kleio_file_clean(AbsFile),
    add_to_prop(kleio,clean,Path),
    fail.

clean_translation(Path,notfound,Id,_Params,_Results):-
    throw(http_reply(not_found(Path),['Request-id'(Id)])).

clean_translation(_Path,_Type,_Id,_Params,Results):-
    get_prop(kleio,clean,Results).

%% clean_translation_results(+RequestType,+Id,+Params,+Results) is det.
% Output the result of the sources_get api call.
%
clean_translation_results(Mode,Id,_,Results):-
    default_results(Mode,Id,_,Results).
