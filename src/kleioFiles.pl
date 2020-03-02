:-module(kleiofiles,[
        dict_of_files/1,
        directories_from_files/2,
        file_attributes/2,
        get_file_attribute/3,
        find_files_with_extension/3,
        find_files_with_extension/4,
        find_directories/3,
        find_directories/2,
        path_type/2,
        kleio_file_set/2,
        kleio_file_set_relative/3,
        kleio_file_status/2,
        kleio_conf_dir/1,
        kleio_home_dir/1,
        kleio_stru_dir/1,
        kleio_source_dir/1,
        kleio_log_dir/1,
        kleio_token_db/1,
        kleio_default_stru/1,
        kleio_user_source_dir/2,
        kleio_user_structure_dir/2,
        kleio_resolve_source_file/3,
        kleio_resolve_source_list/3,
        kleio_resolve_structure_file/3,
        kleio_file_clean/1,
        kleio_file_delete/1,
        kleio_mime_type/2
    ]).

:-use_module(shellUtil).
:-use_module(persistence).
:-use_module(utilities).
:-use_module(logging).
/** <module> Utilities for Kleio files
 
## Kleio Files 

The translation of kleio source files (.cli) produces several output files.

The predicates in this module help the management of those related files and
provide further utilities related to management of kleio files in the file system.

 * Extracted from utilities.pl 17 Oct 2018.

 */

%% kleio_file_set(+KleioFile,-KleioFileSet)is det.
%
%  KleioFileSet is information on the files produced by the translation of
%  of KleioFile. The translation process produces files with specific extensions:
%  rpt file with a report of the translation in human readable form
%  err file with a summary of errors and waring in machine readable form
%  xml file with the exported data from the kleio file
%  org file with the original kleio file before the first translation (no explicit ids)
%
% Warning: this predicate returns absolute file paths in the file attributes. Use kleio_file_set_relative/3
% for safe return of relative paths computed from token information.
%
% KleiFileSet is a list of:
%   kleio(Attributes),rpt(Attributes),err(Attributes),xml(Attributes), org(Attributes).
%   where Attributes are file attributes as produced by file_attributes/2
%
kleio_file_set(KleioFile,[kleio([tstatus(KStatus)|KleioAttrs]),rpt(RptAttrs),err(ErrAttrs),xml(XMLAttrs),org(OrgAttrs),old(OldAttrs),ids(IdsAttrs)]):-
    file_attributes(KleioFile,KleioAttrs),
    (exists_directory(KleioFile) -> 
        (RptAttrs=[],ErrAttrs=[],XMLAttrs=[],OrgAttrs=[],OldAttrs=[],IdsAttrs=[],KStatus='D') % is a directory
    ;
    (
    file_name_extension(BaseName,_Ext,KleioFile), % extract the base name of the name without extension
    file_name_extension(BaseName,rpt,RptFile),
    file_name_extension(BaseName,err,ErrFile),
    file_name_extension(BaseName,xml,XMLFile),
    file_name_extension(BaseName,org,OrgFile),
    file_name_extension(BaseName,old,OldFile),
    file_name_extension(BaseName,ids,IdsFile),
    (exists_file(RptFile) -> file_attributes(RptFile,RptAttrs); RptAttrs=[]),
    (exists_file(ErrFile) -> file_attributes(ErrFile,ErrAttrs); ErrAttrs=[]),
    (exists_file(XMLFile) -> file_attributes(XMLFile,XMLAttrs); XMLAttrs=[]),
    (exists_file(OrgFile) -> file_attributes(OrgFile,OrgAttrs); OrgAttrs=[]),
    (exists_file(OldFile) -> file_attributes(OldFile,OldAttrs); OldAttrs=[]),
    (exists_file(IdsFile) -> file_attributes(IdsFile,IdsAttrs); IdsAttrs=[]),
    kset_status([kleio(KleioAttrs),rpt(RptAttrs),err(ErrAttrs),xml(XMLAttrs),org(OrgAttrs),old(OldAttrs),ids(IdsAttrs)],KStatus)
    )),!.

%% kleio_file_set_relative(+KleioFile,-RelativeKleioFileSet,+TokenOptions) is det.
% Same as kleio_file_set, but the paths are relative to the user sources directory as contained in a Token.
% This is needed so that the API does not return absolute files names, posing a security risk.
%
kleio_file_set_relative(KleioFile,RelativeKleioFileSet, Options) :-
    kleio_file_set(KleioFile,[kleio([tstatus(KStatus)|KleioAttrs]),rpt(RptAttrs),err(ErrAttrs),xml(XMLAttrs),org(OrgAttrs),old(OldAttrs),ids(IdsAttrs)]),
    file_attributes_relative(KleioAttrs,RelKleioAttrs,Options),
    file_attributes_relative(RptAttrs,RelRptAttrs,Options),
    file_attributes_relative(ErrAttrs,RelErrAttrs,Options),
    file_attributes_relative(XMLAttrs,RelXMLAttrs,Options),
    file_attributes_relative(OrgAttrs,RelOrgAttrs,Options),
    file_attributes_relative(OldAttrs,RelOldAttrs,Options),
    file_attributes_relative(IdsAttrs,RelIdsAttrs,Options),
    RelativeKleioFileSet = [kleio([tstatus(KStatus)|RelKleioAttrs]),rpt(RelRptAttrs),err(RelErrAttrs),xml(RelXMLAttrs),org(RelOrgAttrs),old(RelOldAttrs),ids(RelIdsAttrs)],
    !.

%! kleio_file_clean(+KleioFile) is det.
%  
% Cleans the translation results of a kleio source file. The translation results are the
% files with extensions: err, rpt, xml and old. Note that files with extension "org" are
% not the result of translation, they are in fact the original file before the first translation.
%
kleio_file_clean(File):-
    kleio_file_set(File,Set),
    member(Type,[xml,err,rpt,ids]),
    RelatedFile =..[Type,Attributes],
    option(RelatedFile,Set),
    option(path(P),Attributes),
    delete_file(P),
    fail.
kleio_file_clean(_):-!.

%! kleio_file_delete(+KleioFile) is det.
%  
% Delete a kleio source file and all the translation results. The trabslation results are the
% files with extensions: err, rpt, xml, org and old. 
%
% To delete just the translation results see kleio_file_clean/1.
%
kleio_file_delete(File):-
    kleio_file_set(File,Set),
    delete_file(File),
    member(Type,[xml,err,rpt,ids,org,old]),
    RelatedFile =..[Type,Attributes],
    option(RelatedFile,Set),
    option(path(P),Attributes),
    delete_file(P),
    fail.
kleio_file_delete(_):-!.

%% kleio_file_status(+File,-Status) is det.
%
%  Status represents the state of File in the current translation pipeline.
%   'T' - needs to be translated (either no rpt file or existing rpt older than kleio file)
%   'E' - was last translated with errors
%   'W' - was last translated with warnings
%   'V' - file has a valid trasnlation, can be imported.
% Note that status can overlap and this predicate will not backtrack, but give the most relevant
% state T > E > W > V
%
kleio_file_status(File,Status):- 
    kleio_file_set(File,Set),
    option(kleio(Attrs),Set),
    option(tstatus(Status),Attrs),!.

kset_status(S,'D'):-is_a_directory(S),!. 
kset_status(S,'T'):-needs_translation(S),!.
kset_status(S,'E'):-has_errors(S),!.
kset_status(S,'W'):-has_warnings(S),!.
kset_status(_,'V'):-!.

is_a_directory(KleioSet) :-
    option(kleio(K),KleioSet),
    option(is_directory(yes),K).

needs_translation(KleioSet):-
    option(kleio(K),KleioSet),option(err(E),KleioSet),option(modified(MK),K),option(modified(MR),E),MK>MR.
needs_translation(KleioSet):-
    option(rpt([]),KleioSet).
needs_translation(KleioSet):-
    option(err([]),KleioSet).
needs_translation(KleioSet):-
    option(xml([]),KleioSet).


has_errors(KleioSet):-
    option(err(E),KleioSet),option(errors(NE),E),NE > 0.

has_warnings(KleioSet):-
    option(err(E),KleioSet),option(warnings(NW),E),NW > 0.

%%  dict_of_files(-D) is det. 
%   List files recursively under the working directory, and 
%   unify D with the result in the form of the dict with 
%        <subdirectory name> - <sub list> for subdirectories. 
% 
% ?- dict_of_files(D). 

ignore_special_dots((.)). 
ignore_special_dots((..)). 

% 
dict_of_files(X):- directory_files((.), Files), 
                directory_files(Files, X, []). 
% 
directory_files([], X, X). 
directory_files([F|R], X, Y):- ignore_special_dots(F), !, 
           directory_files(R, X, Y). 
directory_files([D|R], [D-Z|X], Y):- exists_directory(D), !, 
                directory_files(D, Files), 
                working_directory(_, D), 
                directory_files(Files, Z, []), 
                working_directory(_, (..)), 
                directory_files(R, X, Y). 
directory_files([F|R], [F|X], Y):- directory_files(R, X, Y). 

%% find_files_with_extension(+BaseDir,+Ext,-Code, -Files) is det.
%  Return list of Files with extension Ext in BaseDir and its subdirectories
%
%  Uses shell command "find Basedir -type -f -name *.Ext".
%
%  Code is the shell code return and is equal to zero if command succeeded.
%
find_files_with_extension(BaseDir,Ext,Code,Files):-
    %CMD = ['find -f', BaseDir, '  \\( -type f -name *.',Ext,' -or -type d \\)',' -and -not -path \'*/\\.*\' ' ], % this also gets the dirs
    CMD = ['find ', BaseDir, ' -type f -name *.',Ext ], %this only gets the files.
    atomic_list_concat(CMD,'',S), 
    %writeln(shell:S),
    shell_to_list(S,Code,Files).

%% find_files_with_extension(+BaseDir,+Ext,-Files) is det.
% same as find_files_with_extension but only succeeds if return code for shell search is 0
%
find_files_with_extension(BaseDir,Ext,Files):-
    find_files_with_extension(BaseDir,Ext,0,Files).

%% find_directories(+BaseDir,-Code, -Dirs) is det.
%  Return list of sub directories of BaseDir
%
%  Uses shell command "find Basedir -type -d".
%
%  Code is the shell code return and is equal to zero if command succeeded.
%
find_directories(BaseDir,Code,Dirs):-
    CMD = ['find ', BaseDir, ' -type d', ' -and -not -path \'*/\\.*\' '], % this also gets the dirs
    atomic_list_concat(CMD,'',S), 
    %writeln(shell:S),
    shell_to_list(S,Code,Dirs).

%% find_directories(+BaseDir,-Dirs) is det.
% same as find_directories/3 but only succeeds if return code for shell search is 0
%
find_directories(BaseDir,Dirs):-
    CMD = ['find ', BaseDir, ' -type d', ' -and -not -path \'*/\\.*\' '], % this also gets the dirs
    atomic_list_concat(CMD,'',S), 
    %writeln(shell:S),
    shell_to_list(S,0,Dirs).


%% directories_from_files(+FileList,-DirectoryList) is det.
%  Extract to DirectoryList the directories of files in FileList.
directories_from_files(FileList,DiretoryList):-
    setof(Directory,F^(member(F,FileList),file_directory_name2(F,Directory)),DiretoryList).

%% path_type(+Path,?Type) is det.
%
% Path is of Type where type is directory, file, notfound (in case on non existant path)
%
path_type(Path,directory):-
    exists_directory(Path),!.
path_type(Path,file):-
    exists_file(Path),!.
path_type(_,notfound):-!.

% work around the problem that the builtin produces a atom directory from a File String
% file_directory_name("/Users/jrc/develop/mhk-git/clio/",D).
% D = '/Users/jrc/develop/mhk-git/clio'.
file_directory_name2(F,Directory):-
    file_directory_name(F,D),
    (string(F)->text_to_string(D, Directory); D=Directory).


%% files_attributes(+FileList,-FilesWithAttributes) is det.
% For each file F in FileList determine file attributes A and return a 
% list of file(F,A). See file_attributes.
%
files_attributes(FileList,Files):-
    findall(file(F,Attributes),
        (member(F,FileList),
        file_attributes(F,Attributes)),
        Files).

%% file_attributes(+File,-Attributes) is det.
%   Get the list of attributes of File.
%   Attributes is set to file(F,AttrList) where _AttrList_ contains:
%
%       * name(N) the name of the file in File, without the Directory
%       * path(P) the full path to the file.
%       * is_directory(YD) if the path is a diretory is_directory(yes) if not is_directory(no).
%       * if the file type is cli or kleio: tstatus(T) translation status V,T,E or W see kleio_file_status/2 
%       * directory(D) the directory of file.
%       * base(B) the base name of File without the extension
%       * base_path(BP) the ful base name with path without the file extension
%       * extension(E) the extension of File
%       * modified(T) the time of last modification , as a float
%       * modified_string(FT) the formated time of last modification using format '%Y-%m-%d %H:%M:%S'
%       * modified_rfc1123(RFC) modification time in rfc1123 (used in http)
%       * modified_iso(ISO) modification time in ISO8601.
%       * size(S) the size of file in bytes.
%       * if the file type is "err" (summary of translation errors) then the extra attributes are availabe:
%       * errors(E) number of errors in translation.
%       * warnings(W) number of warnings in translation.
%       * version(V) ClioInput version string
%       * translated(T) the time of the last translation as a float
%       * translated_string(S) S is the time of last translation in format "day-month-year hour-minutes"
%
file_attributes(F,[name(N),
                    % absolute(A),
                    path(F),
                    directory(D),
                    is_directory(YD),
                    base(B),
                    base_path(BP),
                    extension(E),
                    modified(T),
                    modified_string(FT),
                    modified_rfc1123(RFC),
                    modified_iso(ISO),
                    size(S)|More]):-
    % absolute_file_name(F, A),
    file_base_name(F,N),
    file_name_extension(B,E,N),
    file_name_extension(BP,E,F),
    file_directory_name(F,D),
    (exists_directory(F) -> YD=yes;YD=no),
    time_file(F,T),
    format_time(string(FT),'%Y-%m-%d %H:%M:%S',T), 
    format_time(string(RFC),'%a, %d %b %Y %T GMT',T), 
    format_time(string(ISO), '%FT%T%z',T),
    size_file(F,S),
    more_attributes(F,T,E,More),!. % get more attributes specific of the file type TODO: pass file time (T) to allow caching

% TODO: add extra time parameter and cache results globally. set_shared_prop(F,more_attributes,cache(T,[errors(E),....]), and a first clause get_shared_prop... if Cached > T ...)
more_attributes(F,T,err,MoreAttr):-
    get_shared_prop(F,more_attributes,cached(T1,MoreAttr)),
    format_time(string(TS),'%Y-%m-%d %H:%M:%S',T1), 
    log_debug('more_atributes of ~w fetched from cached at ~w~n',[F,TS]),
    T1 >= T,!.

more_attributes(F,T,err,MoreAttr):-
    list_to_a0([cat,' "',F,'"'],Cmd),
    shell_to_list(Cmd,0,XerrInfo),
    (XerrInfo = [Version,DateS,Err,Warn]; (XerrInfo=[Version,DateS,Err],Warn="-1 warnings")),
    extract_date(DateS,Date),
    format_time(string(DateFormated),'%Y-%m-%d %H:%M:%S',Date),
    split_string(Err," "," ",[ErrN|_]),
    number_string(E,ErrN),
    split_string(Warn," "," ",[WarnN|_]),
    number_string(W,WarnN),
    MoreAttr = [errors(E),warnings(W),version(Version),translated(Date),translated_string(DateFormated)],
    format_time(string(TS),'%Y-%m-%d %H:%M:%S',T), 
    log_debug('more_atributes of ~w at ~w cached.~n',[F,TS]),
    set_shared_prop(F,more_attributes,cached(T,MoreAttr)),
    !.

more_attributes(_,_,_,[]):-!.

extract_date(String,Date):-
    split_string(String," ", " ", [DateS,TimeS]),
    split_string(DateS,"-","-",[DayS,MonthS,YearS]),
    number_string(Day,DayS),number_string(Month,MonthS),number_string(Year,YearS),
    split_string(TimeS,"-","-",[HourS,MinuteS]),
    number_string(Hour,HourS),number_string(Minute,MinuteS),
    date_time_stamp(date(Year,Month,Day,Hour,Minute,0,_,_,_), Date),!.
    
%%  file_attributes_relative(OrgAttrs,RelKleioAttrs,Options)
%
% Converts file_attributes in relative file attributes using kleio_resolve_source_file/3
% Uses a list to avoid attributes that do not contain paths
%
file_attributes_relative([A|Attrs],[R|RelAttrs],Options):-
    A =.. [Attribute,Value],
    (member(Attribute,[is_directory,name,extension,modified,modified_string,modified_rfc1123,modified_iso,size])
    -> R=A % attribute does not contain a path
    ; (kleio_resolve_source_file(RelValue,Value,Options) -> 
        R =.. [Attribute,RelValue]
        ;
        R=A % argument cannot be made relative, use as it is
        )
    ), 
    file_attributes_relative(Attrs,RelAttrs,Options),
    !.
file_attributes_relative([],[],_):-!.


%% get_file_attribute(+File,+Attribute,?Value) is det.
% Get a specific attribute of a file. Fails if Attribute does not exist. See file_attributes/2 for list.
%
get_file_attribute(File,Attribute,Value):-
    file_attributes(File,Attributes),
    Option =.. [Attribute,Value],
    option(Option,Attributes).

%% kleio_home_dir(?D)is det.
% Returns the kleio home dir. 
%
% The translator assumes a standard working directory HOME with the following structure:
% * HOME/system/conf/kleio contains configuration information.
%       * HOME/system/conf/kleio/token_db is the token database.
%       * HOME/system/conf/kleio/stru/gacto2.str is the default structure file.
% * HOME/sources is the base directory for source files.
% * HOME/users/ is the base directory for user related information (e.g. specific stru files).
% ----
% The following environment variables can determine the location of main files and directories used by the system.
%
% * KLEIO_HOME_DIR defaults to, in order of preference:
%      *  ${KLEIO_HOME_DIR}
%      *  /kleio-home (normally mapped in a container)
%      *  /timelink-home (normally mapped in a container)
%      *  /mhk-home (normally mapped in a container)
%      *  . (current dir if ./system, ./sources, ./users exist)
%      *  ./kleio-home
%      *  ./timelink-home
%      *  ./mhk-home
%      *  ~/kleio-home 
%      *  ~/timelink-home
%
% * KLEIO_SOURCE_DIR defaults  KLEIO_HOME_DIR/sources/
% * KLEIO_CONF_DIR defaults KLEIO_HOME_DIR/system/conf/kleio
% * KLEIO_STRU_DIR  defaults KLEIO_HOME_DIR/system/con/kleio/stru/
% * KLEIO_TOKEN_DB defaults KLEIO_CONF_DIR/token_db
% * KLEIO_DEFAULT_STRU default KLEIO_HOME_DIR/system/con/kleio/stru/gacto2.str
%
% variable KLEIO_HOME or  ~/kleio-home or ~/timelink-home or ~/mhk-home.
kleio_home_dir(D):-
    getenv('KLEIO_HOME_DIR', D1),
    absolute_file_name(D1,D).

kleio_home_dir(D):-
    member(D,['/kleio-home','/timelink-home','/mhk-home']),
    exists_directory(D),
    !.

% current dir is a kleio-home type dir
kleio_home_dir(Home):-
    working_directory(Home,Home),
    atom_concat(Home,'/system',Sys),
    absolute_file_name(Sys,Sys1),
    exists_directory(Sys1),
    ( 
        (atom_concat(Home,'/sources',Sources),
        absolute_file_name(Sources,Sources1),
        exists_directory(Sources1))
    ;
        (atom_concat(Home,'/sources',Sources),
        absolute_file_name(Sources,Sources1),
        exists_directory(Sources1))
    ),
    atom_concat(Home,'/users',Users),
    absolute_file_name(Users,Users1),
    exists_directory(Users1).

kleio_home_dir(D):-
    working_directory(Home,Home),
    atom_concat(Home,'/kleio-home',D1),
    absolute_file_name(D1,D),
    exists_directory(D),
    !.
kleio_home_dir(D):-
    working_directory(Home,Home),
    atom_concat(Home,'/timelink-home',D1),
    absolute_file_name(D1,D),
    exists_directory(D),
    !.
kleio_home_dir(D):-
    working_directory(Home,Home),
    atom_concat(Home,'/mhk-home',D1),
    absolute_file_name(D1,D),
    exists_directory(D),
    !.
kleio_home_dir(D):-
    getenv('HOME',Home),
    atom_concat(Home,'/kleio-home',D1),
    absolute_file_name(D1,D),
    exists_directory(D),
    !.
kleio_home_dir(D):-
    getenv('HOME',Home),
    atom_concat(Home,'/timelink-home',D1),
    absolute_file_name(D1,D),
    exists_directory(D),
    !.
kleio_home_dir(D):-
    getenv('HOME',Home),
    atom_concat(Home,'/mhk-home',D1),
    absolute_file_name(D1,D),
    exists_directory(D),
    !.

%% kleio_conf_dir(?Dir) is det.
% Returns the configuration dir, normally KLEIO_HOME/system/conf/kleio but can be overriden by environment 
% variable KLEIO_CONF_DIR.
%
kleio_conf_dir(D):-getenv('KLEIO_CONF_DIR', D),!.
kleio_conf_dir(D):-
    kleio_home_dir(H),
    atom_concat(H, '/system/conf/kleio', D1),
    absolute_file_name(D1,D),!.
kleio_conf_dir('.').
    
%% kleio_token_db(?Path) is det.
% Returns the path to the token database, normally KLEIO_HOME/system/conf/kleio/token_db,
%  but can be overriden by environment variable KLEIO_TOKEN_DB.
%
kleio_token_db(D):-getenv('KLEIO_TOKEN_DB', D),!.
kleio_token_db(D):-
    kleio_conf_dir(C),
    atom_concat(C, '/token_db', D1),
    absolute_file_name(D1,D),!.

kleio_token_db('./token_db').

%% kleio_source_dir(?Dir) is det.
% Returns the sources base dir, normally KLEIO_HOME/sources but can be overriden by environment 
% variable KLEIO_SOURCE_DIR.
%
kleio_source_dir(D):-getenv('KLEIO_SOURCE_DIR', D),!.
kleio_source_dir(D):-
    kleio_home_dir(H),
    atom_concat(H, '/sources', D1),
    absolute_file_name(D1,D),!.

kleio_source_dir('.').

%% kleio_log_dir(?Dir) is det.
% Returns the log base dir, normally KLEIO_HOME/system/logs/kleio/ but can be overriden by environment 
% variable KLEIO_LOG_DIR.
%
kleio_log_dir(D):-getenv('KLEIO_SOURCE_DIR', D),!.
kleio_log_dir(D):-
    kleio_home_dir(H),
    atom_concat(H, '/system/logs/kleio', D1),
    absolute_file_name(D1,D),!.

kleio_log_dir('./system/logs/kleio/').

%% kleio_stru_dir(?Dir) is det.
% Returns the structures base dir, normally KLEIO_HOME/system/conf/kleio/stru but can be overriden by environment 
% variable KLEIO_STRU_DIR.
% TODO not sure this makes sense. STRU files should be close to the source files they describe, with generic ones in KLEIO_CONF_DIR/stru
kleio_stru_dir(D):-getenv('KLEIO_STRU_DIR', D),!.
kleio_stru_dir(D):-
    kleio_conf_dir(H),
    atom_concat(H, '/stru', D1),
    absolute_file_name(D1,D),!.

kleio_stru_dir('.').

%% kleio_default_stru(?Path) is det.
% Returns the path to the default stru for translations, 
% normally KLEIO_STRU_DIR/gacto2.str but can be overriden by environment 
% variable KLEIO_DEFAULT_STRU.
%
kleio_default_stru(D):-getenv('KLEIO_DEFAULT_STRU', D),!.
kleio_default_stru(D):-
    kleio_stru_dir(H),
    atom_concat(H, '/gacto2.str', D1),
    absolute_file_name(D1,D),!.

kleio_default_stru('src/gacto2.str').

%% kleio_user_source_dir(?Dir,+Options) is det.
%
% Gets the base dir for sources using token information
% The Dir is:
% 
% Base Dir =  KLEIO_HOME / S from option(sources(S),Options) 
%
kleio_user_source_dir(Dir,Options):-
    kleio_home_dir(Kleio_home_dir),
    option(sources(UserSourceDir),Options),  
    atomic_list_concat([Kleio_home_dir,UserSourceDir],'/',D1),
    absolute_file_name(D1,Dir),!.

%% kleio_user_structure_dir(?Dir,+Options) is det.
%
% Gets the base dir for structures using token information
% The Dir is:
% 
% Base Dir =  KLEIO_HOME / S from option(structures(S),Options) 
%
kleio_user_structure_dir(Dir,Options):-
    kleio_home_dir(Kleio_home_dir),
    option(structures(UserDir),Options),  
    atomic_list_concat([Kleio_home_dir,UserDir],'/',D1),
    absolute_file_name(D1,Dir),!.


%% kleio_resolve_source_file(?RelativePath,?AbsolutePath,+Options) is det.
%
% Resolves a relative path to a source to an absolute file name. 
% Can also be used to extract the relative path from the absolute file name.
% The absolute path is:
% 
% AbsolutePath =  KLEIO_HOME / S from option(sources(S),Options) / RelativePath
% TODO: could be generalized with kleio_resolve_file/3 taking into account the 
%       extension to determine file type and allowing for different dirs for diferent file types.
kleio_resolve_source_file('',AbsolutePath,Options):-
    kleio_home_dir(Kleio_home_dir),
    option(sources(UserSourceDir),Options),  
    atomic_list_concat([Kleio_home_dir,UserSourceDir],'/',D1),
    absolute_file_name(D1,AbsolutePath),!.

kleio_resolve_source_file(RelativePath,AbsolutePath,Options):-
    atomic(RelativePath),
    kleio_home_dir(Kleio_home_dir),
    option(sources(UserSourceDir),Options),  
    atomic_list_concat([Kleio_home_dir,UserSourceDir,RelativePath],'/',D1),
    absolute_file_name(D1,AbsolutePath),!.

kleio_resolve_source_file(RelativePath,AbsolutePath,Options):-
    atomic(AbsolutePath),
    kleio_home_dir(Kleio_home_dir),
    option(sources(UserSourceDir),Options),  
    atomic_list_concat([Kleio_home_dir,UserSourceDir,''],'/',BaseDir),
    absolute_file_name(BaseDir,BaseDir2),
    atom_concat(BaseDir2,RelativePath,AbsolutePath),!. 

%% kleio_resolve_source_list(?Relative,?Absolute,+Options) is det.
%
% Bidrectional mapping of a list of relative paths to absolute paths.
% 
% @see kleio_resolve_source_file for details on the conversion
%
kleio_resolve_source_list([],[],_):-!.
kleio_resolve_source_list(Relative,[A|MoreAbs],Options):-
    var(Relative),
    kleio_resolve_source_file(R,A,Options),
    kleio_resolve_source_list(MoreRel,MoreAbs,Options),
    Relative = [R|MoreRel].
kleio_resolve_source_list([R|MoreRel],Absolute,Options):-
    var(Absolute),
    kleio_resolve_source_file(R,A,Options),
    kleio_resolve_source_list(MoreRel,MoreAbs,Options),
    Absolute = [A|MoreAbs].

  

%% kleio_resolve_structure_file(?RelativePath,?AbsolutePath,+Options) is det.
%
% Resolves a relative path to a structure to an absolute file name. 
% Can also be used to extract the relative path from the absolute file name.
% The absolute path is:
%
% AbsolutePath =  KLEIO_HOME / S from option(structures(S),Options) / RelativePath
%
kleio_resolve_structure_file('',AbsolutePath,Options):-
    kleio_home_dir(Kleio_home_dir),
    option(structures(UserStruDir),Options),  
    atomic_list_concat([Kleio_home_dir,UserStruDir],'/',D1),
    absolute_file_name(D1,AbsolutePath),!.

kleio_resolve_structure_file(RelativePath,AbsolutePath,Options):-
    atomic(RelativePath),
    kleio_home_dir(Kleio_home_dir),
    option(structures(UserSourceDir),Options), 
    atomic_list_concat([Kleio_home_dir,UserSourceDir,RelativePath], '/' ,D1),
    absolute_file_name(D1,AbsolutePath),!.

kleio_resolve_structure_file(RelativePath,AbsolutePath,Options):-
    atom(AbsolutePath),
    kleio_home_dir(Kleio_home_dir),
    option(structures(UserSourceDir),Options), 
    atomic_list_concat([Kleio_home_dir,UserSourceDir,''],'/',BaseDir),
    absolute_file_name(BaseDir,BaseDir2),
    atom_concat(BaseDir2,RelativePath,AbsolutePath),!. 

%% kleio_mime_type(+FileName,-MimeType) is semidet.
%
% Determines the mime type of a kleio file.
%
kleio_mime_type(FileName,MimeType):-
    file_name_extension(_,Ext,FileName),
    member_check(Ext,[cli,org,old,rpt,err,xml,xerr,xrpt,str,json,ids,srpt]),
    (Ext = 'xml' -> MimeType=text/xml
        ;
    MimeType=text/Ext),!.
kleio_mime_type(FileName,text/cli):-
    file_name_extension(_,kleio,FileName),!.
kleio_mime_type(FileName,MimeType):- % TODO: why? these are files with no ext
    file_name_extension(Ext,'',FileName),
    member_check(Ext,[cli,org,old,rpt,err,xml,xerr,xrpt,str]),
    (Ext = 'xml' -> MimeType=text/xml
        ;
    MimeType=text/Ext),!.

