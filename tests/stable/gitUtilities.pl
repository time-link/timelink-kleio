:-module(gitUtilities,[
    git_global_status/3,
    git_ahead_behind/5,
    find_git_directory/3,
    print_git_global_status/1,
    git_remotes_branches_info/3,
    git_fetch/2
    ]).
/** <module> Utilities dealing with GIT repositiories
 * 
 * These utilties allow for a minimalistic management of a git repository,
 * including keeping synced with remote origin.
* 
*/      


%% git_global_status(+Dir,-GlobalStatus,+Options) is det.
%
% Gets an overview of the global status of the local branch, including 
% divergence from origin and local change. The information is designed to
% to provide an overview of what is going on in the remote and compare with local
% using logs and diff list.
%
% Options:
%
% $ compare_to(Branch): compare with origin/Branch instead of origin/CURRENTBRANCH
%
git_global_status(CurrentDir,GlobalStatus,Options):-
    (find_git_directory(CurrentDir,Dir,Options)->true;throw(error(not_inside_git_repository))),
    git_default_branch(CurrentBranch,[directory(Dir)]),
    git_remotes(Dir,Remotes,Options),
    git_ahead_behind(Dir,Ahead,Behind,AheadBehindOf,Options),
    git_origin_new_log(Dir,OrgLog,Options),
    git_local_new_log(Dir,LocLog,Options),
    git_diff_origin_to_local(Dir,OrgDiff,Options),
    git_diff_local_to_origin(Dir,LocDiff,Options),
    git_status(CurrentDir,DirStatus,Options),
    GlobalStatus = gs{directory:CurrentDir,git_root:Dir,branch:CurrentBranch,remotes:Remotes,ahead:Ahead,behind:Behind,comparing_to:AheadBehindOf,origin_new_logs:OrgLog,local_new_logs:LocLog,
    origin_changes:OrgDiff, local_changes:LocDiff, work_dir_status:DirStatus},
    !.

%% print_git_global_status(+GobalStatus) is det.
%
% Prints the output of git_global_status is a readable format.
%
print_git_global_status(GS):-
    get_time(Now),
    format_time(string(NowString),'%Y-%m-%d %H:%M:%S',Now),
    format('Repository status: comparing "~w" with "~w" local time: ~w~nCurrent dir: ~w. Git root: ~w ~n',[GS.branch,GS.comparing_to,NowString,GS.directory,GS.git_root]),
    format('Current branch "~w" is ~w behind and ~w ahead of "~w"~n~n',[GS.branch,GS.behind,GS.ahead,GS.comparing_to]),
    format('Remotes:\n'),
    forall(member(Remote,GS.remotes),format('  ~w ~w ~w~n',[Remote.name,Remote.url,Remote.direction])),
    (GS.behind > 0 -> format('~nNEXT PULL~n');format('~nNOTHING TO PULL~n')),
    (GS.origin_new_logs \= [] ->
        (   format('~n Changes in "~w" not in "~w" (need pull):~n~n',[GS.comparing_to,GS.branch]),
            forall(member(Log,GS.origin_new_logs),format('    ~w (~w)~n    ~w~n~n~@~n',[Log.comment,Log.author,Log.author_date,print_files_in_commit(GS.directory,Log.hash,[])]))
        )
        ;
        true),
    % (GS.origin_changes \= [] ->
    %     (   format('~n Files affected by next pull from ~w (A)dded, (D)eleted, (M)odified:~n~n',[GS.comparing_to]),
    %         forall(member(File,GS.origin_changes),format(' ~w ~w~n',[File.status,File.name]))
    %     )
    %     ;
    %     true),
    (GS.ahead >0 -> format('~nNEXT PUSH~n');format('~nNOTHING TO PUSH~n')),
    (GS.local_new_logs \= [] ->
            (   format('~n Local changes not in "~w" (need push):~n~n',[GS.comparing_to]),
                forall(member(Log2,GS.local_new_logs),
                    format('    ~w (~w) ~n    ~w~n~n~@~n',[Log2.comment,Log2.author,Log2.author_date,print_files_in_commit(GS.directory,Log2.hash,[])]))
            )
            ;
            true),
    % (GS.local_changes \= [] ->
    %         (   format('~n Files affected by next push to ~w (A)dded, (D)eleted, (M)odified:~n~n',[GS.comparing_to]),
    %             forall(member(File2,GS.local_changes),format('   ~w ~w~n',[File2.status,File2.name]))
    %         )
    %         ;
    %         true),

    (GS.work_dir_status \= [] ->
            (   format('~n Files changed in "~w" but not commited (need commit before push):~n~n',[GS.branch]),
                forall(member(File3,GS.work_dir_status),format('  ~w ~w ~w~n',[File3.index,File3.working,File3.name]))
            )
            ;
            true),
    nl,nl,!.

%% print_files_in_commit(+Directory,+Commit,+Options) is det.
%
% Prints the list of files associated with a hash.
%
print_files_in_commit(Directory,Commit,Options):-
    git_list_commit_files(Directory,Commit,Files,Options),
    forall(member(F,Files),format('       ~w ~w~n',[F.status,F.name])).

%% git_ahead_behind(+Directory,-Ahead,-Behind,-Comparing_to,+Options) is det.
%
% Compares current branch to another branch and gives commits ahead and behind.
% The target branch to compare can be specified by the option `compare_to_branch`.
% It defaults to the origin/CURRENT_BRANCH
%
% Does not fetch first. See git_fetch/2
% Uses the following command:
%   
%   git rev-lis6t --left-right --count CURRENT_BRANCH...COMPARE_TO_BRANCH
% 
% Options:
% $ compare_to(Branch): compare with Branch instead of origin/CURRENTBRANCH
%
git_ahead_behind(CurrentDir,Ahead,Behind,OtherBranch,Options):-
    (find_git_directory(CurrentDir,Directory,Options)->true;throw(error(not_inside_git_repository))),
    git_default_branch(CurrentBranch,[directory(Directory)]),
    atomic_list_concat(['origin/',CurrentBranch], OriginBranch),
    option(compare_to_branch(OtherBranch),Options,OriginBranch),
    atomic_list_concat([CurrentBranch,'...',OtherBranch],Compare),
    git(['rev-list','--left-right','--count',Compare],[directory(Directory),output(O)]),
    split_string(O,"\t ","\t\n  ",L),
    [Ours,Theirs] = L,
    number_string(Ahead,Ours),
    number_string(Behind,Theirs),!.

%% git_fetch(+Directory,+Options) is det.
%
% Fetchs state from origin
%
% Corresponds to the command:
%
%    git fetch
%
% Options: no options currently defined
% TODO: devia devolver o current time
%
git_fetch(Directory,Options):-
    option(git_params(MoreParams),Options,[]),
    git(['fetch'|MoreParams],[directory(Directory),output(Output)]),
    (option(lines(Lines),Options)->split_string(Output,"\n","\n",Lines);true),!.
    
%% git_origin_new_log(+Directory,-ResultLogs,+Options) is det.
%
% Fetch the logs of commits in a remote that are not in the current local branch
% These commits correspond to the "behind" returned by git_ahead_behind/5.
% 
% Options:
% $ compare_to : remote branch to compare to. Defaults to origin/CurrentBranch
% $ include_files: yes/no include de files for each commit. Defaults to no.
%
%
git_origin_new_log(Directory,ResultLogs,Options):-
    git_default_branch(CurrentBranch,[directory(Directory)]),
    atomic_list_concat(['origin/',CurrentBranch], OriginBranch),
    option(compare_to_branch(OtherBranch),Options,OriginBranch),
    atomic_list_concat([CurrentBranch,'..',OtherBranch],Revs),
    git_shortlog(Directory,SL,[revisions(Revs)]),
    (bagof(git_log{hash:Hash,author:User,author_date:When,commit:User2,commit_date:When2,what:What,comment:Comment,head:SomeList,files:[]},
            L^(member(L,SL), L = git_log(Hash,User,When,User2,When2,What,Comment,SomeList)),
            Logs)
    ;
    Logs=[]),
    (option(include_files(yes),Options) ->
        attach_files_to_log(Directory,Logs,ResultLogs,Options)
    ;
        ResultLogs = Logs
    ),!.

%% git_local_new_log(+Directory,-ResultLogs,+Options) is det.
%
% Fetch the logs of local commits that are not in another(usually remote) branch
% These commits correspond to the "ahead" returned by git_ahead_behind/5.
% 
% Options:
% $ compare_to : remote branch to compare to. Defaults to origin/CurrentBranch
% $ include_files: yes/no include de files for each commit. Defaults to no.
%
git_local_new_log(Directory,ResultLogs,Options):-
    git_default_branch(CurrentBranch,[directory(Directory)]),
    atomic_list_concat(['origin/',CurrentBranch], OriginBranch),
    option(compare_to_branch(OtherBranch),Options,OriginBranch),
    atomic_list_concat([OtherBranch,'..',CurrentBranch],Revs),
    git_shortlog(Directory,SL,[revisions(Revs)]),
    (bagof(git_log{hash:Hash,author:User,author_date:When,commit:User2,commit_date:When2,what:What,comment:Comment,head:SomeList,files:[]},
            L^(member(L,SL), L = git_log(Hash,User,When,User2,When2,What,Comment,SomeList)),
            Logs)
    ;
    Logs=[]),
    (option(include_files(yes),Options) ->
        attach_files_to_log(Directory,Logs,ResultLogs,Options)
    ;
        ResultLogs = Logs
    ),!.

%% attach_files_to_log(+Dir,+Logs,-LogsWithFiles) is det.
%
% Logs is a list of log entries. For each entry in Logs
% the files associated with the commit are added with the key "files"
%
attach_files_to_log(Directory,[Log|Logs],[LogF|LogsF],Options):-
    git_list_commit_files(Directory,Log.hash,Files,Options),
    LogF = Log.put('files',Files),
    attach_files_to_log(Directory,Logs,LogsF,Options).
attach_files_to_log(_,[],[],_).

%% git_diff_origin_to_local(+Directory,-Changes,+Options) is det.
%
% Changes is the list of files changed in other branch not present
% in the current one. Normally used to know changes at the remote 
% that will be incomporated in the next pull.
% Target branch can be specified by the compare_to_branch Option.
%
% Options:
% $ compare_to : remote branch to compare to. Defaults to origin/CurrentBranch
% 
git_diff_origin_to_local(Directory,Changes,Options):-    
    git_default_branch(CurrentBranch,[directory(Directory)]),
    atomic_list_concat(['origin/',CurrentBranch], OriginBranch),
    option(compare_to_branch(OtherBranch),Options,OriginBranch),
    atomic_list_concat(['...',OtherBranch],Revs),
    git([diff,'--name-status',Revs],[directory(Directory),output(O)]),
    split_string(O,"\n","\n",Lines),
    (bagof(file{status:S,name:F},L^(member(L,Lines),split_diff_status(L,S,F)),Changes);Changes=[]),!.

%% git_diff_local_to_origin(+Directory,-Changes,+Options) is det.
%
% Changes is the list of files in the current branch not present in another.
% branch. Normally used to know changes that will be in the next push to remote.
% Target branch can be specified by the compare_to_branch Option.
%
% Options:
% $ compare_to : remote branch to compare to. Defaults to origin/CurrentBranch
% 
git_diff_local_to_origin(Directory,Changes,Options):-
    git_default_branch(CurrentBranch,[directory(Directory)]),
    atomic_list_concat(['origin/',CurrentBranch], OriginBranch),
    option(compare_to_branch(OtherBranch),Options,OriginBranch),
    atomic_list_concat([OtherBranch,'...'],Revs),
    git([diff,'--name-status',Revs],[directory(Directory),output(O)]),
    split_string(O,"\n","\n",Lines),
    (bagof(file{status:S,name:F},L^(member(L,Lines),split_diff_status(L,S,F)),Changes);Changes=[]),!.


git_status(Directory,Results,Options):-
    option(git_params(P),Options,[]),
    git([status,'--porcelain',Directory|P],[directory(Directory),output(O)]),
    split_string(O,"\n","\n",Lines),
    (bagof(file{index:I,working:W,name:F},L^(member(L,Lines),split_status(L,I,W,F)),Changes);Changes=[]),
    find_first_line_only_changes(Directory,Files,Options),
    mark_no_changes_files(Changes,Files,Results),!.

mark_no_changes_files([],_,[]):-!.
mark_no_changes_files([Change|Changes],Files,[Marked|MoreChanges]):-
    member(Change.name,Files),!,
    Marked = Change.put('first_line_only',"yes"),
    mark_no_changes_files(Changes,Files,MoreChanges).
mark_no_changes_files([Change|Changes],Files,[Marked|MoreChanges]):-
    Marked = Change.put('first_line_only',"no"),
    mark_no_changes_files(Changes,Files,MoreChanges),!.

git_current_branch(D,CurrentBranch):-
    git_default_branch(CurrentBranch,[directory(D)]),!.

git_origin_url(Directory,URL,_):-
    git_remote_url(origin,URL,[directory(Directory)]),
    !.

git_remotes(Directory,Remotes,_):-
    git([remote,'-v'],[directory(Directory),output(O)]),
    split_string(O,"\n","\n",Lines),
    (bagof(remote{name:N,url:F,direction:D},R^(member(R,Lines),split_remote(R,N,F,D)),Remotes);Remotes=[]),!.


git_list_commit_files(Directory,CommitHash,Files,_):-
    git(['diff-tree','--no-commit-id', '--name-status','-r',CommitHash],[directory(Directory),output(O)]),
    split_string(O,"\n","\n",Lines),
    (bagof(file{status:S,name:F},L^(member(L,Lines),split_diff_status(L,S,F)),Files);Files=[]),!.

%% find_first_line_only_changes(+Directory,-FLChanges,+Options) is det.
%
% No change translations are files in the working dir that differ only in the first
% line from the last commited version. Such files are detected as modifications
% by git but in fact correspond to files unchanged since the last translation.
% They are produced "normally" by the translator
% because it always updates the first line with the "translation=n" element.
% This behaivour is scheduled to be changed, meanwhile this predicate helps to mitigate.
%
% The files found with this predicate can then be reverted to the commit version
% with git checkout -- FILE.cli; touch -r FILE.err FILE.cli (to keep the change date) 
%  to avoid unecessary clutter of repository logs.
%
% see git_revert_first_line_changes/3.
%
find_first_line_only_changes(Directory,Files,_):-
    git([diff,'--numstat',
        '--minimal',
        '--diff-filter=M',
        '-G',
        'translations',
        '.'],
        [directory(Directory),output(O)]),
        split_string(O,"\n","\n",Lines),
        (bagof(File,
            Line^File^(member(Line,Lines),split_diff_numstat(Line,"1","1",File)),Files);Files=[]),!.


split_diff_status(Line,Status,File):-
    sub_string(Line,0,1,_,Status),sub_string(Line,2,_,0,File),!.

split_diff_numstat(Line,LeftCount,RightCount,File):-
    split_string(Line,"\t","",[LeftCount,RightCount,File]),!.

split_status(Line,IStatus,WStatus,File):-
    sub_string(Line,0,1,_,IStatus),
    sub_string(Line,1,1,_,WStatus),
    sub_string(Line,3,_,0,File),!.

split_remote(Remote,Name,URL,Direction):-
    split_string(Remote,"\t","",[Name,Rest]),
    split_string(Rest," "," ",[URL,Direction]),
    !.

%% find_git_directory(+StartDir,-GitRoot,+Options) is det.
%
%  Finds the root of the repository of a given dir. Fails if StartDir
%  is not inside a Git repository.
%  GitRoot is the base directory of the Git repository of StartDir.
%  This is usefull because SWI-Prolog git commands required a parameter
%  with the git root and are unable to find it by themselves.
%
%  Uses bultin predicate is_git_directory/1.
%
%  No Options are currently defined.
%
find_git_directory(StartDir,StartDir,_):-
    is_git_directory(StartDir),!. 
find_git_directory('/',_,_):-!,fail.
find_git_directory(StartDir,GitRoot,Options):-
    relative_file_name(Up,StartDir, './'),
    find_git_directory(Up,GitRoot,Options),!.
   

%% git_remotes_branches_info(+Directory,-Branches,+Options) is det.
%
%  Gets information on the branches of the remotes associated to the repository of Directory.
%  Works with information of the last fetch.
%  For each remote branch gathers the following information:
%  Name, commits ahead of current branch, commits behind, info on remote (name, origin, url).
%
% Returns a list of dictionnaries, each describing a remote branch, as follows:
%
% [ remote_branch{ ahead:12,
%                  behind:0,
%                  current:master,
%                  name:'origin/editor-ana',
%                  remote:remote{ direction:"(fetch)",
%                                 name:"origin",
%                                 url:"https://timelink-sources.visualstudio.com/Kleio-tests/_git/Kleio-test-sources"
%                               }
%                },
%   remote_branch{ ahead:12,
%                  behind:1,
%                  current:master,
%                  name:'origin/editor-joaquim',
%                  remote:remote{ direction:"(fetch)",
%                                 name:"origin",
%                                 url:"https://timelink-sources.visualstudio.com/Kleio-tests/_git/Kleio-test-sources"
%                               }
%                },...]
%
% Options: currently none:
%
git_remotes_branches_info(D,Branches,Options):-
    find_git_directory(D,GitRoot,Options),
    git_remotes(GitRoot,Remotes,[]), 
    (setof(B,B^(member(R,Remotes),get_remote_branches_info(GitRoot,R,B)),Bs); Bs=[]),
    flatten(Bs,Branches),!.
    
get_remote_branches_info(D,Remote,Branches):-
    git_remote_branches(Remote.url,RBranches),
    bagof(Result,Branch^(member(Branch,RBranches),get_remote_branch_info(D,Remote,Branch,Result)),Branches),
    !.

get_remote_branch_info(D,Remote,Branch,Result):-
    atomic_list_concat([Remote.name,"/",Branch],Target),
    git_current_branch(D,CurrentBranch),
    git_ahead_behind(D,A,B,_,[compare_to_branch(Target)]),
    Result=remote_branch{remote:Remote,name:Target,ahead:A,behind:B,current:CurrentBranch},!.

git_set_remote_url(D,Name,URL,OS,ES):-
    git([remote, 'set-url', Name,URL],[directory(D),output(O),error(E)]),
    string_codes(OS,O),
    string_codes(ES,E).

git_test_remote(__D,__Name):-!.
    
