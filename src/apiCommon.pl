:-module(apiCommon,[]).

/** <module> A REST / JSON_RPC 2.0 server for the kleio translator.

## Entities exposed by the server

  $ _sources_: kleio source files: they can be created, replaced and deleted
  $ _directories_: are containers of kleio source files, and can contain also directories. They can be created,deleted and their content listed.
  $ _structures_: kleio structure files (schema): they can be retrieved. Supports resolving structures associated with kleio source files.
  $ _translations_: sources that were translated. They can be created by translating sources and the results of the translation can be retrived.
  $ _reports_:  reports on the translation process. They can contain errors, warnings and other human readable information. They result from the translation process, and can be retrieved.
  $ _exports_: are xml representations of the information contained in source files. They result from the translation process, and can be retrieved.
  $ _identifications_: identification files (mhk_identification*.json). They can be retrieved.
  $ _tokens_  : tokens, must be present in request to regulate permissions and file access.
  $ _users_   : users invalidate user related tokens

 Note that when a translation is deleted the corresponding entities in _reports_, _exports_ and _errors_
 are also deleted. Also that these derivative entities cannot be directly created, they result from creating
 translations.


## API functions

The _function_ is the predicate that implements the request and also the method name to be used in JSONRPC requests.

    |---------------|-------------------|------------------|----------------------------|
    | _entity_       | _HTTP Method_     | _json_rpc_       | _meaning_                   |
    | sources        | GET               | sources_get      | retrieve the source. If path is a directory returns list of source files |
    |                | POST (multipart)  | sources_upload   | upload a new source file  _Not available in JSON_RPC_|
    |                | PUT  (multipart)  | sources_update   | update an existing source file  _Not available in JSON_RPC_|
    |                | POST              | sources_copy     | copy an existing file to another location |
    |                | PUT               | sources_move     | move an existing file to another location |
    |                | DELETE            | sources_delete   | delete source file or directory |
    | directories    | GET               | directories_get  | list directories under path (recurse=yes for subdirectories) |
    |                | POST              | directories_create | create a directory |
    |                | POST (origin)     | directories_copy | copy directory from origin to path |
    |                | DELETE            | directories_delete | remove a directory |
    | structures     | GET               | structures_get   | retrieve structure file info. Use kleio=PATH param to resolve structure for a kleio file |
    | translations   | POST              | translations_translate | translate the file(s) in Path |
    |                | GET               | translations_get | get the translation result (kleio_set) |
    |                | DELETE            | translations_delete | clear the translation result (derived files) |
    | exports        | GET               | exports_get      | retrieve the xml export file |
    | reports        | GET               | reports_get      | retrieve the translation report file |
    | identifications| GET               | identifications_get | retrieve identification files (mhk_identification*.json) |
    | versions       | GET               | versions_get_remotes_branches | list branches in remote repository |
    |                | GET               | versions_get_global_status | global status of repository |
    |                | GET               | versions_get_user_info | name and email of git user |
    |                | GET               | versions_pull    | pull from remote repository |
    |                | PUT               | versions_push    | push to remote repository |
    |                | PUT               | versions_commit  | commit to local repository |
    |                | PUT               | versions_set_user_info | set git user name and email |
    |                | DELETE            | versions_reset   | reset local repository to commit_ref |
    | tokens         | POST              | tokens_generate  | generate a token for a user |
    |                | DELETE            | tokens_invalidate | invalidate a token |
    | users          | DELETE            | users_invalidate | invalidate all tokens for a user |
    | client_log     | POST              | client_log_send  | send debug message to server logs |

Map the combination of HTTPMethod and Entity into a specific operation

 ==
   /sources/path       GET (json:sources_get) if Path is a file returns the file, if it is a directory returns list of sources
   /sources/path       POST multipart (json:NOT_IMPLEMENTED) upload a source file to "path"
   /sources/path       POST with param origin=path2 copy source file from "path2" to path
   /sources/path       PUT multipart (json:NOT_IMPLEMENTED) update source file in path
   /sources/path       PUT with origin=path2 (json:NOT_IMPLEMENTED) move source file from path2 to path
   /sources/path       DELETE (json:sources_delete) delete source file or directory
   /directories/path   GET (json:directories_get) list directories under path recurse=yes returns all subdirectories
   /directories/path   POST (json:directories_create) create a directory
   /directories/path   POST with origin=path2 (json:directories_copy) copy directory from path2 to path
   /directories/path   DELETE (json:directories_delete) removes a directory
   /structures/path    GET (json:structures_get) retrieve structure file. Use ?kleio=KLEIO_PATH to resolve structure for a kleio file
   /translations/path  POST,PUT (json:translations_translate) start a translation, if path is a directory translate all directory
   /translations/path  GET (json:translations_get) get the result of the translation (kleioset)
   /translations/path  DELETE (json:translations_delete) delete translation results, if path a directory delete all translations
   /exports/path       GET (json:exports_get) the xml export file of the path (extension is ignored) if path dir returns list
   /reports/path       GET (json:reports_get) the translation report in path, if path a directory list all the reports in the dir
   /identifications/path GET (json:identifications_get) identification files (mhk_identification*.json)
   /versions/remotes/branches/path GET (json:versions_get_remotes_branches) list of branches in the remote repository
   /versions/status/global GET (json:versions_get_global_status) global status of the repository
   /versions/user-info/ GET (json:versions_get_user_info) name and email of the user
   /versions/pull/path  GET (json:versions_pull) pull from remote repository
   /versions/push       PUT (json:versions_push) push to remote repository
   /versions/commit     PUT (json:versions_commit) commit to local repository
   /versions/set-user-info PUT (json:versions_set_user_info) with &user_name and &user_email set name and email of the user
   /versions/reset      DELETE (json:versions_reset) reset local repository to &commit_ref
   /client_log          POST (json:client_log_send) send debug message to server logs
 ==

**/
:-reexport('apiSources').
:-reexport('apiTokens').
:-reexport('apiDirectories').
:-reexport('apiTranslations').
:-reexport('apiExports').
:-reexport('apiReports').
:-reexport('apiGit').
:-reexport('apiIdentifications').
:-reexport('apiLog').
:-reexport('apiStructures').

