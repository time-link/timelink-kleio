:-module(apiCommon,[]).

/** <module> A REST / JSON_RPC 2.0 server for the kleio translator.

## Entities exposed by the server

  $ _sources_: kleio source files: they can be created, replaced and deleted
  $ _directories_: are containers of kleio source files, and can contain also directories. They can be created,deleted and their content listed.
  $ _structures_: kleio structure files (schema): they can be created, replaced and deleted
  $ _sdirectories_: containers of kleio structure files and can contain also sdirectories. They can be created,deleted and their content listed.
  $ _translations_: sources that were translated. They can be created by translating sources and the results of the translation can be retrived.
  $ _reports_:  reports on the translation process. They can contain errors, warnings and other human readable information. They result from the translation process, and can be retrieved.
  $ _exports_: are xml representations of the information contained in source files. They result from the translation process, and can be retrieved.
  $ _errors_: error summaries for translations. They result from the translation process, and can be retrieved.
  $ _originals_: original kleio files before the first translation. The translation process inserts explicit ids in the source file and keeps the original for reference and backup. This file is not deleted if the translation that generated it is deleted
  $ _changes_ : source files newer than the last translation if any. This is a subset of _sources_.
  $ _tokens_  : tokens, must be present in request to regulate permissions and file access.
  $ _users_   : users invalidate user related tokens
 
 Note that when a translation is deleted the corresponding entities in _reports_, _exports_ and _errors_
 are also deleted. Also that these derivative entities cannot be directly created, they result from creating
 translations. 

  * _sreports_: are reports on the processing of structure files. They can contain errors, warnings and other human readable information. 
 They result from the translation process, and can be retrieved.


## API funcions

The _function_ is the predicate that implements the request and also the method name to be used in JSONRPC requests.

| _entity_      | _HTTP Method_     | _function_       | _meaning_                   |
| sources       | GET               | sources_get      | retrieve the source. If path is a directory returns list of source files |
|               | POST (multipart)  | sources_upload   | upload a new source file  _Not available in JSON_RPC_|
|               | PUT  (multipart)  | sources_update   | update an existing source file  _Not available in JSON_RPC_|
|               | POST              | sources_copy     | copy an existing file to another location |
|               | PUT               | sources_move     | move and existing file to another location |
| tokens        | POST              | tokens_generate  | generate a token for a user |
|               | DELETE            | tokens_invalidate| invalidate a token |
| users         | DELETE            | tokens_invalidate| invalide a user token | 
| translations  | POST              | translations_translate | translate the file(s) in Path
|               | GET               | translations_get | get the translation result (kleio_set)
|               | DELETE            | translations_delete |clear the translation result (derived files)
|
 Map the combination of HTTPMethod and Entity into a specific operation

 == 
   /sources/path       GET (json:sources_get) if Path is a file returns the file (json:NOT IMPLEMENTED) if it is a directory returns list of sources
   /sources/path       POST,PUT (json:NOT_IMPLEMENTED) upload a file
   /sources/path       DELETE(json:sources_delete) delete source file
   /directories/path   GET (json:directories_get),list of files and directories under path
   /directories/path   POST,PUT (json:directories_create) create a directory
   /directories/path   DELETE (json:directories_delete) removes a directory
   /structures/path    GET se path é um ficheiro devolve o ficheiro, se é um directoria devolve lista
   /structures/path    POST, PUT upload de um ficheiro de strutura.
   /structures/path    DELETE eliminar um ficheiro ficheiro de estrutura
   /sdirectories/path  GET list of structure files and directories under path
   /sdirectories/path  POST,PUT create a directory
   /sdirectories/path  DELETE removes a directory
   /translations/path  POST,PUT start a translation if path a directory translate all directory
   /translations/path  GET the result of the translation (kleioset)
   /translations/path  DELETE delete translation results, if path a directory delete all translations
   /reports/path       GET the translation report in path, if path a directory list all the reports in the dir
   /sreports/path      GET the structure processing report in path, if path a directory list all the reports in the dir
   /exports/path       GET the xml export file of the path (extension is ignored) if path dir returns list
   /errors/path        GET the error sumaary
   /originals/path     GET the original clio file (before the first translation)
   /additions/path     GET list of files that need to be translated. 
   /status/path        GEt the status of file(s) at path status can be VTEW and aditionally PQ (currently sources)
   /processes/path     GET lista de ficheiros em tradução
   /queued/path        GET lista de ficheiros em fila de espera para serem traduzidos.
   /versions           GET status do repositório
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
 
