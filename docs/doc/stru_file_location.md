# Location of str (structure) schema files

`str` files contain the definitions that
allow the translation of `kleio` source files.


It is important that diferent source collections can use different stru definitions.

One of the current problems is that for every new source format the shared `gacto2.str` has to be changed.

By allowing the stru file to be stored close to the sources directory it is possible to have different `stru` in usage.

### implementation

The standard source repository has a `sources` directory at the same level of a `structures` directory.

So for a given source file to be translated with path `sources/SUBPATH/file.cli` the default stru would be, in order:

* `structures/SUBPATH/file.str`  
* `structures/SUBPATH2/gacto2.str`
 where `SUBPATH2` is a parent path of SUBPATH
* `structures/DIRNAME.str`  where DIRNAME, is the name of a dir in SUBPATH
* `structures/gacto2.str` can be replaced by `sources.str`  for clarity in all patterns.

This allows for a source repository to have formats specific to a single file, a sub directory of files or to all the sources in the repository:

*  `structures/baptisms/baptism-index.str` format specific for `sources/baptisms/baptism-index.cli` 
* `structures/baptisms/gacto2.str` or `structures/baptisms/sources.str` for all sources inside `sources/baptisms` and sub directories
* `structures/baptisms.str`  for all sources in `sources/baptisms` (not sure about this one, the advantage is to manage more easily different structure files with different names)
* `structures/gacto2.str` or `structures/sources.str` default str for all sources.

---
This was based in issue 7 https://github.com/time-link/timelink-kleio/issues/7
