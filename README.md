# Kleio translation services for Timelink.

This API provides access to translation services of _Kleio_ files. 

See the API documentation at [docs/api](docs/api/index.html)

## About `Kleio` files

_Kleio_ files are text files with a special notation designed to the transcription of historical sources. The notation was created by Manfred Thaller, as part of the _Kleio historical database system_ (http://web.archive.org/web/20130603204750/http://www.hki.uni-koeln.de/kleio/old.website/).

Although Thaller's Kleio database system is no longer publically available, the notation developped for historical source transcritpion proved very powerfull, providing a concise way for the transcription of complex historical documents.

The _Timelink-Kleio translator_ implements a subset of the Kleio notation designed for the Timelink database system. _Timelink_ provides a set of data models designed for handling person-oriented information collected in historical documents.


This is how a baptism looks like in _Timelink_ keio notation:

      bap$b1714-2/12/11714/fl.117v./igreja de sao silvestre/manuel lopes serra (padre)

         celebrante$manuel lopes serra
            ls$profissao/padre

         n$francisca/f

            pn$antonio ferreira
               ls$morada/espinheiro
               ls$freguesia/lousa

            mn$leonarda francisca

            pad$ joao fernandes ramalheiro
               ls$morada/moita
               ls$freguesia/lousa

            mad$francisca
               ls$ec/solteira

               pmad$joao goncalves
                  ls$morada/espinheiro
                  ls$freguesia/lousa

Text files implementing the Timelink Kleio notation can be translated by
the `timelink-kleio` server and imported into the Timelink relational database. 

Translation is _intelligent_ in the sense that it operates a _normalization_ of the source information, infering information from the context, and so greatly reducing the overhead of producing normalized data. 

Timelink database-services then implement a set of functions that allow the identification of people, reconstruction of biographies, inference of personal networks, and other funcionalities.

For more information on Timelink see: [XXXXXX]


## Services provided by this API

The API is designed to decouple Kleio source handling from other software components. It provides funcionality to translate source files, inspect results of translation for errors or warning, obtain the generated data in XML format. It also allows basic file management (upload, download, delete, directory management, moving files and directories).

Main services are:

* translations: translates kleio source files.
* sources: lists available sources for translation.
* file management services: including downloading, uploading and deleting files (sources and structures), creating,copying, moving and deleting directories.
* permission managment using tokens: generate_token, invalidate_token,invalidate_use: magament of 'authorization' with token.

See the API documentation at [docs/api](docs/api/index.html)