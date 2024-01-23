# Linked data

From version 12.0.540 the Kleio translator supports linked data notation.

Currently linked data can be used to link the value of an attribute (e.g. a geographical name)
to an external source of information (e.g. wikidata).

## How to declare and use linked data

### 1. Declare the external source, with an attribute of the `kleio$` group

In the `kleio$` group, declare the external source with a `link$` attribute.
The attribute value should be a url pattern containing a place holder ($1) for a specific id of the data item to be linked.

Example:

        kleio$...
            link$wikidata/"http://wikidata.org/wiki/$1"

The format of the link$ group is:

    link$short-name/"url-pattern"

where `short-name` is a short name for an external source and
`url-pattern` should be a url containing a place
holder ($1) for a specific id of the data item to be linked.

Note that quotes around the url-pattern are required.

### 2. Anotate element values with external ids

Annotate element values with external ids, using the `@` notation.

    ls$jesuita-entrada/Goa, Índia# @wikidata:Q1171/15791200

The format of an external link comment annotation is:

    # @short-name:id 

On translation annotations such as 

        ls$jesuita-entrada/Goa, Índia# @wikidata:Q1171/15791200

will generate an aditional attribute:


        ls$ jesuita-entrada@/https://www.wikidata.org/wiki/Q1171/15791200/obs=%Goa, Índia


The comment can contain other information before or after the linked data annotation.:

    ls$morte/Batávia# @wikidata:Q1199713 (Jakarta), Indonésia em viagem/17250000


See discussion in https://github.com/time-link/timelink-kleio/issues/6