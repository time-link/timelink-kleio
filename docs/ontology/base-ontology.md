# Base Ontology

The base ontology is defined in [`elements.yaml`](../../tests/kleio-home/structures/elements.yaml)
and [`groups.yaml`](../../tests/kleio-home/structures/groups.yaml). It is
**language-independent**: the Portuguese, or any other natural-language,
vocabulary is built on top of it by *extension* (see
[Portuguese vocabulary](portuguese-vocabulary.md)).

---

## Elements

Elements are the atomic data fields. Defining new elements by *specializing*
the ones below (with `source:`) triggers the correct processing and database
mapping behaviours.

### Abstract data types

These are not used directly; they serve as the root types for concrete
elements.

| Element | Description |
| --- | --- |
| `number` | Any number. |
| `string64` | 64-character strings, used for ids and references. |
| `string256` | Longer strings, like names or short descriptions. |
| `text` | Long texts, like descriptions, summaries, etc. |
| `control` | Elements that affect file processing (used in `kleio` and `source`). |
| `json` | JSON data, used internally. |

### Dates

| Element | Source | Description |
| --- | --- | --- |
| `day` | `number` | Two-digit day. |
| `month` | `number` | Two-digit month. |
| `year` | `number` | Four-digit year. |
| `date` | — | Date in `YYYYMMDD` or `YYYY-MM-DD`; allows ranges (`2021-01-01:2021-01-31`) and relative dates (`>2021-01-01`). |
| `date_extra_info` | `json` | Used internally to represent complex dates. |

### Identification and references

| Element | Source | Description |
| --- | --- | --- |
| `id` | `string64` | The id of the entity, used to link entities in the database. |
| `same_as` | `id` | Links occurrences of the same entity *in the same file*. |
| `xsame_as` | `id` | Links occurrences of the same entity *across files*. |
| `entity` | `id` | Reference to an entity in another entity. |
| `origin` | `id` | Id of the origin of a relation. |
| `destination` | `id` | Id of the destination of a relation. |
| `inside` | `id` | Id of the entity that contains this entity (automatic). |

### Standard fields

| Element | Source | Description |
| --- | --- | --- |
| `type` | `string256` | The type of an attribute, relation, act or entity. |
| `value` | `text` | The value of an attribute or relation. |
| `class` | `string64` | The class of an entity (e.g. person, object). |
| `loc` | `string256` | Location (place) of an entity or act. |
| `name` | `string256` | Name, normally of a person. |
| `description` | `string256` | Similar to name, but for objects or events. |
| `destname` | `string256` | Name of the destination in a relation. |
| `sname` | `name` | Standard/canonical name of an entity. |
| `sex` | `string64` | Gender of a person. |

### Long texts

| Element | Source | Description |
| --- | --- | --- |
| `obs` | `text` | Observations, comments, notes. Use triple quotes for multi-line. |
| `summary` | `text` | Summary of a document. |

### Provenance / sourcing

| Element | Source | Description |
| --- | --- | --- |
| `ref` | `string256` | Call number or reference to a document in an archive/library. |
| `page` | `string64` | Page in a document. |
| `pages` | `string64` | Page range. |
| `title` | `text` | Title of a work (e.g. a book). |

### Processing control

| Element | Source | Description |
| --- | --- | --- |
| `replaces` | `id` | Register id of an entity replaced by this one. |
| `replace`, `subs` | `replaces` | Aliases of `replaces`. |
| `autorels` | `control` | Mode of generation of automatic relations (currently unused). |
| `prefix` | `control` | Prefix all ids in the file (namespace concept). |
| `structure` | `control` | Path to the structure (schema) to be used. |
| `translations` | `control` | Number of times this file was translated. |
| `translator` | `control` | Name of the translator module. |
| `urlpattern` | `string256` | URL pattern for linked data, with `$1` for the linked id. |
| `shortname` | — | Short name for a linked-data URL, used in the `link` group. |

### Automatic / bookkeeping elements

| Element | Source | Description |
| --- | --- | --- |
| `groupname` | `string64` | Name of the Kleio group used to register the entity (automatic). |
| `level` | `number` | Nesting level of the entity in the source text. |
| `line` | `number` | Line in the source text where the entity is registered. |
| `kleiofile` | `string256` | Path of the file where the entity is registered. |

### Authority-register elements

| Element | Type | Description |
| --- | --- | --- |
| `atype` | `string256` | Act type. |
| `dbase` | `string256` | Database. |
| `func` | `string256` | Function. |
| `mode` | `string256` | Mode. |
| `occurrence` | `id` | Occurrence reference. |
| `status` | `string256` | Status. |
| `user` | `string256` | User. |

---

## Groups — overview

Groups are structured records. Each group may inherit from another via
`source:`, may declare positional/optional elements, and may *contain*
other groups. The descriptions below reproduce the `description` keys of
the YAML definitions.

The hierarchy has four top-level containers, declared directly under the
`kleio` root group:

```
kleio
├── historical-source     (records of acts/events: parish books, notarial, …)
├── authority-register    (record-linking registers; only "identifications" exists)
├── link                  (shortcuts to external / linked data)
└── property              (parser-level file properties)
```

### `kleio` — file root

> This is the top level group of Kleio files. It serves as a container for
> historical sources, authority registers, and other processing related
> groups. It has no representation in the database (person oriented model).

- **contains**: `historical-source`, `authority-register`, `link`, `property`
- **also**: `structure`, `translator`, `autorels`, `obs`, `prefix`, `translations`

---

## Hierarchy level 1 — the four containers

### `historical-source`

> Main group to register historical sources. A historical source contains
> the records of historical acts or events that happened in the past. They
> can also contain lists of people, objects, geoentities, abstractions, etc.
> Specific types of sources are defined by extending this group. Each
> source has a unique id (compulsory), recommended to match the file name
> without the extension.

- **source**: `entity`
- **position**: `[id, year, type, loc, ref]`
- **guaranteed**: `[id]`
- **contains**: `historical-act`, `event`, `text`
- **also**: `date`, `year`, `obs`, `replace`, `kleiofile`

`source` is an alias of `historical-source`.

### `authority-register` (abstract)

> A container for authority records. Authority records in Timelink are
> similar to authority records in library systems — they register real
> entities (people, objects) and link them. This is an abstract group,
> intended to be extended.

- **contains**: `authority-record`
- **position**: `[id, name]`
- **also**: `date`, `user`, `dbase`

The only concrete authority register is **`identifications`**, used for
record linking — it aggregates occurrences in sources pertaining to the
same person/entity.

- **source**: `authority-register`
- **contains**: `rentity`, `rperson`, `robject`

### `link`

> Defines a shortcut to insert links to external data as comments in the
> kleio file. A link is recorded as `@shortcut:external-id`, e.g.
> `link$wikidata/"https://www.wikidata.org/wiki/$1"` then
> `ls$place-of-stay/Canton#@wikidata:Q16572`.

- **position**: `[shortname, urlpattern]`

### `property`

> A property of the file for the kleio parser. Properties are not stored
> in the database; they affect parsing. Currently only
> `multiple-entry-flag` is defined, e.g.
> `property$multiple-entry-flag/124/obs=ascii for pipe char`.

- **position**: `[name, value]`

---

## Hierarchy level 2 — inside a historical source

### `historical-act`

> Represents a historical act, i.e. a record of an event — something that
> happened at a moment and place in time. Used for parish records, notarial
> acts, etc.

- **source**: `event`
- **position**: `[id, type, date]`
- **guaranteed**: `[id, type, date]`
- **contains**: `person`, `object`, `geoentity`, `abstraction`, `ls`, `atr`, `rel`, `cevent`, `end`

Related groups:

- **`event`** — "Something that happened", for letters, chronicles,
  interviews; also inside a formal dated record to note things that
  occurred at another time.
- **`cevent`** (and synonym `crono`) — chronology event: date, place,
  description.
- **`pevent`** — a *personal* event, contained inside a `person` or
  `object` (as opposed to `cevent`, which contains people).
- **`ulist`** — a list of unique people/objects (prosopographies,
  biographical dictionaries, household lists). Source: `historical-act`.
- **`text`** — record the original text (triple-quoted), before the
  corresponding Kleio notation.

### `authority-record` and its real entities (inside `identifications`)

- **`authority-record`** (abstract) — an entry in an authority register.
- **`rentity`** — a real entity; defines the standard name and the
  occurrences (`occ` subgroups).
- **`rperson`** — a real person (inside `identifications`).
- **`robject`** — a real object (inside `identifications`).
- **`occ`** — the ids of occurrences of a real entity.

---

## Hierarchy level 3 — entities (people, objects, places)

All of these ultimately derive from the abstract **`entity`** group
("a thing that exists independently of other things, with a name and a
type").

### `person`, `female`, `male`

| Group | Source | Description |
| --- | --- | --- |
| `person` | `entity` | A person. Position `[name, sex]`. Contains `attribute`, `relation`, `pevent`. |
| `female` | `person` | A female person. Groups based on this have female gender. |
| `male` | `person` | A male person. Groups based on this have male gender. |

#### Kin / non-kin roles (used inside acts)

| Group | Source | Description |
| --- | --- | --- |
| `kin-m` / `kin-f` | `male` / `female` | A male/female person related by kin to an actor. |
| `notkin-m` / `notkin-f` | `male` / `female` | A male/female non-kin person that can be part of an actor (e.g. servants, slaves, foundlings, persons referred to but not present). |
| `kin-father` | `kin-m` | Father. |
| `kin-mother` | `kin-f` | Mother. |
| `kin-husband` | `kin-m` | Husband. |
| `kin-wife` | `kin-f` | Wife. |
| `kin-son` | `kin-m` | Son. |
| `kin-daughter` | `kin-f` | Daughter. |
| `actorf` | `female` | A female actor in an act or event. |
| `actorm` | `male` | A male actor in an act or event. |

### `geoentity` (and synonym `place`)

> An entity that endures due to a specific location in space, in spite of
> changes in its characteristics and boundaries. Examples: a parish, a
> place, a region, a country. See Wikidata Q56061 (Administrative
> Territorial Entity) and Q486972 (Human Settlement).

- **source**: `entity`; **position**: `[name, type]`; **guaranteed**: `[name]`

### `object`, `abstraction`, `topic`

| Group | Source | Description |
| --- | --- | --- |
| `object` | `entity` | Anything that is not a person or geoentity: abstractions, animals, plants, objects, etc. |
| `abstraction` | `object` | An abstraction, such as a concept, idea, or event. |
| `topic` | `abstraction` | A topic, such as a subject or theme. |

---

## Groups inside acts and entities

### Attributes (`attribute` and aliases)

> Registers time-varying attributes of people, objects, geoentities, etc.

| Group | Source |
| --- | --- |
| `attribute` | — (position `[type, value, date]`, guaranteed `[type, value]`) |
| `ls` | `attribute` — alias, short for "life story". |
| `attr` | `attribute` — usually for metadata attributes. |
| `atr` | `attribute` — alias. |

### Relations (`relation` and alias)

> A relation between two or more entities. Relations have a type (kinship,
> professional…) and a value (e.g. "father", "mother", "colleague"). Some
> special relations are generated automatically by the parser, such as
> "identification" relations (from `same_as`/`xsame_as`) and
> "function-in-act" relations.

- **position**: `[type, value, destname, destination, date]`
- **guaranteed**: `[type, value, destname, destination]`
- **`rel`** — short hand for `relation`.

### `end`

> Marks the end of the enclosing group. Two uses: (1) to prevent subsequent
> groups being read as subgroups where there is ambiguity; (2) to trigger
> processing/export of groups in acts with long lists (e.g. household rolls
> — *róis de confessados* — where inference rules apply to each household
> independently).

### Other utility groups

| Group | Description |
| --- | --- |
| `group-element` | A group whose name matches an element of the enclosing group; during import the value of its first element becomes the value of that element. Useful when source values appear later in the text. |
| `attribute-list` | A list, similar to an act, enclosing people/objects/etc. that automatically receive a predefined attribute (type/value) from the enclosing group. |
| `geodesc` | A hierarchical group for geoentities (source: `historical-act`). |
| `geo1` … `geo4` | Geographical entities nested by level (`geo1` contains `geo2`, etc.). |
