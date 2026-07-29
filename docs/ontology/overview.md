# Overview — how it all fits in the Timelink system

This page situates the Kleio ontology documented in this directory within
the wider **Timelink** system. It is a conceptual map: for the precise
definitions of each group and element, see
[Base ontology](base-ontology.md) and
[Portuguese vocabulary](portuguese-vocabulary.md).

> The description here follows the
> [Timelink basic concepts](https://time-link.github.io/timelink-docs/introduction/basic_concepts/#persons)
> documentation.

## The two models

Timelink is built on a **dual-model architecture** that bridges two
genuinely different tasks: faithfully recording historical texts, and
performing modern data analysis.

### Source Oriented Model (SOM)

The SOM captures historical data **exactly as it appears in the original
texts**. It is based on Manfred Thaller's Kleio notation. A source is
transcribed as a hierarchy of *groups* (entities) and *elements*
(attributes). The ontology in this directory — the groups and elements
defined in the `structures/` YAML files — is precisely the vocabulary of
the SOM.

The guiding principle of the SOM is to **minimize data loss**: everything
the source says is recorded, in the order and form it appears, before any
interpretation is applied.

### Person Oriented Model (POM)

The POM is the **relational, analytical** view. It aggregates the
fragmentary records of the SOM into cohesive biographies, organized around
people and objects. It uses ordinary relational-database terminology:
*tables* for entities, *columns* for attributes.

The POM organizes a historical life around three domains:

| Domain | Meaning | Example |
| --- | --- | --- |
| **Functions** | The roles an individual plays in acts. | *father* in a baptism, *groom* in a marriage, *lender* in a contract. |
| **Attributes** | Time-varying details tied to a date. | profession, residence, age, civil status. |
| **Relations** | Connections between entities, with a type and a value. | kinship (*wife*), professional (*colleague*), or a *function-in-act*. |

The bridge between the two models is the **Kleio translator**.

## The core concepts

These are the entities the ontology is built to express. Each maps to a
historical reality and to one or more groups in the schema.

- **Sources.** A historical source contains one or more acts. In the
  schema this is the `historical-source` group (Portuguese: `fonte`).
- **Acts / Events.** A record of something that happened at a specific
  moment and place, as described in the source — a baptism, a marriage, a
  burial, a rental contract, a notarial deed. Acts contain actors and
  objects. In the schema: `historical-act` / `event` (Portuguese:
  `pt-acto` and its many specializations such as `bap`, `cas`, `obito`,
  `escritura`…).
- **Persons.** The actors in the acts. A person always appears with a
  *function* in an act (e.g. the child in a baptism, the bride in a
  marriage). In the schema: `person` / `female` / `male` and the actor
  and kin groups (Portuguese: `n`, `referido/a`, `celebrante`, `noivo/a`,
  etc.).
- **Objects.** Anything that is not a person or a geographical entity:
  physical objects, buildings, spaces, but also *abstract* or intangible
  things such as institutions. In the schema: `object` / `abstraction`
  (Portuguese: `bem`, `divida`, `garantia`, institutions like
  `misericordia`, `convento`…).
- **Geoentities.** Entities that endure due to a location in space — a
  parish, a place, a region, a country. In the schema: `geoentity`
  (Portuguese: `lugar`, `freguesia`, `provincia`, `bispado`…).
- **Attributes.** Information describing an entity, tied to a specific
  date (a name, an age, a profession, a residence). In the schema:
  `attribute` and its aliases `ls`, `atr`, `attr`.
- **Relations.** Connections between entities, with a *type* (kinship,
  professional…) and a *value* (father, wife, lender, colleague). Some
  relations are generated automatically by the parser (identification
  relations from `same_as`/`xsame_as`, and function-in-act relations). In
  the schema: `relation` and its alias `rel`.

## The translation pipeline

The **Kleio translator** is the program that turns a SOM transcription
into POM data. The flow is:

```
                       Kleio translator
   Kleio notation  ──────────────────────►  Person Oriented Model
   (*.kleio files)        │                 (relational database)
                         │
            configured by three kinds of file:
            ├── Structure / schema files   (this ontology)
            ├── Database mapping files     (planned doc)
            └── Inference files            (planned doc)
```

1. **Transcription.** A researcher writes a source in Kleio notation,
   using the groups and elements of a chosen structure (e.g. the
   Portuguese `fonte`/`pt-acto` vocabulary). Kleio files use the `.kleio`
   extension; `.cli` is the legacy extension still found in old projects,
   and both are accepted by the translator.
2. **Translation.** The Kleio translator parses the file, applies the
   schema, generates stable unique identifiers, and emits structured data
   for the relational database.
3. **Storage & analysis.** The POM data lands in interconnected tables and
   can be queried for statistical analysis, network analysis and
   prosopography — via Timelink's web interface, or directly (e.g. pandas
   in Jupyter notebooks).

## How the configuration files relate

To handle specialized historical sources that use their own terminology
(writing `padrinho` instead of a generic `person`, or `bap` instead of
`historical-act`), the translator is driven by **three families of
configuration files**:

| File family | Role | Documented here? |
| --- | --- | --- |
| **Structure / schema files** | Define the groups and elements (the ontology) and how new groups relate to the core groups — so transcribers can use terminology close to the original source. | ✅ This directory |
| **Database mapping files** | Describe how the information of each group and element is stored in the database tables and columns. | 🔜 Planned |
| **Inference files** | Contain rules to infer attributes and relations that are *not explicit* in the source — e.g. deduce a person's gender from being recorded as *father* or *mother*, or infer civil status from a marriage/burial. | 🔜 Planned |

> **Roadmap.** Documentation for **database mapping files** and
> **inference rules** will be added to this directory at a later stage.

## Where the ontology sits

The Kleio ontology is the **vocabulary of the SOM**. Everything in this
directory — the base `entity`/`person`/`historical-act`/… groups and the
Portuguese `fonte`/`bap`/`cas`/… extensions — exists so that a source can
be transcribed richly and faithfully, while remaining processable by the
translator into the analytical POM. The inheritance mechanism (`source:`)
is what lets a project introduce source-specific terms (`padrinho`,
`escrivao`, `devassa`) while staying connected to the core model that the
mapping and inference files rely on.

## Further reading

- Timelink docs — [Basic concepts](https://time-link.github.io/timelink-docs/introduction/basic_concepts/#persons)
- [Base ontology](base-ontology.md) — core elements and groups.
- [Portuguese vocabulary](portuguese-vocabulary.md) — Portuguese acts and actors.
