# Kleio Ontology

This directory documents the **Kleio ontology** as expressed in the structure
files under `kleio-home/structures/`. The ontology is the vocabulary of
*groups* and *elements* that Kleio source files (`*.kleio`) may use to
transcribe historical sources.

## What is a "structure"?

In Kleio, a **structure** (or *schema*) is a YAML file that defines:

- **Elements** — atomic data fields (a name, a date, an observation, …).
  Elements trigger specific behaviours during parsing and database mapping.
- **Groups** — structured records (a person, an act, an attribute, …).
  Groups have positional and named elements, may *contain* other groups,
  and may *extend* other groups through the `source` key (inheritance).

The default structure is assembled from modular files, starting from
[`sources-structure.yaml`](../../tests/kleio-home/structures/sources-structure.yaml):

```
sources-structure.yaml
├── elements.yaml          # core elements (base data types)
├── groups.yaml            # core groups (the generic ontology)
└── pt-sources-structure.yaml
    ├── pt-groups.yaml     # Portuguese top-level groups & overrides
    │   ├── pt-elements.yaml   # Portuguese element aliases
    │   ├── pt-actors.yaml     # Portuguese actors (people in acts)
    │   │   ├── pt-parents.yaml
    │   │   │   ├── pt-parentem.yaml   # male relatives
    │   │   │   └── pt-parentef.yaml   # female relatives
    │   │   ├── pt-actorm.yaml         # male actors
    │   │   └── pt-actorf.yaml         # female actors
    └── pt-acts.yaml       # Portuguese historical acts (baptism, marriage, …)
```

## Start here

➡️ **[Overview — how it all fits in the Timelink system](overview.md)**
If you are new to this, read the overview first. It explains the two models
(Source Oriented vs. Person Oriented), the core concepts (sources, acts,
persons, objects, attributes, relations), the Kleio translation pipeline,
and where this ontology sits — i.e. how everything works together.

## Documents

1. **[Overview](overview.md)** — how the ontology fits in the Timelink
   system (start here).

2. **[Base ontology](base-ontology.md)** — the core elements and groups
   defined in `elements.yaml` and `groups.yaml`, language-independent, and
   the first levels of the group hierarchy.

3. **[Containment hierarchy](containment-hierarchy.md)** — the implicit
   "has-a" nesting tree (`contains`/`part`/`arbitrary`) that determines
   which groups may be written inside which, rooted at `kleio$`.

4. **[Portuguese vocabulary](portuguese-vocabulary.md)** — how the Portuguese
   schema extends the base ontology, with the full list of Portuguese
   **acts** and **actors** (female and male).

5. **[Portuguese acts — examples](portuguese-examples.md)** — short, real
   examples of the most common Portuguese acts drawn from
   `reference_sources` (baptism, marriage, death, crisma, rolls, vereação,
   misericórdia, devassa, escritura).

## Key concepts used throughout

| Concept | Meaning |
| --- | --- |
| `source` | Inheritance ("is-a"): this group/element extends another, keeping its parameters unless overridden. |
| `contains` / `part` / `arbitrary` | Containment ("has-a"): the groups that may be nested inside this one. See [Containment hierarchy](containment-hierarchy.md). |
| `position` | Elements that may be written without their name (e.g. `person$id/Name/M`). |
| `guaranteed` | Elements that must be present. |
| `also` | Optional named elements in addition to the inherited ones. |
| `idprefix` | Prefix automatically generated for the group's ids. |
| `abstract` | The group/element is intended to be extended, not used directly. |

## Roadmap / TODO

These items are planned for a later stage:

- **Database mapping files** — document how Kleio groups/elements map to
  relational tables/columns (see [Overview](overview.md)).
- **Inference rules** — document the rules that infer attributes and
  relations not explicit in the source (e.g. gender from *father*/*mother*).
- **Kleio grammar for Linguist** — Kleio is not yet registered in
  [Linguist](https://github.com/github-linguist/linguist), so `` ```kleio ``
  renders as plain text on GitHub and the doc code blocks are left
  uncolored. A TextMate grammar already exists in the `timelink-vscode`
  extension (`syntaxes/Kleio.tmLanguage`). Contribute it to Linguist so
  `` ```kleio `` and `.kleio`/`.cli` files are highlighted natively; then
  switch the doc code fences to `kleio`.
