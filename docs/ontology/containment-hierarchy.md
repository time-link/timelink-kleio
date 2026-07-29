# Containment hierarchy

The Kleio ontology has **two independent hierarchies**. It is important
not to confuse them:

| Hierarchy | Defined by | Meaning |
| --- | --- | --- |
| **Inheritance** ("is-a") | the `source:` key | A group *extends* another: it keeps the parent's parameters unless overridden. This is how `cas` *is-a* `pt-acto` *is-a* `historical-act`. See [Base ontology](base-ontology.md). |
| **Containment** ("has-a" / nesting) | the `contains`, `part` and `arbitrary` keys | A group *may contain* other groups nested inside it, as children in the Kleio file. **This page is about containment.** |

Containment is what determines the **shape of a Kleio file** — which
groups may be written inside which. It is implicit in the base ontology:
most groups declare a `contains` list, and the result is a tree rooted at
the `kleio$` line of every file.

## The three containment keys

- **`contains`** — the canonical list of groups that may appear nested
  inside this group. Used by the core ontology.
- **`part`** — synonym of `contains`, used in the Portuguese files for
  readability ("these are the parts of this act").
- **`arbitrary`** — like `contains`, but signals a looser, more ad-hoc
  containment (groups allowed without a strict schema contract). Common
  in the Portuguese act definitions.

All three have the same effect: the listed groups are valid children.
A child that is not declared in any of the three is rejected by the
parser.

> **Inheritance brings containment along.** When a group extends another
> via `source:`, it inherits the parent's containment list *unless* it
> re-declares `contains`/`part`/`arbitrary`. This is why, for example, a
> Portuguese `bap` (which extends `pt-acto` and only adds a `contains`
> list) ends up allowing the `pt-acto` children plus its own.

## The framework

Every Kleio file has the same top-level skeleton. The `kleio$` line is the
document root; it contains one or more **sources**, **authority
registers**, **links** and **properties**. A source, in turn, contains
**acts/events**, which contain **people, objects, places, attributes and
relations**. The diagram below shows the first levels, with the leaf
groups collapsed.

```
kleio$  (the document; not stored in the database)
│
├── source$  historical-source      ◄── the main container (alias: historical-source)
│   │                                 Portuguese: fonte$
│   │
│   ├── historical-act$  / event$    a dated record (Portuguese: pt-acto, bap, cas, obito…)
│   │   ├── person$  / female$ / male$        the actors (Portuguese: n, noivo, celebrante…)
│   │   │   ├── attribute$  (alias ls, atr)   time-varying attributes
│   │   │   ├── relation$   (alias rel)       relations to other entities
│   │   │   └── pevent$                         a personal event (inside the person)
│   │   ├── object$  / abstraction$           anything not a person or place
│   │   │   ├── attribute$
│   │   │   ├── relation$
│   │   │   └── pevent$
│   │   ├── geoentity$  (alias place)         a place (parish, region, country)
│   │   ├── attribute$  (alias ls, atr)
│   │   ├── relation$   (alias rel)
│   │   ├── cevent$                              a chronology/event nested in the act
│   │   └── end$                                 marks the end of a processable block
│   │
│   ├── event$                                  a non-formal event (letters, chronicles)
│   └── text$                                   the original transcribed text
│
├── authority-register$  (abstract)             Portuguese usage: identifications$
│   └── authority-record$  (abstract)
│       ├── rentity$         a real entity (aggregate of occurrences)
│       │   └── occ$           the occurrences of that entity
│       ├── rperson$         a real person
│       │   └── occ$, attribute$, relation$ …
│       └── robject$         a real object
│           └── occ$, attribute$, relation$ …
│
├── link$        shortcut to external / linked data (@shortname:id)
└── property$    parser-level file properties (not stored)
```

The arrows below each box show the containment declared by that group's
`contains` key. Reading the diagram top-down answers the question
*"what am I allowed to write inside what?"*.

## Where each level comes from

These are the `contains` declarations in
[`groups.yaml`](../../tests/kleio-home/structures/groups.yaml) that produce
the framework above:

- **`kleio`** contains `[historical-source, authority-register, link, property]`.
- **`historical-source`** contains `[historical-act, event, text]`.
- **`historical-act`** contains
  `[person, object, geoentity, abstraction, ls, atr, rel, cevent, end]`.
- **`event`** contains
  `[person, object, geoentity, abstraction, ls, atr, rel, end]`.
- **`person`** contains `[attribute, relation, pevent]`.
- **`object`** contains `[attribute, relation, pevent]`.
- **`authority-register`** contains `[authority-record]`.
- **`identifications`** (the concrete authority register) contains
  `[rentity, rperson, robject]`.
- **`rentity`** contains `[occ]`; **`rperson`/`robject`** contain
  `[occ, ls, atr, rel, attribute, relation]`.

Note that `attribute` (`ls`/`atr`/`attr`) and `relation` (`rel`) are
**leaves**: they declare no `contains`, so nothing is nested inside them.

## How the Portuguese vocabulary changes the tree

The Portuguese groups override or extend these `contains` lists. Two
recurring patterns:

1. **An act re-declares its children.** `pt-acto` (the Portuguese abstract
   act) sets `contains: [actorm, actorf, item]`. Concrete acts then build
   on top: `bap` contains
   `[celebrante, n, test, referido, referida, procurador-pad, procurador-mad]`;
   `cas` contains the full bride/groom kinship tree; `escritura` lists the
   dozens of notarial actor roles; `obito` contains `n`, `marido`,
   `mulher`, `pai`, `mae` and the group-elements `sacr`, `testamento`,
   `locs`, `locf`, `causa`, `oficios`. See
   [Portuguese vocabulary → Acts](portuguese-vocabulary.md#portuguese-acts).

2. **`arbitrary` is used heavily.** Many Portuguese acts allow children
   through `arbitrary` rather than `contains`, e.g. `hab` allows
   `[n, referido, referida, test]`; `docregio` allows
   `[atr, ls, rel, item, autor, autorf, destinatario, …]`. The effect on
   nesting is the same as `contains`.

3. **Kinship nesting.** Inside an actor such as `n` (the main person of a
   baptism), the Portuguese schema nests relatives: `n` contains
   `[kin-m, kin-f, notkin-m, notkin-f]`, and within a marriage act `noivo`
   contains `pnoivo`/`mpnoivo` (the groom's parents), which in turn
   contain the grandparents, and so on. This deep, regular nesting is
   spelled out in
   [Portuguese vocabulary → Kinship tree in marriage acts](portuguese-vocabulary.md#kinship-tree-in-marriage-acts).

## Why `end$` matters in the tree

`end$` is a leaf group that may appear inside any act or event. It is not
a child *entity* — it is a **delimiter**. It tells the parser "the current
processable block ends here", which matters in acts that hold many
independent items (e.g. a *rol de confessados* with many households):
each household is closed by an `end$` so inference rules fire on it
independently and context is cleared before the next one. See
[`end` in the base ontology](base-ontology.md#end).

## Further reading

- [Overview](overview.md) — how containment feeds the translation pipeline.
- [Base ontology](base-ontology.md) — the groups referenced above and the inheritance (`source:`) hierarchy.
- [Portuguese vocabulary](portuguese-vocabulary.md) — the Portuguese `contains`/`arbitrary`/`part` overrides.
