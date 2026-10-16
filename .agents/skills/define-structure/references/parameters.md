# Structure parameter reference

Every key accepted in `- group:` and `- element:` blocks, with the internal (Latin) name the translator uses, and examples. English keys are translated to Latin internally (`engkw`); either works in YAML but English is conventional.

## Group parameters

### Identity & documentation

| YAML key | Internal | Meaning | Example |
|---|---|---|---|
| `name` | `nomen` | Group name (lowercase, hyphens; must be unique) | `name: cas` |
| `description` | `nota` | Free-text documentation | `description: Marriage act` |

### Inheritance

| YAML key | Internal | Meaning | Example |
|---|---|---|---|
| `source` | `fons` | The parent group this extends (single inheritance; parent must be defined earlier). Triggers property copy from parent. | `source: pt-acto` |
| `abstract` | `abstractus` | Marks an over-generic class that `clio_bclass` should skip when computing the base class (e.g. `entity`, data types). Not inheritable. See issue #58. | `abstract: true` |

### Element slots

| YAML key | Internal | Meaning | Example |
|---|---|---|---|
| `position` | `locus` | Ordered positional elements filled left-to-right from the `/`-separated values after `group$id`. Must be defined elements. | `position: [id, dia, mes, ano]` |
| `guaranteed` | `certe` | Required elements — translator errors if absent | `guaranteed: [id, dia, mes, ano]` |
| `also` | `ceteri` | Optional named elements usable in this group | `also: [obs, loc, ref]` |
| `always` | `semper` | Elements repeated for every instance | `always: [attribution]` |
| `only` | `solum` | Restricts this group to appear only inside listed containers | `only: [lista]` |
| `order` | `ordo` | Declaration order hint | rarely used |

### Containment (what may nest inside)

| YAML key | Internal | Meaning | Example |
|---|---|---|---|
| `contains` | `pars` | Canonical containment list (children of this group) | `contains: [n, referido, atr]` |
| `part` | `pars` | Synonym of `contains` (Portuguese files prefer `part` for readability) | `part: [ppai, mpai]` |
| `arbitrary` | `repetitio` | Looser containment — allowed children without strict contract; also what allows repeated occurrence | `arbitrary: [pevent, ls, atr, rel]` |

All three have identical translator behavior. Inheritance brings the parent's containment along unless re-declared.

### IDs

| YAML key | Internal | Meaning | Example |
|---|---|---|---|
| `idprefix` | `signum` | 3-letter seed for auto-generated IDs (defaults from the base class) | `idprefix: cas` |
| `prefix` | `prae` | Prepended to IDs | `prefix: non` (suppresses prefix) |
| `suffix` | `post` | Appended to IDs | `suffix: non` (suppresses suffix) |
| `repeat` | (repetitio) | legacy synonym of `arbitrary` | |

## Element parameters

| YAML key | Internal | Meaning | Example |
|---|---|---|---|
| `name` | `nomen` | Element name | `name: dia` |
| `description` | `nota` | Documentation | |
| `source` | `fons` | Parent element this specializes (data type or concrete element). Element inheritance is also single-parent. | `source: day` or `source: string256` |
| `type` | | Documentation hint of the data type | `type: number` |

### Abstract data types (elements that are valid `source:` targets)

`number`, `string64` (short/ids), `string256` (names/short text), `text` (long text), `control` (parser control), `json`, plus concrete common elements: `name`, `sex`, `date`, `loc`, `ref`, `obs`, `id`.

Marked `abstract: true` in `elements.yaml`; they exist so database mapping has a stable root, and are skipped by `clio_element_bclass` (issue #58).

## File-level keys

| Key | Meaning | Example |
|---|---|---|
| `- file:` block | Header with `name` + `description` (documentation only) | see any schema file top |
| `- include: <file>.yaml` | Pull in another schema file (processed once; later includes of the same file are skipped with a benign warning) | `- include: pt-groups.yaml` |

## English → Latin keyword map (the common ones)

| English | Latin (internal) |
|---|---|
| `source` | `fons` |
| `name` | `nomen` |
| `position` | `locus` |
| `guaranteed` | `certe` |
| `also` | `ceteri` |
| `contains` / `group` / `part` | `pars` |
| `arbitrary` / `repeat` | `repetitio` |
| `always` | `semper` |
| `only` | `solum` |
| `idprefix` | `signum` |
| `prefix` | `prae` |
| `suffix` | `post` |
| `order` | `ordo` |
| `note` / `description` | `nota` |
| `abstract` | `abstractus` |
| `element` | `terminus` |
