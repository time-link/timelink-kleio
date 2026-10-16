---
name: define-structure
description: Define new Kleio structures (schemas) in YAML — new acts, person roles, kinship groups, elements, or entire language packs. Use whenever the user asks to "define a new structure", "create a schema", "add a new group/act/role to the structure", "add a new element", "make a Portuguese variant of a group", or wants to extend the Kleio ontology with new group or element definitions. Also use for "how do I add a new act type" or "add a new role" questions.
---

# Define Kleio Structures (Schemas)

## Purpose

Guide the definition of new Kleio structure definitions in YAML: new groups (acts, person roles, kinship, containers) and elements. The skill ensures definitions follow the established ontology conventions, end up in the right file, get tested, and are synced for distribution.

## Before you start — read the conventions

Read these two files first (they are short and authoritative):

1. `docs/ontology/containment-hierarchy.md` — the two hierarchies (inheritance vs containment) and the three containment keys
2. `references/parameters.md` (in this skill) — every YAML parameter with meaning and examples

If defining Portuguese vocabulary, also skim `docs/ontology/portuguese-vocabulary.md`.

## Where definitions live

| You are defining... | It goes in... | Why |
|---|---|---|
| Generic (language-neutral) group or element | `tests/kleio-home/structures/groups.yaml` / `elements.yaml` | The system-level core |
| Portuguese specialization of an existing generic group | `tests/kleio-home/structures/pt-groups.yaml` (acts → `pt-acts.yaml`, male roles → `pt-actorm.yaml`, female → `pt-actorf.yaml`, kinship → `pt-parentem.yaml`/`pt-parentef.yaml`, elements → `pt-elements.yaml`) | Language packs specialize the generic core |
| A whole new language pack | New `xx-*.yaml` files + include chain | Same pattern as `pt-*` |

**Always edit in `tests/kleio-home/structures/`** (the test-time authority), then sync with `make yaml-stru-cpy` (copies to `src/stru/` for distribution).

## The workflow

### Step 1 — Interview the user

Ask (in one round, batching questions) unless the request already answers them:

1. **What kind of thing?** — an act/event, a person role, a kinship group, a non-kin associated person, a container, an object/place type, an element, or a whole act family?
2. **Language-neutral or language-specific?** — if the group's positional elements are language-specific (e.g. `nome`/`sexo` vs `name`/`sex`), you need a generic group + a language specialization. If it's only ever needed in one language, a single file may do — but prefer generic + specialization to match the established architecture.
3. **What does it extend?** — every group needs a `source:` (except roots like `entity`). Find the right parent in `docs/ontology/base-ontology.md`. Use AskUserQuestion to propose parents if ambiguous.
4. **What are its positional elements?** — what values follow `group$id/` in the Kleio data? (e.g. `bap$id/dia/mes/ano/fol/loc/celebrante`)
5. **What can nest inside it?** — actors, kin, attributes, relations?

### Step 2 — Choose the parent class correctly

This is the step where mistakes are most common. Use this decision table:

| The new group is... | Parent (`source:`) | Containment note |
|---|---|---|
| An act type (dated record) | `pt-acto` (or the closest existing act) | Declare `contains:`/`part:` with its actors |
| A male actor role in acts (e.g. `outorgante`) | `pt-actorm` | Actors are NOT contained by `n` — correct |
| A female actor role | `pt-actorf` | Same |
| Male kin of a person (e.g. a new kin term) | the matching `pt-kin-*` (`pt-kin-father`, `pt-kin-son`, ...) | Kin nest under `n` automatically via `n.contains: [kin-m, kin-f, ...]` — do NOT list them in `n` |
| Female kin | the matching `pt-kin-*` (`pt-kin-mother`, `pt-kin-wife`, ...) | Same |
| A non-kin person that nests under a person (servant, foundling, referred) | `pt-notkin-m` / `pt-notkin-f` | Nests under `n` via `notkin-m`/`notkin-f` |
| A household-level person (like `escravo`, nests under `fogo`, NOT under `n`) | `pt-actorm` / `pt-actorf` | Add to the household group's `contains:` explicitly |
| A new element | the closest existing element (`name`, `loc`, `date`...) or an abstract data type (`string64`, `string256`, `text`, `number`) | Elements also use `source:` for inheritance |

**Critical rules learned from the migration:**

- **Reference classes the fons chain actually traverses.** In `contains:` lists, name the *generic* convergence classes (`kin-m`, `kin-f`, `notkin-m`, `notkin-f`), not language specializations (`pt-kin-m`) — the chains pass through the generics. See issue #60 for the open design question here.
- **`position:` must be re-declared in language specializations** (`position: [nome, sexo]` in `pt-*` files). Otherwise the fons-copy overwrites it with the parent's language (English) and the data parsing breaks with `undefined element` errors.
- **Portuguese element names need element definitions** — `dia`/`mes`/`ano` etc. must exist as elements (in `pt-elements.yaml`, sourced from `day`/`month`/`year`) before being used in a `position:`.
- **Do not enumerate** descendants in `contains:` when a convergence class covers them (the 283-name `arbitrary` enumeration was replaced by 4 names — don't reintroduce enumerations).

### Step 3 — Write the definition

Use this template (block style for groups with many parameters, flow style `{...}` for one-liners):

```yaml
# Generic (groups.yaml) — language-neutral
- group:
    name: mygroup
    source: <parent>
    description: >
        One-paragraph description of what this group represents.
    position: [<elem1>, <elem2>, ...]      # positional elements after group$id/
    guaranteed: [<elem>, ...]               # required elements
    also: [<elem>, ...]                     # optional named elements
    contains: [<child>, ...]                # what may nest inside
    idprefix: <3-letter-prefix>             # for auto-generated IDs
```

```yaml
# Language specialization (e.g. pt-actorm.yaml) — flow style for simple roles
- group: {name: myrole, source: pt-actorm}
# or block style with position override
- group:
    name: myrole
    source: pt-actorm
    position: [nome, sexo]
```

For elements:

```yaml
- element:
    name: myelement
    source: string256            # or name, loc, date, number, text...
    description: What this element records.
```

See `references/parameters.md` for every available parameter (`part`, `arbitrary`, `always`, `only`, `repeat`, `prefix`, `suffix`, `order`, `abstract`, ...).

### Step 4 — Wire into the include chain (new files only)

If you created a *new* YAML file, it must be included. Check the chain:
`sources-structure.yaml` → `pt-sources-structure.yaml` → `pt-groups.yaml` → `pt-actors.yaml`/`pt-parents.yaml` → `pt-actorm.yaml`/`pt-actorf.yaml`/`pt-parentem.yaml`/`pt-parentef.yaml` → `pt-acts.yaml`.

Add `- include: <file>.yaml` at the right level (elements before groups that use them).

### Step 5 — Verify

```bash
cd /Users/jrc/develop/timelink-kleio-worktrees/yaml_str
export PATH="/Applications/SWI-Prolog.app/Contents/MacOS:$PATH"

# 1. Load the schema and check the new group resolves correctly:
cat > /tmp/test_newgrp.pl << 'EOF'
go :-
    catch(stru('kleio-home/structures/sources-structure.yaml'), E, (print_message(error, E), halt(1))),
    % EDIT: replace grp with the new group name
    ( externals:clio_group_param(grp, locus, Pos) -> format('position=~w~n',[Pos]) ; format('NO position~n') ),
    ( externals:clio_extends(grp, Parent) -> format('extends ~w~n',[Parent]) ; true ),
    ( dataDictionary:contained_by(grp, n) -> writeln('contained by n: YES') ; writeln('contained by n: no') ),
    halt.
EOF
/Applications/SWI-Prolog.app/Contents/MacOS/swipl -f src/swiStart.pl -g "consult('/tmp/test_newgrp.pl'), go" -t halt 2>&1 | grep -E "position=|extends|contained|NO"

# 2. Check structure warnings did not increase (compare stru_warnings before/after):
#    look at any tests/kleio-home/sources/test_translations/**/*.files.json -> "stru_warnings"

# 3. Run the semantic tests (dev only, ~2 min):
make redo-test-semantics
python3 .agents/skills/run-semantic-tests/scripts/generate_report.py
```

**Expect:** 0 new errors, 0 regressions, and the diff volume unchanged (a pure addition cannot change existing translations unless it collides with an existing name — if the diff grows, the name is already defined and you merged into it).

### Step 6 — Write a smoke-test source file

Create a minimal `.cli` exercising the new group and add it to `reference_sources/` (or a scratch dir) to lock in the definition:

```kleio
kleio$gacto2.str/translations=1
   fonte$test-newgroup/tipo=teste/data=1900

      mygroup$test-g1/valor1/valor2
         n$Teste/m
```

Translate it and confirm the output has the expected elements/groups. This file also becomes a regression fixture.

### Step 7 — Sync and commit

```bash
make yaml-stru-cpy     # tests/kleio-home/structures/*.yaml -> src/stru/
git add src/stru/ tests/kleio-home/structures/
git commit -m "feat: define <name> group in <file> (<one-line rationale>)"
```

Both copies must be committed together — they are kept identical by `yaml-stru-cpy`.

## Common pitfalls (all seen in this migration)

1. **Quoted flow lists** — `position: '[nome, sexo]'` (with quotes) parses as a *string*, causing "undefined element" errors. Always `position: [nome, sexo]` unquoted.
2. **Forward references** — a group can only `source:` a group defined *earlier* in the include chain. Define parents before children.
3. **`source:` triggers fons-copy** — setting `source: X` copies X's properties onto the new group (replacing its own if re-declared later by the parent). If the new group needs different `contains:`, it must re-declare them explicitly.
4. **Wrong containment level** — decide person-level (`notkin-*`, nests under `n`) vs household-level (`pt-actor*`, nests under `fogo`/the act container). The `escravo` bug: slaves are household-level, maids are person-level.
5. **Case-sensitivity** — `pt-kin-m` ≠ `pt-kin-M`. All names are lowercase with hyphens.

## References

- `references/parameters.md` — full parameter reference with examples (read when unsure about a parameter)
- `docs/ontology/base-ontology.md` — the class hierarchy
- `docs/ontology/containment-hierarchy.md` — containment rules and the tree shape
- `docs/ontology/portuguese-vocabulary.md` — the Portuguese naming conventions
- `.agents/skills/run-semantic-tests/SKILL.md` — how to run/verify tests
