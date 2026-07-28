# Kleio YAML Structure Migration — Next Steps

## 1. Summary

**Goal:** Eliminate semantic-test XML differences between the stable translator (running legacy `.str` structure files) and the dev translator (running YAML structure files), then expand coverage to more source files.

**Current state (latest run, `dehergne-a.xml`):**
- Schema renames (`tipo`/`type`, `valor`/`value`, `data`/`date`) are resolved.
- Remaining differences cluster in five areas:
  1. Linked-data resolution groups (`ls`/`atra*`) — 270 extra in dev, 232 missing vs stable.
  2. Function-in-act `relation` groups — 64 extra in dev, 58 missing vs stable.
  3. Person-name element naming — 64 `n` groups where dev emits `name` and stable emits `nome`.
  4. Systematic `order` and `level` shifts across `ls`, `relation`, and `referido` groups.
  5. Two structure-processing warnings: undefined `string64ma` base element and undefined `memoria58` group.

The plan below addresses each cluster in priority order, with the file to inspect, the suggested fix, and the verification command.

---

## 2. Priority 1: Linked Data Resolution Groups

### What the issue is
- The dev translator creates separate level-5 `atra*` attribute groups for every resolved Wikidata/linked-data reference (e.g., `ls$wikidata:person-id/...`).
- The stable translator embeds the resolved URL as a `<comment>` inside the original `ls` attribute element.
- Result: 270 `ls` groups appear only in dev, and 232 `ls` groups appear only in stable.

### Where in the code to look
- `src/gactoxml.pl`:
  - `processing_linked_data/2` (around line 1032).
  - `process_xlink_attribute_value/7` and `process_xlink_attribute_type/7` (around lines 1133–1151).
  - `export_auto_attribute/9` (around line 1845) — generates the `atra*` groups.
- Note a code drift vs stable: dev uses `clio_extends/2` while stable uses `clio_bclass/2` in linked-data processing. This changes which groups/elements trigger resolution.

### Suggested approach
1. Decide the target behavior. Because semantic tests compare against stable output, the quickest path to a green diff is to make the dev translator match stable output (embed URL in the original `ls` element comment, do not emit a separate `atra*` group).
2. If the separate `atra*` groups are the desired new behavior, update the reference translations instead.
3. Before changing output logic, reconcile `clio_extends/2` and `clio_bclass/2` usage so both translators resolve the same set of elements; otherwise any output-format change will still differ in count.
4. Add a targeted unit test or small CLI fixture that exercises `link$wikidata/...` annotations and inspects the resulting `ls` element.

### How to verify
```bash
make redo-test-semantics
python .qoder/skills/interpret-kleio-xml-diff/scripts/diff_xml_groups.py \
  --diff tests/reports/test_report_*.diff
```
Expect the linked-data section for `dehergne-a.xml` to show no extra/missing `ls` groups and no `changed elements: type, value, date, line` noise tied to `atra*` emission.

---

## 3. Priority 2: Function-in-Act Relation Groups

### What the issue is
- Dev auto-generates 64 extra `relation` groups of type `function-in-act` that link `referido` persons back to the enclosing act.
- Stable omits those same 64 and generates 58 relations that dev does not.
- This indicates the trigger conditions for `process_function_in_act/2` differ between the two structure-loading paths.

### Where in the code to look
- `src/gactoxml.pl`:
  - `process_function_in_act/2` (around line 966).
  - `person_export/2`, `object_export/2`, `geoentity_export/2` — these call `process_function_in_act` after `group_to_xml`.
- `src/inference.pl`:
  - `do_auto_rels/1` and `export_auto_rel/4` (around lines 1586–1813) for auto-generated relations.
- Also compare `tests/stable/gactoxml.pl` against `src/gactoxml.pl` to find any unintended code drift.

### Suggested approach
1. Compare the stable and dev versions of `process_function_in_act/2` line-by-line; ensure the XML elements written (`inside`, `type`, `value`, `order`, `level`, `date`) are identical.
2. Check which groups are registered for function-in-act processing. The difference may come from `clio_elements/1` returning a different element list or from group inheritance (`source` vs `part`/`contains`) being resolved differently in YAML.
3. Add explicit guards so only the groups that should emit `function-in-act` relations do so.

### How to verify
- Run the diff skill and confirm the `relation` pattern counts drop to zero for both “only in file 2 (dev)” and “only in file 1 (stable)”.
- A minimal reproduction: translate a single act containing `referido$...` and diff only the generated `<GROUP NAME="relation" ... TYPE="function-in-act">` blocks.

---

## 4. Priority 3: Name/Nome Element Rename

### What the issue is
- In 64 `n` groups, dev outputs element `name` while stable outputs element `nome`.
- This is a concrete YAML structure definition error, not a translator logic bug.

### Where in the code to look
- `src/stru/pt-actorm.yaml`, line 27:
  ```yaml
  - group:
      name: n
      source: pt-actorm
      position: [name, sex]
  ```
- `tests/kleio-home/structures/pt-actorm.yaml`, same `n` group definition.
- The stable `.str` equivalent declares `part name=n; source=actorm; position=nome,sexo`, which is why stable emits `nome`.

### Suggested approach
1. Change the `n` group position from `[name, sex]` to `[nome, sexo]` in both YAML files.
2. Regenerate any cached JSON/derived structure artifacts (`*.str.json`, `*-structure.json`, `*-structure.yaml`) that were produced from the old YAML, or let the test run regenerate them.
3. Check other Portuguese actor groups (`referido`, `actorm`, `actorf`, etc.) for the same `[name, sex]` vs `[nome, sexo]` mismatch.

### How to verify
```bash
make yaml-stru-cpy    # copy tests/kleio-home/structures/*.yaml to src/stru
make redo-test-semantics
```
The diff skill should no longer report `n` groups with `changed elements: name, nome, order`.

---

## 5. Priority 4: Order and Level Differences

### What the issue is
- `ls` groups: 985 pure order diffs, 125 level+order diffs, 48 date/line/type/value ordering diffs.
- `relation` groups: 64 order-only diffs.
- `referido` groups: 43 `inside`/`level`/`order` diffs.
- These are systematic, not random: the YAML and `.str` paths produce groups/elements in different sequences and sometimes assign different nesting levels.

### Where in the code to look
- `src/dataDictionary.pl`:
  - `classes_topological_order/2` (around line 665) determines group ordering.
  - `group_elements/2` and `element_of/2` determine element ordering per group.
- `src/gactoxml.pl`:
  - `group_to_xml/3` and `elements_to_xml/1` / `ielements_to_xml/2` write elements in the order returned by `clio_elements/1`.
  - `export_auto_attribute/9` and `export_auto_rel/4` assign `ORDER` and `LEVEL` based on `inccount(group,_)` and `clio_path/1`.
- `src/yamlSupport.pl`:
  - `process_str_params/2` (around line 159) processes `source` and `name` parameters in a specific order; this affects how inherited `position`/`part` lists are merged.

### Suggested approach
1. **Element order within a group**: ensure `clio_elements/1` returns elements in declaration order for both `.str` and YAML paths. If the YAML loader uses `setof/3` or dictionary-key ordering, replace with order-preserving list processing.
2. **Group order**: make `classes_topological_order/2` stable and deterministic (e.g., tie-break by original declaration index). Compare the group list printed at translation start:
   ```
   Groups in this file:[...]
   ```
3. **Level differences**: investigate why `referido` nesting differs. The `referido` group `contains: [attribute, relation]` in YAML but the stable `.str` may include different child groups. Reconcile `part`/`contains` lists.
4. Consider adding a normalization step in the test comparator that ignores benign order differences, but only after confirming the order is semantically irrelevant.

### How to verify
- Use the diff skill in non-aggregated mode on a single file to inspect representative cases:
  ```bash
  python .qoder/skills/interpret-kleio-xml-diff/scripts/diff_xml_groups.py \
    --diff tests/reports/test_report_*.diff --no-aggregate | head -200
  ```
- After fixes, the aggregated report should show zero patterns containing only `order` or `level, order` changes.

---

## 6. Priority 5: Structure Processing Warnings

### What the issue is
Two warnings remain during structure loading:
1. `undefined base element string64ma`
2. `undefined fons/source group memoria58`

### Where in the code to look
- `src/stru/elements.yaml`:
  - The `sex` element declares `source: string64ma`, but no element named `string64ma` is defined. Only `string64` and `string256` exist.
- `src/stru/pt-acts.yaml`:
  - `apontamentosd` declares `source: memoria58` around line 1333.
  - `memoria58` is defined twice: a first minimal definition around lines 1314–1327 and a fuller one around lines 1479–1494. The first definition may be processed too late or be malformed, causing `copy_fons_g` in `src/dataDictionary.pl` to warn when `apontamentosd` inherits from it.
  - The warning is emitted from `copy_fons_g/2` in `src/dataDictionary.pl` (around line 629).

### Suggested approach
1. Fix `string64ma`:
   - Option A: change `sex` element source to `string64` (matches the 64-char id/names convention).
   - Option B: define a new `string64ma` element if a distinct type is required.
   - Prefer Option A unless a longer string is needed.
2. Fix `memoria58`:
   - Remove the duplicate/early malformed `memoria58` definition (lines 1314–1327) and keep the complete one (lines 1479–1494).
   - Or, if the early definition is intentional, move it before `apontamentosd` and ensure it has a valid `idprefix` and `guaranteed` list.
   - Similarly clean up the duplicate `apontamentos`/`apontamentosd` definitions.

### How to verify
- Run structure loading in isolation and confirm zero warnings:
  ```bash
  # From SWI-Prolog, load src/swiStart.pl and call stru_yaml on the schema.
  ```
- After `make redo-test-semantics`, check that the `.err` / `.rpt` files for `dehergne-a` and other tests no longer contain the two warnings.

---

## 7. Priority 6: SUPER_CLASS / BASE_CLASS semantics (needs importer review)

### What the issue is
The XML export emits two attributes on each `<ELEMENT>` that describe the
element's type lineage:

- `SUPER_CLASS` — the **direct** source/parent of the element
  (`clio_element_super/2` in `src/externals.pl:174`, i.e. the immediate `fons`).
- `BASE_CLASS` — the **root base class** of the element
  (`clio_element_bclass/2`).

These were made self-consistent for `SUPER_CLASS` by adding explicit
`source=` to the bare base elements (`type`, `loc`, `obs`, `value`,
`destination`, `destname`) in `tests/stable/gacto2.str` and
`src/stru/gacto2.str`. **However the meaning of `BASE_CLASS` changed between
the stable engine and the dev engine (issue #58), so the two attributes are no
longer self-evident and must be reconciled against the downstream importer.**

### What changed and why the values diverge
The dev engine (`src/externals.pl:196`, referencing issue #58) treats the
abstract data types (`string256`, `text`, `number`, `string64`, `id`, ... —
those marked `abstract: true` in `elements.yaml`) as **abstract**, and stops the
base-class chain *before* them. The old stable engine
(`tests/stable/externals.pl:146`) walks the chain to its absolute root.

Concrete example for `loc` (chain: `loc → string256`):

| engine | `SUPER_CLASS` | `BASE_CLASS` |
|---|---|---|
| stable (old) | `string256` | `string256` (chain root) |
| dev / YAML (issue #58) | `string256` | `loc` (last non-abstract) |

The dev behaviour is the intended one per the comment at `src/externals.pl:194`
(`clio_element_bclass(loc) = loc`), but it means `BASE_CLASS` is no longer "the
concrete DB column type" — it is now "the element's own logical class". Whether
that distinction matters depends on what consumes the XML.

### Where in the code to look
- Emission: `src/gactoxml.pl:2459-2483`
  (`clio_element_bclass/2` → `BASE_CLASS`; `clio_element_super/2` → `SUPER_CLASS`).
- Old vs new base-class logic:
  - old — `tests/stable/externals.pl:146` (`\+ clio_element_super(_,Bclass)`).
  - new — `src/externals.pl:196` + `last_non_abstract_el/3`
    (`clio_element_abstract_class/1`, `abstractus` flag).
- Abstract type declarations: `tests/kleio-home/structures/elements.yaml`
  (`abstract: true` on the data types).
- Structure definitions: `tests/stable/gacto2.str` and `src/stru/gacto2.str`.

### Why this needs the upstream importer
The Python importer that consumes these XML files relies on `BASE_CLASS` /
`SUPER_CLASS` to decide DB column types and table mapping. Before treating the
issue-#58 `BASE_CLASS` change as "correct and final", we must confirm the
importer still derives the right column type from the new values (e.g. that
`BASE_CLASS="loc"` with `SUPER_CLASS="string256"` maps to a `varchar(256)`
column, not to something keyed off `BASE_CLASS` alone).

### Suggested approach
1. Locate the importer code that reads `BASE_CLASS` / `SUPER_CLASS` from the XML.
2. Determine which attribute(s) it uses for: column type, column length,
   join/table mapping.
3. If it keys off `BASE_CLASS` for the concrete type, the new values will break
   it — either fix the importer to read `SUPER_CLASS` (the abstract root is now
   reachable only by following the chain), or revert/adjust the `bclass` logic.
4. Add a test fixture whose XML exercises a sourced element so the importer
   path is covered.

### How to verify
- Translate a fixture and confirm the importer produces the expected table DDL
  / ORM mapping for elements like `loc`, `type`, `obs`.
- After any importer change, re-run `make test-semantics`; the `BASE_CLASS`
  lines should no longer diverge once stable is moved to the issue-#58 engine
  (`make current-to-stable`).

---

## 8. Priority 7: `fonte$` backward-compatibility scanner (script needed)

### What the issue is
The `fonte` group no longer assumes any positional element beyond `id`
(see commit `1c9b5fe`: `position: [id]`, with `tipo` moved to `also:`).
Every field after `fonte$ID/` must now be declared explicitly
(`loc=`, `data=`, `tipo=`, `ano=`, ...). This is a **serious backward-
compatibility break**: many old transcriptions put very different kinds of
information in the second positional slot of `fonte$` without qualifying
the element name, e.g.:

```
fonte$bapt1714/a.u.c./tipo=bapt/obs=...     <- old: bare "a.u.c." positional
fonte$obiteirasproblem/1740-1745/loc=auc/... <- old: bare date positional
```

With the new schema the bare positional becomes an `UNDEFINED` element
and the file errors out. Each old `.cli` that relies on this must be
migrated to name its elements explicitly.

### What is needed
A script that scans `.cli`/`.kleio` files for `fonte$ID/SOMETHING/`
lines where `SOMETHING` is a bare positional value (not an
`element=value` pair), so old sources can be detected and migrated
before being processed by this version. The script should:

1. Find lines matching `fonte$<id>/<token>/` where `<token>` does not
   contain `=` (i.e. is positional, not `name=value`).
2. Report the file, line number, and the offending positional value.
3. (Optionally) suggest the likely intended element name based on the
   value's shape: a date `YYYY/YYYY` or `YYYY` -> `data=`, a location
   string -> `loc=`, a type word -> `tipo=`, etc.

### Where to put it
`tests/scripts/` or `utilities/` (a small standalone tool, not part of
the translator). Can be awk/grep/python; should run over a directory tree.

### How to verify
Run it against the existing `tests/kleio-home/sources/` tree; the known
offenders (the paroquiais files already migrated in `1c9b5fe`) should
come up clean, while untouched historical sources should be flagged.

---

## 9. Verification


### Full semantic test run
```bash
cd /Users/jrc/develop/timelink-kleio-worktrees/yaml_str
make test-semantics            # stable + dev + compare (slow)
# or, when stable translations already exist:
make redo-test-semantics       # dev only (faster iteration)
```

### Inspect the diff
```bash
# Aggregated summary (recommended first look)
python .qoder/skills/interpret-kleio-xml-diff/scripts/diff_xml_groups.py \
  --diff tests/reports/test_report_*.diff

# Full detail for debugging a specific pattern
python .qoder/skills/interpret-kleio-xml-diff/scripts/diff_xml_groups.py \
  --diff tests/reports/test_report_*.diff --no-aggregate
```

### Per-fix smoke tests
1. **Name/nome**: translate a minimal CLI containing `n$João/M` and grep the XML for `<ELEMENT NAME="nome">`.
2. **Linked data**: translate a CLI with `kleio$... link$wikidata/...` plus `n$...#@wikidata:Q...` and count emitted `ls` vs `atra*` groups.
3. **Function-in-act**: translate one act with `referido$...` and diff only `relation` groups of type `function-in-act`.
4. **Warnings**: load the YAML schema and assert `error_count(0), warning_count(0)` after `stru_yaml/1`.

### Success criteria
- `dehergne-a.xml` diff shows no patterns related to linked-data `ls` groups, `function-in-act` relations, or `name`/`nome`.
- Order-only and level-only patterns are eliminated or explicitly accepted as benign.
- Structure processing produces zero warnings.

---

## 10. Expansion

Once `dehergne-a.xml` (the linked-data reference file) passes cleanly:

1. **Re-run the full semantic suite** on all source files under `tests/kleio-home/sources/`:
   ```bash
   make test-semantics
   ```
2. **Add new reference source files** that exercise the fixed areas:
   - A small file dedicated to `link$` annotations.
   - A file with nested `referido`/`referida` inside acts.
   - A file using `memoria58` / `apontamentosd` to guard against regression of the structure warnings.
3. **Update `tests/stable/`** only when the dev output is confirmed correct and should become the new reference (run `make current-to-stable`).
4. **Document schema conventions** in `docs/doc/linked_data.md` and/or `README_DEV.md` so future YAML edits preserve element naming and ordering consistency.

---

## Quick Reference: Key Files

| Area | Primary Files |
|---|---|
| Translator logic | `src/gactoxml.pl`, `src/inference.pl`, `src/dataDictionary.pl` |
| YAML structure loader | `src/yamlSupport.pl`, `src/struCode.pl` |
| Portuguese actor groups | `src/stru/pt-actorm.yaml`, `tests/kleio-home/structures/pt-actorm.yaml` |
| Core groups / elements | `src/stru/groups.yaml`, `src/stru/elements.yaml` |
| Portuguese acts | `src/stru/pt-acts.yaml`, `tests/kleio-home/structures/pt-acts.yaml` |
| Test harness | `tests/scripts/run_tests.sh`, `tests/scripts/redo_run_tests.sh`, `tests/scripts/compare_test_results.sh` |
| Diff analysis skill | `.qoder/skills/interpret-kleio-xml-diff/skill.md` |

