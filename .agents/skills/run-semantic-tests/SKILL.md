---
name: run-semantic-tests
description: Run the Kleio semantic test suite and analyze the results. Use whenever the user asks to "run tests", "run semantic tests", "check test results", "redo tests", "analyze the diff", "generate a test report", or wants to verify that YAML schema changes match the stable translator output. Also use when the user wants to translate a single .cli file for debugging.
---

# Run Semantic Tests

## Purpose

Run the Kleio translator semantic test suite, which translates all `.cli` files in `tests/kleio-home/sources/reference_sources/` with both the **stable** translator (legacy `.str` schema) and the **dev** translator (YAML schema), then diffs the XML output. The diff reveals where the YAML-based schema diverges from the stable reference.

After running, analyze the results and write a Markdown report summarizing errors, diff patterns, and root causes — alongside the raw `.diff` report.

## Prerequisites

- **`swipl` (SWI-Prolog) must be in PATH.** On this machine it lives at `/Applications/SWI-Prolog.app/Contents/MacOS/swipl`. If `which swipl` fails, export it:
  ```bash
  export PATH="/Applications/SWI-Prolog.app/Contents/MacOS:$PATH"
  ```
- All commands run from the **repo root** (`/Users/jrc/develop/timelink-kleio-worktrees/yaml_str`).
- The test takes **5–10 minutes** (stable translates ~200 files sequentially, then dev via server). Run it in the background and poll for completion.

## How to run the tests

### Full test (stable + dev + compare)

Use when stable translations need regenerating (e.g. after changing `reference_sources/` or stable code):

```bash
cd /Users/jrc/develop/timelink-kleio-worktrees/yaml_str
export PATH="/Applications/SWI-Prolog.app/Contents/MacOS:$PATH"
make test-semantics
```

This calls `tests/scripts/run_tests.sh`, which:
1. `prepare_tests.sh` — cleans `reference_translations/`, `test_translations/`, `dev/`; copies `src/*` → `dev/`; copies `reference_sources/*` → both translation dirs.
2. Translates every `.cli`/`.kleio` in `reference_translations/` with **stable** (`stable/swiStart.pl` + `stable/gacto2.str`) — sequential, one `swipl` process per file.
3. Starts the **dev** HTTP server (`dev/serverStart.pl`, loads `kleio-home/structures/sources-structure.yaml`) and translates `test_translations/` via REST.
4. `compare_test_results.sh` — diffs `reference_translations/` vs `test_translations/`, filtering known-benign differences via `exclude_while_comparing.grep`.
5. Writes the report to `tests/reports/test_report_<timestamp>.diff`.

### Redo test (dev only — faster)

Use when only the YAML schema or dev code changed, and stable translations already exist:

```bash
make redo-test-semantics
```

This calls `tests/scripts/redo_run_tests.sh` — skips the stable translation step (step 2 above), only re-runs the dev translation + comparison. Takes ~2 minutes.

### Single-file test (for debugging)

To translate one `.cli` file with both translators and compare:

```bash
# Stable (CLI mode — no server needed):
swipl -f tests/stable/swiStart.pl -- -sf tests/stable/gacto2.str -df <path/to/file.cli> -echo no

# Dev (server mode — see scripts below, or use the REST API on port 8088)
```

For the dev server approach, see `tests/scripts/kleio_start_server.sh` / `kleio_translate_remote.sh` / `kleio_stop_server.sh`.

## After the test completes

### 1. Find the latest report

```bash
LATEST=$(ls -t tests/reports/test_report_*.diff | head -1)
```

### 2. Gather error counts

```bash
# Per-file error counts (from .files.json):
for f in $(find tests/kleio-home/sources/test_translations -name "*.files.json"); do
  e=$(python3 -c "import json; print(json.load(open('$f')).get('errors'))")
  [ "$e" != "0" ] && [ -n "$e" ] && echo "$(echo $f | sed 's|.*/test_translations/||; s|/.files.json||'): $e"
done
```

Compare dev vs stable error counts — if stable has 0 and dev has errors, it's a **regression**. If both error similarly, it's a pre-existing data-quality issue.

### 3. Analyze diff patterns

```bash
# Aggregated XML GROUP patterns:
python3 .qoder/skills/interpret-kleio-xml-diff/scripts/diff_xml_groups.py --diff "$LATEST"

# Full detail (no aggregation):
python3 .qoder/skills/interpret-kleio-xml-diff/scripts/diff_xml_groups.py --diff "$LATEST" --no-aggregate
```

### 4. Categorize errors by type

```bash
for f in $(find tests/kleio-home/sources/test_translations -name "*.rpt"); do
  grep -iE "^ERROR:" "$f" 2>/dev/null
done | sed -E 's/line [0-9]+ //; s/\([^)]*\)//g; s/[0-9]+/N/g' | sort | uniq -c | sort -rn | head -20
```

### 5. Diff breakdown by source area

```bash
python3 - <<'EOF'
import re, glob
rep = sorted(glob.glob('tests/reports/test_report_*.diff'))[-1]
txt = open(rep).read()
areas = {}
blocks = re.split(r'^(diff -r.*\.xml\s+\S+\.xml)$', txt, flags=re.M)
for i in range(1, len(blocks), 2):
    m = re.search(r'reference_translations/([^/]+)/', blocks[i])
    area = m.group(1) if m else 'other'
    body = blocks[i+1] if i+1 < len(blocks) else ''
    areas[area] = areas.get(area, 0) + len(re.findall(r'^[<>]', body, re.M))
for a, d in sorted(areas.items(), key=lambda x: -x[1]):
    print(f"  {a}: {d}")
EOF
```

## Writing the Markdown report

Write the report to `tests/reports/<same-basename>.md` (same timestamp as the `.diff`):

```bash
# e.g. if the diff is test_report_2026-07-28_14:36:34.diff
# write to tests/reports/test_report_2026-07-28_14:36:34.md
```

### Report structure

Use this template — adapt categories to what the data shows:

```markdown
# Semantic Test Report — Outstanding Issues Analysis

**Report:** `test_report_<timestamp>.diff`
**Generated:** <date>
**Test set:** `reference_sources/` — <N> files

## At a glance

| Metric | Value |
|---|---|
| Total files translated | N |
| Files with 0 errors | N (X%) |
| Files with errors | N |
| Diff volume | N lines |

### Diff breakdown by source area
(table: area → diff lines)

## Issue categories

For each distinct error/diff category:
- **Category name** — how many files, error counts (dev vs stable)
- **Error sample** — the actual error line from .rpt
- **Source data** — the relevant .cli line
- **Root cause** — what's wrong in the schema/translator
- **Regression?** — does stable also error? (dev-only = regression)

## Recommended fix priority
(table: priority → category → effort → impact)

## How to reproduce
(the make command + swipl PATH export)
```

## Key concepts

- **`reference_sources/`** — the master set of `.cli`/`.kleio` source files that drive the tests. Files here are translated by both stable and dev.
- **`reference_translations/`** — stable translator output (the "reference").
- **`test_translations/`** — dev translator output (compared against reference).
- **`more_sources/`** — source files NOT in the active test set (holding area).
- **`tests/stable/`** — frozen snapshot of the stable translator code + `gacto2.str`.
- **`tests/dev/`** — copy of `src/` made fresh each test run.
- **`exclude_while_comparing.grep`** — POSIX ERE patterns filtered from the diff (paths, timestamps, order numbers, BASE_CLASS metadata, etc.).
- **The `fonte`/kleiofile diff** appears in almost every file — it's a path artifact (stable and dev translate from different absolute paths), not a real difference.
- **`stru_warnings`** in `.files.json` — structure-loading warnings (duplicate includes, group redefinitions). Most are benign by-design.

## Schema sync workflow

When you fix YAML schema files for testing:
1. Edit in `tests/kleio-home/structures/` (this is what the dev server loads).
2. Run `make redo-test-semantics` to verify.
3. Sync to distribution: `make yaml-stru-cpy` (copies `tests/kleio-home/structures/*.yaml` → `src/stru/`).
4. Commit both `tests/kleio-home/structures/` and `src/stru/` versions.
