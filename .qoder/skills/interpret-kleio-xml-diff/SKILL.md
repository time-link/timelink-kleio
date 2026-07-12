---
name: interpret-kleio-xml-diff
description: Interpret XML diffs in Kleio semantic test reports by identifying the top-level GROUP elements that contain differences and showing file A/B value changes. Aggregates structurally repeated differences. Use when the user asks to understand differences between two Kleio XML files, interpret XML sections of a test report diff, or compare XML outputs from semantic tests.
---

# Interpret Kleio XML Diff

## Purpose

Help understand XML differences in Kleio semantic test reports by locating the top-level `<GROUP>` elements that contain changes, showing the actual values from each file, and aggregating repeated structural differences so a single pattern does not flood the output.

## When to use

- User says "help me understand the differences between A1.XML and A2.XML"
- User asks to interpret XML sections of `tests/reports/test_report_*.diff`
- User wants to know which GROUPs changed between stable and dev translator outputs

## How to run

Use the helper script in this skill:

```bash
python .qoder/skills/interpret-kleio-xml-diff/scripts/diff_xml_groups.py <file1.xml> <file2.xml>
```

Parse a whole diff report:

```bash
python .qoder/skills/interpret-kleio-xml-diff/scripts/diff_xml_groups.py --diff <report.diff>
```

Show every individual difference (disable aggregation):

```bash
python .qoder/skills/interpret-kleio-xml-diff/scripts/diff_xml_groups.py --diff <report.diff> --no-aggregate
```

## Output format

The script produces Markdown sections, one per XML file pair. Each section lists structural patterns with a count and a side-by-side table showing the changed element values from each file:

```markdown
### attribute group `ls` — changed elements: data, date, tipo, type, valor, value (×607)

| SIDE | GROUP ID | NAME | CLASS | LEVEL | LINE | ELEMENT | VALUE |
|---|---|---|---|---|---|---|---|
| A | deh-adam-algenler-att118-175 | ls | attribute | 4 | 160 | data | 16331014 |
| B | deh-adam-algenler-att118-175 | ls | attribute | 4 | 160 | data | |
| A | deh-adam-algenler-att118-175 | ls | attribute | 4 | 160 | date | |
| B | deh-adam-algenler-att118-175 | ls | attribute | 4 | 160 | date | 16331014 |
| A | deh-adam-algenler-att118-175 | ls | attribute | 4 | 160 | tipo | nascimento |
| B | deh-adam-algenler-att118-175 | ls | attribute | 4 | 160 | type | nascimento |

... and 606 more group(s) with the same structural pattern.
```

For patterns that occur only once, the full A/B rows are shown without the "and N more" note.

## Steps

1. **Identify inputs**
   - If user provides two XML paths, use them directly.
   - If user provides a diff report path, extract XML file pairs from lines matching `diff -r -b <path1>.xml <path2>.xml`.

2. **Run the helper script**
   - Default (aggregated): `python .qoder/skills/interpret-kleio-xml-diff/scripts/diff_xml_groups.py --diff <report.diff>`
   - Full detail: add `--no-aggregate`
   - If the script fails or Python is unavailable, fall back to manual parsing using XML tools.

3. **Return the Markdown output**
   - One section per XML file pair.
   - Each section lists patterns, counts, and representative A/B rows.

## Aggregation rules

A pattern is defined by the structural shape of a GROUP difference:
- Group NAME (file A and file B)
- Group CLASS (file A and file B)
- Changed ELEMENT names (file A and file B)

Specific element values are **not** part of the pattern key, so all groups with the same structural change collapse into one entry. The representative example shows the values from the first occurrence, and the count tells how many groups share that structure.

## Scope

- Only process XML sections. Ignore `.cli`, `.err`, `.rpt`, `.files.json`, and other diffs unless explicitly requested.
- Focus on top-level `<GROUP>` elements inside the Kleio XML output.
