---
name: xml-diff
description: Compare XML files structurally between reference_translations and test_translations directories
---

# XML Structural Diff Skill

Compares XML files from `tests/kleio-home/sources/reference_translations` with corresponding files in `tests/kleio-home/sources/test_translations` using structural comparison (elements, attributes, and values) rather than text-based diff.

## Usage

```bash
/xml-diff [pattern] [--detailed|-d]
```

- `pattern`: Optional file pattern to limit comparison (e.g., `linked_data/*.xml`)
- `--detailed` or `-d`: Show full list of differences instead of summary

## Examples

```bash
# Compare all XML files
/xml-diff

# Compare specific file pattern
/xml-diff "linked_data/*.xml"

# Show detailed differences for a specific file
/xml-diff "linked_data/dehergne-a.xml" --detailed
```

## What it does

1. Finds matching XML files in both directories (same relative paths)
2. Parses each XML file using lxml for robust XML handling
3. Compares structure and content:
   - Element tags and hierarchy
   - Attributes (excluding metadata like STRUCTURE, SOURCE, WHEN)
   - Text and CDATA content
   - Child element counts
4. Generates a summary report with match rate

## Output

The summary includes:
- Total files compared
- Number of identical/different files
- Match rate percentage
- For each different file:
  - Element counts
  - Number of differences
  - Difference types (attribute, text content, structure)
- Detailed view (with `--detailed`): Shows actual differences

## Notes

- Differences in `STRUCTURE`, `SOURCE`, `WHEN`, and `TRANSLATOR` attributes are filtered out
- Content of `kleiofile` elements is ignored (contains file paths)
- Whitespace-only text nodes are normalized
- Exit code: 0 if all files match, 1 if differences found

## Implementation

The skill is implemented as a Python script (`xml-diff.py`) using lxml for XML parsing.
