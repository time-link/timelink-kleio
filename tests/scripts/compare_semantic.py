#!/usr/bin/env python3
"""Semantic comparison of Python vs Prolog Kleio translations.

This is the structural counterpart to ``compare_test_results_python.sh``.
Where the shell script does a textual ``diff`` (which is noisy because the
Python XML exporter's serialization differs from the Prolog one in many
cosmetic ways), this script parses **both** sides into a common
group-tree representation and compares the semantically meaningful
fields: group names, ids, parent/child nesting, and core element values.

It assumes the translation outputs are already on disk:

* Prolog reference : ``kleio-home/sources/reference_translations/<rel>.xml``
* Python output    : ``kleio-home/sources/test_translations/<rel>.xml``

(produced by ``prepare_tests.sh`` + ``kleio_translate_python.sh``).

Run::

    cd develop/tests
    python scripts/compare_semantic.py

Writes:
    reports/semantic_report_<timestamp>.txt        - full per-file detail
    reports/semantic_report_<timestamp>.summary.txt - one-line-per-file + categories
    reports/latest_semantic.summary.txt             - symlink to the latest summary

Options:
    --subdir PATH     only compare a subtree (e.g. varia/, paroquiais/baptismos/)
    --max-diffs N     cap per-file diff lines in the detail report (default 25)
"""
from __future__ import annotations

import argparse
import datetime
import re
import sys
import xml.etree.ElementTree as ET
from collections import Counter
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

# This script lives at develop/tests/scripts/compare_semantic.py.
TESTS_DIR = Path(__file__).resolve().parents[1]
KLEIO_HOME = TESTS_DIR / "kleio-home"
REFERENCE_SOURCES = KLEIO_HOME / "sources" / "reference_sources"
REFERENCE_TRANSLATIONS = KLEIO_HOME / "sources" / "reference_translations"
TEST_TRANSLATIONS = KLEIO_HOME / "sources" / "test_translations"
REPORTS_DIR = TESTS_DIR / "reports"


# ---------------------------------------------------------------------------
# Common group-tree representation
# ---------------------------------------------------------------------------

@dataclass
class GroupNode:
    """A minimal, format-agnostic representation of a group for comparison."""
    id: str
    name: str
    cls: str
    # core element values, keyed by element name
    elements: dict[str, str] = field(default_factory=dict)
    children: list["GroupNode"] = field(default_factory=list)


def _parse_xml(xml_path: Path) -> Optional[GroupNode]:
    """Parse an XML file (Prolog or Python) into a GroupNode tree.

    The Prolog exporter writes a **flat** list of ``<GROUP>`` elements
    under ``<KLEIO>`` (each preceded by a ``<CLASS>`` definition block),
    with nesting encoded via the ``LEVEL`` attribute — NOT via XML element
    nesting. The Python exporter nests ``<GROUP>`` elements hierarchically.

    This function normalises both formats into a common nested tree by:
      1. Collecting all ``<GROUP>`` elements (skipping ``<CLASS>`` blocks).
      2. If any ``<GROUP>`` has a ``LEVEL`` attribute and contains no
         ``<GROUP>`` child (the Prolog flat format), rebuild the tree
         from the flat list using ``LEVEL`` (a group at level N is the
         child of the most recent group at level N-1).
      3. Otherwise (Python nested format) parse recursively as before.

    Returns None if the file is missing or unparseable.
    """
    if not xml_path.exists():
        return None
    try:
        tree = ET.parse(xml_path)
    except ET.ParseError:
        return None
    root = tree.getroot()

    # Collect all top-level GROUP elements (skip CLASS blocks).
    groups = [c for c in root if c.tag == "GROUP"]
    if not groups:
        return None

    # Detect format: Prolog flat (GROUPs have LEVEL, no nested GROUPs)
    # vs Python nested.
    has_level = all(g.get("LEVEL") for g in groups)
    has_nested = any(g.find("GROUP") is not None for g in groups)

    if has_level and not has_nested:
        # Prolog flat format: rebuild the tree from LEVEL.
        return _flat_to_tree(groups)

    # Python nested format. Skip the kleio$ wrapper group (Prolog starts
    # at the first content group, typically fonte$).
    first = groups[0]
    node = _group_elem_to_node(first)
    if node.name == "kleio" and node.children:
        node = node.children[0]
    return node


def _flat_to_tree(groups: list[ET.Element]) -> Optional[GroupNode]:
    """Build a nested GroupNode tree from a flat list of <GROUP> elements
    using their LEVEL attribute. Mirrors the Prolog XML's logical nesting.
    """
    if not groups:
        return None
    # Build each node without children first.
    nodes: list[tuple[int, GroupNode]] = []
    for g in groups:
        level = int(g.get("LEVEL", "1"))
        node = _group_elem_to_node(g, include_subgroups=False)
        nodes.append((level, node))

    # Re-nest: each node's parent is the most recent node with a smaller level.
    root = nodes[0][1]
    stack: list[tuple[int, GroupNode]] = [nodes[0]]
    for level, node in nodes[1:]:
        while stack and stack[-1][0] >= level:
            stack.pop()
        if stack:
            stack[-1][1].children.append(node)
        stack.append((level, node))
    return root


def _group_elem_to_node(elem: ET.Element, include_subgroups: bool = True) -> GroupNode:
    node = GroupNode(
        id=elem.get("ID", ""),
        name=elem.get("NAME", ""),
        cls=elem.get("CLASS", ""),
    )
    for child_elem in elem:
        if child_elem.tag == "GROUP" and include_subgroups:
            node.children.append(_group_elem_to_node(child_elem))
        elif child_elem.tag == "ELEMENT":
            name = child_elem.get("NAME", "")
            core_elem = child_elem.find("core")
            if core_elem is None:
                core_elem = child_elem.find("CORE")
            value = core_elem.text.strip() if core_elem is not None and core_elem.text else ""
            if name:
                node.elements[name] = value
    return node


# ---------------------------------------------------------------------------
# Comparison
# ---------------------------------------------------------------------------

# Elements whose values are unstable across runs (counters, timestamps,
# path-dependent strings) or are synthetic metadata that the Prolog
# exporter adds but the Python parser doesn't (groupname, class, inside,
# kleiofile, etc.). These are exporter concerns, not parser-correctness
# concerns, so we ignore them in the semantic comparison.
UNSTABLE_ELEMENTS = {
    "order", "level", "line", "id",
    # Synthetic metadata elements added by the Prolog exporter:
    "groupname", "class", "inside", "kleiofile", "group",
}


def _normalize_id(id_val: str) -> str:
    """Strip auto-generated suffixes that differ between Python and Prolog
    (attribute/relation counters, translation-count suffixes)."""
    return re.sub(r"-(?:att|rel|per|his|kle)\d+(?:-\d+)?$", "", id_val)


def compare_trees(prolog: Optional[GroupNode], python: Optional[GroupNode]) -> list[str]:
    """Compare two GroupNode trees, returning a list of difference
    descriptions (empty if they're semantically equivalent)."""
    diffs: list[str] = []
    if prolog is None:
        return ["prolog reference missing or unparseable"]
    if python is None:
        return ["python translation produced no XML"]
    _compare_node(prolog, python, [], diffs)
    return diffs


def _compare_node(p: GroupNode, y: GroupNode, path: list[str], diffs: list[str]) -> None:
    here = "/".join(path + [p.name])

    if p.name != y.name:
        diffs.append(f"{here}: group name {p.name!r} != {y.name!r}")
    if _normalize_id(p.id) != _normalize_id(y.id):
        diffs.append(f"{here}: id {p.id!r} != {y.id!r} (normalized)")

    p_elems = {k: v for k, v in p.elements.items() if k not in UNSTABLE_ELEMENTS}
    y_elems = {k: v for k, v in y.elements.items() if k not in UNSTABLE_ELEMENTS}
    for name in p_elems.keys() | y_elems.keys():
        if p_elems.get(name) != y_elems.get(name):
            diffs.append(
                f"{here}: element {name!r} {p_elems.get(name)!r} != {y_elems.get(name)!r}"
            )

    if len(p.children) != len(y.children):
        diffs.append(f"{here}: child count {len(p.children)} != {len(y.children)}")
    for i, (pc, yc) in enumerate(zip(p.children, y.children)):
        _compare_node(pc, yc, path + [f"[{i}]"], diffs)


def _categorize(diff: str) -> str:
    if "child count" in diff:
        return "child-count"
    if "group name" in diff:
        return "group-name"
    if ": id " in diff:
        return "id-mismatch"
    if "element " in diff:
        m = re.search(r"element '([^']*)'", diff)
        return f"element:{m.group(1)}" if m else "element:other"
    if "prolog reference missing" in diff:
        return "no-prolog-reference"
    if "no XML" in diff:
        return "empty-python-output"
    return "other"


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--subdir", default="",
                        help="only compare a subtree (e.g. varia/)")
    parser.add_argument("--max-diffs", type=int, default=25,
                        help="cap per-file diff lines in the detail report")
    parser.add_argument("--reference-translations", default=str(REFERENCE_TRANSLATIONS),
                        help=f"reference translations dir (default: {REFERENCE_TRANSLATIONS})")
    parser.add_argument("--test-translations", default=str(TEST_TRANSLATIONS),
                        help=f"test translations dir (default: {TEST_TRANSLATIONS})")
    args = parser.parse_args()

    ref_dir = Path(args.reference_translations)
    test_dir = Path(args.test_translations)

    if not ref_dir.exists():
        print(f"ERROR: reference translations dir not found: {ref_dir}", file=sys.stderr)
        return 1
    if not test_dir.exists():
        print(f"ERROR: test translations dir not found: {test_dir}", file=sys.stderr)
        return 1

    # Enumerate the reference XML files (the comparison is XML-only; the
    # other outputs .err/.rpt/.files.json are for human inspection).
    xml_files = sorted(ref_dir.rglob("*.xml"))
    if args.subdir:
        xml_files = [f for f in xml_files if args.subdir in str(f.relative_to(ref_dir))]

    timestamp = datetime.datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    REPORTS_DIR.mkdir(parents=True, exist_ok=True)
    detail_path = REPORTS_DIR / f"semantic_report_{timestamp}.txt"
    summary_path = REPORTS_DIR / f"semantic_report_{timestamp}.summary.txt"

    per_file_status: list[tuple[str, int]] = []
    category_counter: Counter[str] = Counter()
    total = 0
    passing = 0
    skipped = 0

    detail_lines: list[str] = [
        "Kleio Python vs Prolog semantic comparison (structural)",
        f"Generated: {datetime.datetime.now().isoformat()}",
        f"Reference (Prolog): {ref_dir}",
        f"Test (Python)     : {test_dir}",
        f"Files considered  : {len(xml_files)}",
        "=" * 78,
    ]

    for ref_xml in xml_files:
        rel = str(ref_xml.relative_to(ref_dir))
        py_xml = test_dir / rel
        total += 1

        prolog_tree = _parse_xml(ref_xml)
        python_tree = _parse_xml(py_xml) if py_xml.exists() else None

        if python_tree is None and not py_xml.exists():
            skipped += 1
            per_file_status.append((rel, -1))
            detail_lines.append(f"\n[SKIP] {rel} — no Python XML at {py_xml.name}")
            continue

        diffs = compare_trees(prolog_tree, python_tree)
        per_file_status.append((rel, len(diffs)))
        if not diffs:
            passing += 1
            detail_lines.append(f"\n[PASS] {rel}")
            continue

        detail_lines.append(f"\n[FAIL] {rel} — {len(diffs)} difference(s)")
        for d in diffs[:args.max_diffs]:
            detail_lines.append(f"    - {d}")
            category_counter[_categorize(d)] += 1
        if len(diffs) > args.max_diffs:
            detail_lines.append(f"    ... and {len(diffs) - args.max_diffs} more")
            for d in diffs[args.max_diffs:]:
                category_counter[_categorize(d)] += 1

    failing = total - passing - skipped

    # ---- Summary ----
    def _sort_key(item):
        rel, count = item
        if count < 0:
            return (2, 0, rel)
        if count == 0:
            return (1, 0, rel)
        return (0, -count, rel)

    summary_lines = [
        "Kleio Python vs Prolog semantic comparison — summary",
        f"Generated: {datetime.datetime.now().isoformat()}",
        "=" * 78,
        f"Total reference files : {total}",
        f"Passing (zero diffs)  : {passing}",
        f"Failing               : {failing}",
        f"Skipped (no Py XML)   : {skipped}",
        "",
        "Per-file status (diff count; 0 = pass, -1 = skipped):",
    ]
    for rel, count in sorted(per_file_status, key=_sort_key):
        marker = "PASS" if count == 0 else ("SKIP" if count < 0 else "FAIL")
        count_str = str(count) if count >= 0 else "-"
        summary_lines.append(f"  [{marker:4}] {count_str:>5}  {rel}")
    summary_lines.append("")
    summary_lines.append("Difference categories (most common first):")
    for cat, n in category_counter.most_common():
        summary_lines.append(f"  {n:>6}  {cat}")
    summary_lines.append("")

    detail_path.write_text("\n".join(detail_lines) + "\n", encoding="utf-8")
    summary_path.write_text("\n".join(summary_lines) + "\n", encoding="utf-8")

    # latest_*.summary.txt symlink for convenience.
    latest = REPORTS_DIR / "latest_semantic.summary.txt"
    try:
        latest.unlink()
    except FileNotFoundError:
        pass
    latest.symlink_to(summary_path.name)

    print(f"Wrote detail  : {detail_path}")
    print(f"Wrote summary : {summary_path}")
    print(f"Symlink       : {latest}")
    print()
    for line in summary_lines:
        print(line)
    return 0 if failing == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
