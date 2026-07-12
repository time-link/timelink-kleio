#!/usr/bin/env python3
"""
Compare two Kleio XML files and report top-level GROUP elements that differ.

Usage:
    python diff_xml_groups.py <file1.xml> <file2.xml>
    python diff_xml_groups.py --diff <diff_report.diff>

Output: Markdown tables of GROUPs containing differences, with file A/B rows
showing changed element values. Structurally repeated differences are aggregated.
"""

import argparse
import os
import re
import sys
from collections import defaultdict
from xml.etree import ElementTree as ET


def parse_args():
    parser = argparse.ArgumentParser(
        description="Compare Kleio XML files and report differing GROUP elements."
    )
    parser.add_argument("file1", nargs="?", help="First XML file")
    parser.add_argument("file2", nargs="?", help="Second XML file")
    parser.add_argument(
        "--diff", "-d", help="Path to unified diff report to extract XML file paths"
    )
    parser.add_argument(
        "--no-aggregate",
        action="store_true",
        help="Show every differing GROUP instead of aggregating repeated patterns",
    )
    return parser.parse_args()


def extract_xml_paths_from_diff(diff_path):
    """Extract pairs of XML file paths from a unified diff report."""
    pairs = []
    pattern = re.compile(r"^diff -r -b (.+\.xml) (.+\.xml)$")
    with open(diff_path, "r", encoding="utf-8") as f:
        for line in f:
            m = pattern.match(line.strip())
            if m:
                pairs.append((m.group(1), m.group(2)))
    return pairs


def _resolve_relative_path(rel_path, candidate_bases):
    """Return first existing full path, or last candidate if none exist."""
    for base in candidate_bases:
        full = os.path.join(base, rel_path)
        if os.path.exists(full):
            return full
    return os.path.join(candidate_bases[-1], rel_path)


def group_info(group):
    if group is None:
        return {}
    return {
        "id": group.attrib.get("ID", ""),
        "name": group.attrib.get("NAME", ""),
        "class": group.attrib.get("CLASS", ""),
        "level": group.attrib.get("LEVEL", ""),
        "line": group.attrib.get("LINE", ""),
    }


def element_map(group):
    """Map ELEMENT NAME -> normalized text value for a GROUP."""
    result = {}
    for child in group:
        if child.tag == "ELEMENT":
            name = child.attrib.get("NAME", "")
            text = "".join(child.itertext())
            # Normalize whitespace: collapse runs of whitespace to a single space
            text = re.sub(r"\s+", " ", text).strip()
            result[name] = text
    return result


def normalize_group(group):
    """Return a normalized string representation of a GROUP element."""
    attribs = " ".join(f'{k}="{v}"' for k, v in sorted(group.attrib.items()))
    children = []
    for name, text in sorted(element_map(group).items()):
        children.append(f"{name}={text}")
    return attribs + "|" + "|".join(children)


def compare_groups(groups1, groups2):
    """Compare two dicts of GROUP elements and return list of differences."""
    keys1 = set(groups1.keys())
    keys2 = set(groups2.keys())
    all_keys = sorted(keys1 | keys2)

    differences = []
    for key in all_keys:
        g1 = groups1.get(key)
        g2 = groups2.get(key)

        if g1 is None:
            differences.append((key, None, g2, "only in file 2"))
        elif g2 is None:
            differences.append((key, g1, None, "only in file 1"))
        elif normalize_group(g1) != normalize_group(g2):
            differences.append((key, g1, g2, "modified"))

    return differences


def element_differences(g1, g2):
    """Return list of (element_name, value_a, value_b) for differing elements."""
    m1 = element_map(g1) if g1 is not None else {}
    m2 = element_map(g2) if g2 is not None else {}
    diffs = []
    for name in sorted(set(m1.keys()) | set(m2.keys())):
        v1 = m1.get(name, "")
        v2 = m2.get(name, "")
        if v1 != v2:
            diffs.append((name, v1, v2))
    return diffs


def parse_xml_file(path):
    """Parse XML file and return dict of GROUP elements keyed by ID."""
    tree = ET.parse(path)
    root = tree.getroot()
    groups = {}
    for group in root.findall("GROUP"):
        key = group.attrib.get("ID")
        if key:
            groups[key] = group
    return groups


def _format_value(value, max_len=80):
    """Format a value for display in a Markdown table cell."""
    if len(value) <= max_len:
        return value
    return value[: max_len - 3] + "..."


def build_difference_summaries(differences):
    """Convert raw differences into structured summaries, one per GROUP."""
    summaries = []
    for key, g1, g2, status in differences:
        info1 = group_info(g1)
        info2 = group_info(g2)
        summary = {
            "key": key,
            "name_a": info1.get("name", ""),
            "name_b": info2.get("name", ""),
            "class_a": info1.get("class", ""),
            "class_b": info2.get("class", ""),
            "level_a": info1.get("level", ""),
            "level_b": info2.get("level", ""),
            "line_a": info1.get("line", ""),
            "line_b": info2.get("line", ""),
            "status": status,
            "element_changes": [],
        }
        if status == "modified":
            for elem_name, val_a, val_b in element_differences(g1, g2):
                summary["element_changes"].append((elem_name, elem_name, val_a, val_b))
        else:
            # Group present in only one file
            summary["element_changes"].append(
                ("(group)", "(group)",
                 "present" if g1 is not None else "missing",
                 "present" if g2 is not None else "missing")
            )
        summaries.append(summary)
    return summaries


def structural_pattern_key(summary):
    """Return a key that groups structurally similar GROUP differences."""
    name_pair = (summary["name_a"], summary["name_b"])
    class_pair = (summary["class_a"], summary["class_b"])
    element_shape = frozenset(
        (elem_a, elem_b) for elem_a, elem_b, _, _ in summary["element_changes"]
    )
    return (name_pair, class_pair, element_shape)


def aggregate_summaries(summaries):
    """Aggregate GROUP-level summaries by structural pattern (ignoring values)."""
    patterns = defaultdict(list)
    for summary in summaries:
        patterns[structural_pattern_key(summary)].append(summary)
    return patterns


def _pattern_label(pattern_key, rep):
    """Build a human-readable label for a structural pattern."""
    (name_a, name_b), (class_a, class_b), element_shape = pattern_key
    name = name_a or name_b or "group"
    class_ = class_a or class_b or "unknown"
    changes = sorted(element_shape)
    if changes == [("(group)", "(group)")]:
        return f"{class_} group `{name}` — {rep['status']}"
    change_labels = []
    for elem_a, elem_b in changes:
        if elem_a == elem_b:
            change_labels.append(f"{elem_a}")
        else:
            change_labels.append(f"{elem_a}→{elem_b}")
    return f"{class_} group `{name}` — changed elements: {', '.join(change_labels)}"


def print_aggregated_table(differences, label1="File 1", label2="File 2", full1=None, full2=None):
    if not differences:
        print("No differences found between the XML GROUP elements.")
        return

    summaries = build_difference_summaries(differences)
    patterns = aggregate_summaries(summaries)

    print(f"\n## XML GROUP differences\n")
    print(f"**A:** `{full1 or label1}`")
    print(f"**B:** `{full2 or label2}`\n")

    # Sort patterns by occurrence count descending, then by name/class
    sorted_patterns = sorted(
        patterns.items(),
        key=lambda item: (-len(item[1]), item[0][0], item[0][1]),
    )

    for pattern_key, sums in sorted_patterns:
        count = len(sums)
        rep = sums[0]
        label = _pattern_label(pattern_key, rep)
        print(f"### {label} (×{count})\n")

        print("| SIDE | GROUP ID | NAME | CLASS | LEVEL | LINE | ELEMENT | VALUE |")
        print("|---|---|---|---|---|---|---|---|")

        # Show representative group with all its element changes
        for elem_a, elem_b, val_a, val_b in rep["element_changes"]:
            print(
                f"| A | {rep['key']} | {rep['name_a']} | {rep['class_a']} | "
                f"{rep['level_a']} | {rep['line_a']} | {elem_a} | {_format_value(val_a)} |"
            )
            print(
                f"| B | {rep['key']} | {rep['name_b']} | {rep['class_b']} | "
                f"{rep['level_b']} | {rep['line_b']} | {elem_b} | {_format_value(val_b)} |"
            )

        if count > 1:
            print(f"\n... and {count - 1} more group(s) with the same structural pattern.\n")


def print_full_table(differences, label1="File 1", label2="File 2", full1=None, full2=None):
    if not differences:
        print("No differences found between the XML GROUP elements.")
        return

    summaries = build_difference_summaries(differences)

    print(f"\n## XML GROUP differences\n")
    print(f"**A:** `{full1 or label1}`")
    print(f"**B:** `{full2 or label2}`\n")
    print("| SIDE | GROUP ID | NAME | CLASS | LEVEL | LINE | ELEMENT | VALUE |")
    print("|---|---|---|---|---|---|---|---|")
    for summary in summaries:
        for elem_a, elem_b, val_a, val_b in summary["element_changes"]:
            print(
                f"| A | {summary['key']} | {summary['name_a']} | {summary['class_a']} | "
                f"{summary['level_a']} | {summary['line_a']} | {elem_a} | {_format_value(val_a)} |"
            )
            print(
                f"| B | {summary['key']} | {summary['name_b']} | {summary['class_b']} | "
                f"{summary['level_b']} | {summary['line_b']} | {elem_b} | {_format_value(val_b)} |"
            )


def main():
    args = parse_args()

    if args.diff:
        diff_dir = os.path.dirname(os.path.abspath(args.diff))
        candidate_bases = [diff_dir, os.path.dirname(diff_dir)]
        pairs = extract_xml_paths_from_diff(args.diff)
        if not pairs:
            print("No XML file pairs found in the diff report.", file=sys.stderr)
            sys.exit(1)
        for file1, file2 in pairs:
            full1 = _resolve_relative_path(file1, candidate_bases)
            full2 = _resolve_relative_path(file2, candidate_bases)
            groups1 = parse_xml_file(full1)
            groups2 = parse_xml_file(full2)
            differences = compare_groups(groups1, groups2)
            if args.no_aggregate:
                print_full_table(differences, file1, file2, full1=full1, full2=full2)
            else:
                print_aggregated_table(differences, file1, file2, full1=full1, full2=full2)
        return

    if not args.file1 or not args.file2:
        print("Please provide two XML files or use --diff <report.diff>", file=sys.stderr)
        sys.exit(1)

    full1 = os.path.abspath(args.file1)
    full2 = os.path.abspath(args.file2)
    groups1 = parse_xml_file(full1)
    groups2 = parse_xml_file(full2)
    differences = compare_groups(groups1, groups2)
    if args.no_aggregate:
        print_full_table(differences, args.file1, args.file2, full1=full1, full2=full2)
    else:
        print_aggregated_table(differences, args.file1, args.file2, full1=full1, full2=full2)


if __name__ == "__main__":
    main()
