#!/usr/bin/env python3
"""Analyze a Kleio semantic test report and print a structured summary.

Usage:
    python3 generate_report.py [--diff <report.diff>] [--json]

If --diff is omitted, uses the latest test_report_*.diff in tests/reports/.
Output is plain text suitable for inclusion in a Markdown report.
With --json, outputs JSON instead.
"""
import argparse
import glob
import json
import os
import re
import sys
from collections import Counter, defaultdict
from pathlib import Path


def find_latest_report(reports_dir="tests/reports"):
    reports = sorted(glob.glob(os.path.join(reports_dir, "test_report_*.diff")))
    if not reports:
        return None
    return reports[-1]


def count_errors(translations_dir="tests/kleio-home/sources/test_translations",
                 reference_dir="tests/kleio-home/sources/reference_translations"):
    """Count errors per file for dev and stable."""
    dev_errors = {}
    stable_errors = {}
    for base, target in [(translations_dir, dev_errors), (reference_dir, stable_errors)]:
        for f in glob.glob(os.path.join(base, "**", "*.files.json"), recursive=True):
            try:
                d = json.load(open(f))
                e = d.get("errors", 0)
                rel = os.path.relpath(f, base).replace("/.files.json", "").replace("\\", "/")
                if e != 0:
                    target[rel] = e
            except Exception:
                pass
    return dev_errors, stable_errors


def categorize_errors(translations_dir="tests/kleio-home/sources/test_translations"):
    """Categorize ERROR: lines from .rpt files."""
    categories = Counter()
    for f in glob.glob(os.path.join(translations_dir, "**", "*.rpt"), recursive=True):
        try:
            for line in open(f):
                if line.startswith("ERROR:"):
                    # Normalize: strip paths, line numbers, IDs, numbers
                    norm = re.sub(r"line \d+", "line N", line)
                    norm = re.sub(r"\([^)]*\)", "", norm)
                    norm = re.sub(r"\d+", "N", norm)
                    norm = norm.strip()
                    categories[norm] += 1
        except Exception:
            pass
    return categories


def diff_by_area(report_path):
    """Break down diff lines by source area."""
    txt = open(report_path).read()
    areas = defaultdict(int)
    blocks = re.split(r"^(diff -r.*\.xml\s+\S+\.xml)$", txt, flags=re.M)
    for i in range(1, len(blocks), 2):
        m = re.search(r"reference_translations/([^/]+)/", blocks[i])
        area = m.group(1) if m else "other"
        body = blocks[i + 1] if i + 1 < len(blocks) else ""
        areas[area] += len(re.findall(r"^[<>]", body, re.M))
    return areas


def top_diff_files(report_path, n=15):
    """Find files with the most diff lines."""
    txt = open(report_path).read()
    blocks = re.split(r"^(diff -r.*\.xml\s+\S+\.xml)$", txt, flags=re.M)
    results = []
    for i in range(1, len(blocks), 2):
        m = re.search(r"/([^/]+\.xml)\s", blocks[i])
        fname = m.group(1) if m else blocks[i][:60]
        body = blocks[i + 1] if i + 1 < len(blocks) else ""
        diffs = len(re.findall(r"^[<>]", body, re.M))
        if diffs > 0:
            results.append((diffs, fname))
    results.sort(reverse=True)
    return results[:n]


def main():
    ap = argparse.ArgumentParser(description="Analyze Kleio semantic test report")
    ap.add_argument("--diff", "-d", help="Path to test_report_*.diff")
    ap.add_argument("--json", action="store_true", help="Output JSON")
    args = ap.parse_args()

    report = args.diff or find_latest_report()
    if not report:
        print("No test report found in tests/reports/", file=sys.stderr)
        sys.exit(1)

    total_lines = sum(1 for _ in open(report))
    dev_err, stable_err = count_errors()
    categories = categorize_errors()
    areas = diff_by_area(report)
    top_files = top_diff_files(report)

    data = {
        "report": report,
        "diff_lines": total_lines,
        "dev_error_files": len(dev_err),
        "stable_error_files": len(stable_err),
        "dev_errors": dev_err,
        "stable_errors": stable_err,
        "error_categories": dict(categories.most_common(20)),
        "diff_by_area": dict(sorted(areas.items(), key=lambda x: -x[1])),
        "top_diff_files": [{"file": f, "lines": d} for d, f in top_files],
    }

    if args.json:
        print(json.dumps(data, indent=2))
        return

    # Human-readable output
    print(f"Report: {report}")
    print(f"Diff lines: {total_lines}")
    print()
    print("=== Files with errors (dev) ===")
    for f, e in sorted(dev_err.items()):
        s = stable_err.get(f, 0)
        marker = "REGRESSION" if s == 0 and e > 0 else ""
        print(f"  {f}: dev={e} stable={s} {marker}")
    print()
    print("=== Error categories (top 15) ===")
    for cat, count in categories.most_common(15):
        print(f"  {count:3d}x  {cat[:100]}")
    print()
    print("=== Diff by area ===")
    for area, lines in sorted(areas.items(), key=lambda x: -x[1]):
        print(f"  {area}: {lines}")
    print()
    print(f"=== Top {len(top_files)} diff files ===")
    for d, f in top_files:
        print(f"  {f}: {d}")


if __name__ == "__main__":
    main()
