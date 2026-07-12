#!/usr/bin/env python3
"""
XML Structural Diff Tool

Compares XML files between reference_translations and test_translations
using structural comparison rather than text-based diff.
"""

import os
import sys
from pathlib import Path
from typing import Dict, List, Tuple, Any, Optional
from dataclasses import dataclass, field
from collections import defaultdict

try:
    from lxml import etree
except ImportError:
    print("Error: lxml is required. Install with: pip install lxml")
    sys.exit(1)


# Attributes that are expected to differ and should be ignored
IGNORED_ATTRIBUTES = {
    'STRUCTURE',  # Different file paths for structure files
    'SOURCE',     # Different source file paths
    'WHEN',       # Timestamps differ
    'TRANSLATOR', # May differ
}

# Elements whose content should be ignored if they contain file paths
IGNORE_CONTENT_ELEMENTS = {
    'kleiofile',  # Contains file paths
}


@dataclass
class DiffResult:
    """Stores comparison results for a single file."""
    file_path: str
    status: str  # 'identical', 'different', 'missing_ref', 'missing_test'
    differences: List[str] = field(default_factory=list)
    element_count_ref: int = 0
    element_count_test: int = 0


@dataclass
class Summary:
    """Stores overall comparison summary."""
    total_compared: int = 0
    identical: int = 0
    different: int = 0
    missing_ref: int = 0
    missing_test: int = 0
    errors: int = 0
    results: List['DiffResult'] = field(default_factory=list)


def normalize_text(text: Optional[str]) -> str:
    """Normalize text for comparison."""
    if text is None:
        return ""
    return " ".join(text.split()).strip()


def get_element_path(element: etree._Element) -> str:
    """Get XPath-like path for an element."""
    parts = []
    current = element
    while current is not None:
        tag = current.tag
        # Add position if there are siblings with same tag
        siblings = [s for s in current.itersiblings(preceding=True) if s.tag == tag]
        if siblings:
            tag = f"{tag}[{len(siblings) + 1}]"
        parts.append(tag)
        current = current.getparent()
        if current is None:
            break
    return "/" + "/".join(reversed(parts))


def compare_elements(ref_elem: etree._Element, test_elem: etree._Element, path: str = "") -> List[str]:
    """Compare two XML elements and return list of differences."""
    differences = []

    # Compare tag
    if ref_elem.tag != test_elem.tag:
        differences.append(f"Tag mismatch at {path}: '{ref_elem.tag}' vs '{test_elem.tag}'")

    # Compare attributes (filter out ignored ones)
    ref_attrs = set(ref_elem.attrib.keys()) - IGNORED_ATTRIBUTES
    test_attrs = set(test_elem.attrib.keys()) - IGNORED_ATTRIBUTES

    for attr in ref_attrs - test_attrs:
        differences.append(f"Missing attribute '{attr}' at {path}")

    for attr in test_attrs - ref_attrs:
        differences.append(f"Extra attribute '{attr}' at {path}")

    for attr in ref_attrs & test_attrs:
        ref_val = normalize_text(ref_elem.get(attr))
        test_val = normalize_text(test_elem.get(attr))
        if ref_val != test_val:
            differences.append(f"Attribute '{attr}' differs at {path}: '{ref_val[:50]}' vs '{test_val[:50]}'")

    # Compare text content (skip for elements that contain file paths)
    if ref_elem.tag not in IGNORE_CONTENT_ELEMENTS:
        ref_text = normalize_text(ref_elem.text)
        test_text = normalize_text(test_elem.text)
        if ref_text != test_text:
            differences.append(f"Text content differs at {path}: '{ref_text[:50]}' vs '{test_text[:50]}'")

    # Compare tail content
    ref_tail = normalize_text(ref_elem.tail)
    test_tail = normalize_text(test_elem.tail)
    if ref_tail != test_tail:
        differences.append(f"Tail content differs at {path}: '{ref_tail[:50]}' vs '{test_tail[:50]}'")

    # Compare children
    ref_children = [e for e in ref_elem if isinstance(e.tag, str)]
    test_children = [e for e in test_elem if isinstance(e.tag, str)]

    if len(ref_children) != len(test_children):
        differences.append(
            f"Child count differs at {path}: {len(ref_children)} vs {len(test_children)}"
        )

    # Compare children element by element
    max_children = max(len(ref_children), len(test_children))
    for i in range(max_children):
        child_path = f"{path}/{ref_elem.tag if i < len(ref_children) else test_elem.tag}[{i + 1}]"

        if i >= len(ref_children):
            differences.append(f"Extra child element at {child_path}")
        elif i >= len(test_children):
            differences.append(f"Missing child element at {child_path}")
        else:
            differences.extend(
                compare_elements(ref_children[i], test_children[i], child_path)
            )

    return differences


def count_elements(element: etree._Element) -> int:
    """Count total number of elements in XML tree."""
    count = 1
    for child in element:
        if isinstance(child.tag, str):
            count += count_elements(child)
    return count


def parse_xml_file(file_path: Path) -> Optional[etree._ElementTree]:
    """Parse XML file and return root element."""
    try:
        tree = etree.parse(str(file_path))
        return tree
    except Exception as e:
        print(f"Warning: Could not parse {file_path}: {e}", file=sys.stderr)
        return None


def find_matching_files(ref_dir: Path, test_dir: Path, pattern: str = "*.xml") -> List[Tuple[Path, Path]]:
    """Find matching XML files in both directories."""
    matches = []

    for ref_file in ref_dir.rglob(pattern):
        rel_path = ref_file.relative_to(ref_dir)
        test_file = test_dir / rel_path

        if test_file.exists():
            matches.append((ref_file, test_file))

    return matches


def compare_xml_files(ref_file: Path, test_file: Path, base_dir: Path) -> DiffResult:
    """Compare two XML files and return differences."""
    result = DiffResult(
        file_path=str(test_file.relative_to(base_dir)),
        status="identical"
    )

    ref_tree = parse_xml_file(ref_file)
    test_tree = parse_xml_file(test_file)

    if ref_tree is None:
        result.status = "error_ref"
        result.differences.append("Could not parse reference file")
        return result

    if test_tree is None:
        result.status = "error_test"
        result.differences.append("Could not parse test file")
        return result

    result.element_count_ref = count_elements(ref_tree.getroot())
    result.element_count_test = count_elements(test_tree.getroot())

    # Compare root elements
    differences = compare_elements(
        ref_tree.getroot(),
        test_tree.getroot(),
        ref_tree.getroot().tag
    )

    if differences:
        result.status = "different"
        result.differences = differences

    return result


def print_summary(summary: Summary, detailed: bool = False):
    """Print comparison summary."""
    print("\n" + "=" * 70)
    print("XML STRUCTURAL COMPARISON SUMMARY")
    print("=" * 70)

    print(f"\nFiles compared: {summary.total_compared}")
    print(f"  Identical:     {summary.identical}")
    print(f"  Different:     {summary.different}")
    print(f"  Errors:        {summary.errors}")

    if summary.total_compared > 0:
        match_rate = (summary.identical / summary.total_compared) * 100
        print(f"  Match rate:    {match_rate:.1f}%")

    print("\n" + "-" * 70)

    # Group results by status
    by_status = defaultdict(list)
    for result in summary.results:
        by_status[result.status].append(result)

    if by_status.get("different"):
        print("\nFILES WITH DIFFERENCES:")
        for result in by_status["different"]:
            # Show cleaner path - remove common prefix
            clean_path = result.file_path
            if clean_path.startswith("test_translations/"):
                clean_path = clean_path.replace("test_translations/", "")
            print(f"\n  {clean_path}")
            print(f"    Elements: {result.element_count_ref} -> {result.element_count_test}")
            print(f"    Differences: {len(result.differences)}")

            if detailed and result.differences:
                # Show up to 20 differences in detailed mode
                for diff in result.differences[:20]:
                    print(f"      - {diff}")
                if len(result.differences) > 20:
                    print(f"      ... and {len(result.differences) - 20} more")
            elif result.differences:
                # In non-detailed mode, show a sample of difference types
                seen_types = set()
                for diff in result.differences[:10]:
                    # Extract difference type
                    if "Tag mismatch" in diff:
                        seen_types.add("Tag mismatches")
                    elif "Missing attribute" in diff:
                        seen_types.add("Missing attributes")
                    elif "Extra attribute" in diff:
                        seen_types.add("Extra attributes")
                    elif "Attribute" in diff:
                        seen_types.add("Attribute value differences")
                    elif "Text content" in diff:
                        seen_types.add("Text content differences")
                    elif "Child count" in diff:
                        seen_types.add("Child count differences")
                    elif "Extra child" in diff:
                        seen_types.add("Extra child elements")
                    elif "Missing child" in diff:
                        seen_types.add("Missing child elements")

                if seen_types:
                    print(f"    Difference types: {', '.join(sorted(seen_types))}")

    if by_status.get("error_ref") or by_status.get("error_test"):
        print("\nFILES WITH PARSING ERRORS:")
        for result in by_status.get("error_ref", []):
            print(f"  ERROR (ref):  {result.file_path}")
        for result in by_status.get("error_test", []):
            print(f"  ERROR (test): {result.file_path}")

    print("\n" + "=" * 70)
    print("\nNote: Differences in STRUCTURE, SOURCE, WHEN attributes are filtered out.")
    print("Use 'xml-diff --detailed' to see all differences.")
    print("=" * 70 + "\n")


def find_extra_files(ref_dir: Path, test_dir: Path) -> Tuple[List[Path], List[Path]]:
    """Find files that exist in only one directory."""
    ref_files = set(f.relative_to(ref_dir) for f in ref_dir.rglob("*.xml"))
    test_files = set(f.relative_to(test_dir) for f in test_dir.rglob("*.xml"))

    only_in_ref = [ref_dir / p for p in ref_files - test_files]
    only_in_test = [test_dir / p for p in test_files - ref_files]

    return only_in_ref, only_in_test


def main():
    """Main entry point."""
    # Parse arguments
    detailed = "--detailed" in sys.argv or "-d" in sys.argv

    # Get pattern from arguments (skip flag arguments)
    args = [a for a in sys.argv[1:] if not a.startswith("--") and not a.startswith("-")]
    pattern = args[0] if args else "*.xml"

    # Get base directory - assume we're running from project root
    cwd = Path.cwd()
    if (cwd / "tests" / "kleio-home" / "sources").exists():
        base_dir = cwd / "tests" / "kleio-home" / "sources"
    else:
        # If running from script location
        script_dir = Path(__file__).parent.absolute()
        project_dir = script_dir.parents[2]  # Go up to project root
        base_dir = project_dir / "tests" / "kleio-home" / "sources"

    ref_dir = base_dir / "reference_translations"
    test_dir = base_dir / "test_translations"

    print(f"Comparing XML files:")
    print(f"  Reference: {ref_dir}")
    print(f"  Test:      {test_dir}")
    print(f"  Pattern:   {pattern}")

    if not ref_dir.exists():
        print(f"Error: Reference directory not found: {ref_dir}", file=sys.stderr)
        sys.exit(1)

    if not test_dir.exists():
        print(f"Error: Test directory not found: {test_dir}", file=sys.stderr)
        sys.exit(1)

    summary = Summary()

    # Find and compare matching files
    matches = find_matching_files(ref_dir, test_dir, pattern)
    summary.total_compared = len(matches)

    for ref_file, test_file in matches:
        result = compare_xml_files(ref_file, test_file, test_dir)
        summary.results.append(result)

        if result.status == "identical":
            summary.identical += 1
        elif result.status == "different":
            summary.different += 1
        elif result.status.startswith("error"):
            summary.errors += 1

    # Find files only in one directory
    only_in_ref, only_in_test = find_extra_files(ref_dir, test_dir)

    if only_in_ref:
        print(f"\nFiles only in reference ({len(only_in_ref)}):")
        for f in sorted(only_in_ref)[:10]:  # Show first 10
            print(f"  {f.relative_to(ref_dir)}")
        if len(only_in_ref) > 10:
            print(f"  ... and {len(only_in_ref) - 10} more")

    if only_in_test:
        print(f"\nFiles only in test ({len(only_in_test)}):")
        for f in sorted(only_in_test)[:10]:  # Show first 10
            print(f"  {f.relative_to(test_dir)}")
        if len(only_in_test) > 10:
            print(f"  ... and {len(only_in_test) - 10} more")

    # Print summary
    print_summary(summary, detailed=detailed)

    # Exit with error code if there are differences
    sys.exit(0 if summary.different == 0 and summary.errors == 0 else 1)


if __name__ == "__main__":
    main()
