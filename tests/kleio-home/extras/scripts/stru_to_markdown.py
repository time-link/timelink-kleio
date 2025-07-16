import yaml
# flake8: noqa: E501
"""
This script parses a Kleio groups YAML file and generates Markdown documentation for the defined groups.

Features:
- Reads one or more YAML documents from the input file.
- Extracts file-level metadata and group definitions.
- Formats group references as Markdown links.
- Handles multiline descriptions and lists.
- Outputs a Markdown file with a header, group index, and detailed group definitions.

Usage:
    python stru_to_markdown.py <input_yaml> [output_md]

Arguments:
    input_yaml   Path to the input YAML file (e.g., groups.yaml).
    output_md    (Optional) Path to the output Markdown file (default: GROUPS_DOC.md).

Functions:
    format_value(key, value, all_group_names)
        Formats a value for Markdown output, creating links for group references and handling lists and multiline strings.

    generate_markdown(yaml_file_path, md_file_path)
        Parses the YAML file and writes the formatted Markdown documentation to the specified output file.

Example:
    python stru_to_markdown.py kleio_groups.yaml GROUPS_DOC.md
"""
import argparse
import textwrap


def format_value(key, value, all_group_names):
    """Formats the value for a given key, creating links where appropriate."""
    if value is None:
        return ""

    # For keys that reference other groups by name
    if key in ["source"]:
        if value in all_group_names:
            return f"[`{value}`](#{value})"
        else:
            return f"`{value}`"

    # For keys that contain a list of other groups
    if key in ["part", "arbitrary"]:
        links = []
        for item in value:
            item_clean = item.strip(",")  # Clean up potential trailing commas from YAML
            if item_clean in all_group_names:
                links.append(f"[`{item_clean}`](#{item_clean})")
            else:
                links.append(f"`{item_clean}`")
        return ", ".join(links)

    # For other list values
    if isinstance(value, list):
        return f"`{value}`"

    # For multiline string values (like description)
    if isinstance(value, str) and "\n" in value:
        # Use textwrap.dedent to handle indentation in YAML multiline strings
        return textwrap.dedent(value)

    # For simple values
    return str(value)


def generate_markdown(yaml_file_path, md_file_path):
    """Parses a Kleio groups YAML file and generates Markdown documentation."""
    try:
        with open(yaml_file_path, "r", encoding="utf-8") as f:
            data = yaml.safe_load_all(f)
            # The file might contain multiple YAML documents
            docs = [doc for doc in data if doc]
    except FileNotFoundError:
        print(f"Error: Input file not found at {yaml_file_path}")
        return
    except yaml.YAMLError as e:
        print(f"Error parsing YAML file: {e}")
        return

    # Extract file description and the list of groups
    file_info = {}
    groups = []
    for doc in docs:
        if "file" in doc:
            file_info = doc["file"]
        elif "group" in doc:
            groups.append(doc["group"])

    all_group_names = [group["name"] for group in groups if "name" in group]

    with open(md_file_path, "w", encoding="utf-8") as f:
        # 1. Write File Header
        if "description" in file_info:
            f.write(f"# {file_info.get('name', 'Kleio Group Definitions')}\n\n")
            f.write(format_value("description", file_info["description"], []))
            f.write("\n\n---\n\n")

        # 2. Write Group Index
        f.write("## Group Index\n\n")
        for name in all_group_names:
            f.write(f"*   [{name}](#{name})\n")
        f.write("\n\n---\n\n")

        # 3. Write Group Definitions
        f.write("## Group Definitions\n\n")
        for group in groups:
            group_name = group.get("name", "Unnamed Group")
            f.write(f"### {group_name}\n\n")

            if "description" in group:
                f.write(f"{format_value('description', group['description'], [])}\n\n")

            # Order of keys for consistent output
            key_order = [
                "source",
                "position",
                "guaranteed",
                "also",
                "part",
                "arbitrary",
                "idprefix",
            ]

            for key in key_order:
                if key in group:
                    formatted = format_value(key, group[key], all_group_names)
                    if formatted:
                        f.write(f"*   **{key.capitalize()}**: {formatted}\n")
            f.write("\n")

    print(f"Markdown documentation successfully generated at {md_file_path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Generate Markdown documentation from a Kleio groups YAML file."
    )
    parser.add_argument(
        "input_yaml", help="Path to the input YAML file (e.g., groups.yaml)"
    )
    parser.add_argument(
        "output_md",
        nargs="?",
        default="GROUPS_DOC.md",
        help="Path to the output Markdown file (default: GROUPS_DOC.md)",
    )
    args = parser.parse_args()

    generate_markdown(args.input_yaml, args.output_md)
