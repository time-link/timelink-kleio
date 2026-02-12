#!/usr/bin/env python3
import argparse
from ruamel.yaml import YAML
from ruamel.yaml.comments import CommentedMap


def reorder_map(original_map):
    if not isinstance(original_map, dict):
        return original_map

    keys = list(original_map.keys())
    priority = ["name", "description", "source"]

    head = [k for k in priority if k in keys]
    tail = sorted([k for k in keys if k not in priority])
    new_order = head + tail

    # If order is already correct, return original to preserve maximum fidelity
    if keys == new_order:
        return original_map

    new_map = CommentedMap()
    for k in new_order:
        new_map[k] = original_map[k]

    # Copy generic hooks if any (ruamel specific)
    if hasattr(original_map, "ca"):
        try:
            new_map.ca = original_map.ca
        except AttributeError:
            # In newer ruamel.yaml versions, .ca is a read-only property.
            # We copy the internal storage of comments.
            if hasattr(original_map.ca, "_items") and hasattr(new_map.ca, "_items"):
                new_map.ca._items = original_map.ca._items

    return new_map


def process_file(file_path):
    yaml = YAML()
    yaml.preserve_quotes = True
    # settings for Timelink YAMLs
    yaml.indent(mapping=2, sequence=4, offset=0)

    try:
        with open(file_path, "r") as f:
            data = yaml.load(f)

        changed = False

        if isinstance(data, list):
            for item in data:
                if isinstance(item, dict):
                    if "group" in item:
                        item["group"] = reorder_map(item["group"])
                        changed = True
                    if "element" in item:
                        item["element"] = reorder_map(item["element"])
                        changed = True

        if changed:
            print(f"Reordering {file_path}")
            with open(file_path, "w") as f:
                yaml.dump(data, f)
        else:
            print(f"No changes for {file_path}")

    except Exception as e:
        print(f"Error processing {file_path}: {e}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Reorder keys in Kleio YAML files.")
    parser.add_argument(
        "files", metavar="F", type=str, nargs="+", help="files to process"
    )
    args = parser.parse_args()

    for f in args.files:
        process_file(f)
