# flake8: noqa: E501
import yaml
import sys
from pathlib import Path


def remove_key_recursively(data, key_to_remove):
    """
    Recursively traverse the data structure and remove the specified key from all dictionaries.
    """
    if isinstance(data, dict):
        data.pop(key_to_remove, None)  # Remove the specified key if present
        for value in data.values():
            remove_key_recursively(value, key_to_remove)
    elif isinstance(data, list):
        for item in data:
            remove_key_recursively(item, key_to_remove)


def process_yaml_file(input_file, output_file, key_to_remove):
    """
    Read YAML file, remove the specified key from all dictionaries, and write to output file.
    """
    try:
        with open(input_file, 'r', encoding='utf-8') as file:
            data = yaml.safe_load(file)
        remove_key_recursively(data, key_to_remove)
        with open(output_file, 'w', encoding='utf-8') as file:
            yaml.dump(data, file, default_flow_style=False, allow_unicode=True, indent=2)
        print(f"Successfully processed {input_file} -> {output_file}, removed key '{key_to_remove}'")
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)


def main():
    if len(sys.argv) != 4:
        print("Usage: python yaml_remove_key.py <input_yaml_file> <output_yaml_file> <key_to_remove>")
        print("Example: python yaml_remove_key.py pt-actorm.yaml pt-actorm-cleaned.yaml arbitrary")
        sys.exit(1)
    input_file, output_file, key_to_remove = sys.argv[1:4]
    if not Path(input_file).exists():
        print(f"Error: Input file '{input_file}' does not exist.")
        sys.exit(1)
    process_yaml_file(input_file, output_file, key_to_remove)


if __name__ == "__main__":
    main()
