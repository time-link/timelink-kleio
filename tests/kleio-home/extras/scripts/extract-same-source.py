import yaml
import sys

if len(sys.argv) != 3:
    print("Usage: python extract-same-source.py <yaml_file> <source_value>")
    sys.exit(1)

yaml_file = sys.argv[1]
source_value = sys.argv[2]

with open(yaml_file, 'r') as f:
    data = yaml.safe_load(f)

same_source_groups = [g for g in data.get('group', [])
                      if g.get('source') == source_value]

output_file = f"{source_value}-groups.yaml"
with open(output_file, 'w') as f:
    yaml.dump({'group': same_source_groups},
              f,
              sort_keys=False,
              allow_unicode=True)

print(f"Extracted {len(same_source_groups)} groups with source '{source_value}' to '{output_file}'")