# Update Mesmo Que Script

This script updates `mesmo_que` references in Kleio `.cli` files to `xmesmo_que` when appropriate.

## Purpose

In Kleio files, `mesmo_que=<ID>` is used to reference entities defined elsewhere. However, sometimes these references point to IDs that are not defined in the same file. In these cases, the references should be prefixed with `x` to become `xmesmo_que=<ID>` to indicate that they reference external entities.

## How It Works

The script:
1. Goes through all `.cli` files in a directory and its subdirectories
2. For each file, finds all `mesmo_que=<ID>` references
3. Checks if there's a corresponding `id=<ID>` defined in the same file
4. If no such ID exists in the file, changes `mesmo_que=<ID>` to `xmesmo_que=<ID>`
5. Leaves `mesmo_que=<ID>` unchanged if the ID is defined in the same file
6. Also leaves `xmesmo_que=<ID>` unchanged (preserves existing correct prefixes)

## Usage

```bash
# Dry run to see what changes would be made
python3 update_mesmo_que.py /path/to/directory --dry-run

# Actually make the changes
python3 update_mesmo_que.py /path/to/directory
```

## Example

Before:
```
pad$paulo ribeiro cabral/m/mesmo_que=c24-7/id=b1686.28-per5
```

After (if `c24-7` is not defined in the same file):
```
pad$paulo ribeiro cabral/m/xmesmo_que=c24-7/id=b1686.28-per5
```

Unchanged (if `c24-7` is defined in the same file):
```
pad$paulo ribeiro cabral/m/mesmo_que=c24-7/id=b1686.28-per5
```

Also unchanged (already correctly prefixed):
```
pad$paulo ribeiro cabral/m/xmesmo_que=c24-7/id=b1686.28-per5
```

## Results

When run on `/tests/kleio-home/sources/reference_sources/more_sources`, the script made 1048 changes across 184 files.