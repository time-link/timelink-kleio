#!/usr/bin/env python3
"""
Script to update mesmo_que references in Kleio .cli files.

This script goes through all .cli files in a directory and its subdirectories,
finds all 'mesmo_que=<ID>' references, and checks if there's a corresponding
'id=<ID>' in the same file. If not, it changes 'mesmo_que=<ID>' to 'xmesmo_que=<ID>'.
"""

import os
import re
import argparse
from pathlib import Path


def find_ids_in_file(file_path):
    """
    Extract all IDs from a file that are defined with 'id=<ID>'
    
    Args:
        file_path (str): Path to the .cli file
        
    Returns:
        set: Set of ID strings found in the file
    """
    ids = set()
    id_pattern = r'/id=([^/\s]+)'
    
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
            matches = re.findall(id_pattern, content)
            ids.update(matches)
    except Exception as e:
        print(f"Error reading file {file_path}: {e}")
    
    return ids


def update_mesmo_que_references(file_path, dry_run=False):
    """
    Update mesmo_que references in a file if the referenced ID is not present in the same file.
    
    Args:
        file_path (str): Path to the .cli file
        dry_run (bool): If True, only report changes without making them
        
    Returns:
        list: List of changes made or that would be made
    """
    changes = []
    
    # Read the file
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            lines = f.readlines()
    except Exception as e:
        print(f"Error reading file {file_path}: {e}")
        return changes
    
    # Find all IDs in the current file
    file_ids = find_ids_in_file(file_path)
    
    # Pattern to match mesmo_que=<ID>
    mesmo_que_pattern = r'/mesmo_que=([^/\s]+)'
    
    # Process each line
    modified_lines = []
    for line_num, line in enumerate(lines, 1):
        # Find all mesmo_que references in this line
        matches = re.finditer(mesmo_que_pattern, line)
        line_modified = False
        
        for match in matches:
            referenced_id = match.group(1)
            
            # Check if the referenced ID exists in the same file
            if referenced_id not in file_ids:
                # Need to change mesmo_que to xmesmo_que
                old_pattern = f'/mesmo_que={referenced_id}'
                new_pattern = f'/xmesmo_que={referenced_id}'
                
                # Replace in the line
                if old_pattern in line:
                    line = line.replace(old_pattern, new_pattern)
                    line_modified = True
                    changes.append({
                        'file': file_path,
                        'line': line_num,
                        'old': f'mesmo_que={referenced_id}',
                        'new': f'xmesmo_que={referenced_id}'
                    })
        
        modified_lines.append(line)
    
    # Write the modified content back to the file if not in dry run mode
    if not dry_run and changes:
        try:
            with open(file_path, 'w', encoding='utf-8') as f:
                f.writelines(modified_lines)
        except Exception as e:
            print(f"Error writing file {file_path}: {e}")
    
    return changes


def process_directory(directory_path, dry_run=False):
    """
    Process all .cli files in a directory and its subdirectories.
    
    Args:
        directory_path (str): Path to the directory to process
        dry_run (bool): If True, only report changes without making them
    """
    directory = Path(directory_path)
    
    if not directory.exists():
        print(f"Directory {directory_path} does not exist.")
        return
    
    # Find all .cli files
    cli_files = list(directory.rglob("*.cli"))
    
    if not cli_files:
        print(f"No .cli files found in {directory_path} and its subdirectories.")
        return
    
    print(f"Found {len(cli_files)} .cli files to process.")
    
    total_changes = []
    
    # Process each file
    for file_path in cli_files:
        print(f"Processing {file_path}...")
        changes = update_mesmo_que_references(file_path, dry_run)
        total_changes.extend(changes)
        
        if changes and dry_run:
            print(f"  Would make {len(changes)} changes:")
            for change in changes:
                print(f"    Line {change['line']}: {change['old']} -> {change['new']}")
        elif changes:
            print(f"  Made {len(changes)} changes")
    
    # Summary
    if dry_run:
        print(f"\nDRY RUN SUMMARY: Would make {len(total_changes)} total changes across {len(cli_files)} files.")
    else:
        print(f"\nSUMMARY: Made {len(total_changes)} total changes across {len(cli_files)} files.")
    
    return total_changes


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('directory', help='Directory to process')
    parser.add_argument('--dry-run', action='store_true', 
                        help='Show what would be changed without making changes')
    
    args = parser.parse_args()
    
    process_directory(args.directory, args.dry_run)


if __name__ == "__main__":
    main()