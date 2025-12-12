#!/usr/bin/env python3

import re
import sys

def remove_duplicate_groups(file_path):
    with open(file_path, 'r') as f:
        lines = f.readlines()
    
    # Find all group start positions
    group_starts = []
    for i, line in enumerate(lines):
        if line.strip() == '- group:':
            group_starts.append(i)
    
    # Extract group names and their positions
    group_info = []
    for start_idx in group_starts:
        # Look for the name line within the next few lines
        for j in range(start_idx, min(start_idx + 10, len(lines))):
            if re.match(r'^\s+name:\s+(.+)', lines[j]):
                group_name = re.match(r'^\s+name:\s+(.+)', lines[j]).group(1).strip()
                group_info.append({
                    'name': group_name,
                    'start_line': start_idx,
                    'name_line': j
                })
                break
    
    # Find duplicate group names
    seen_names = {}
    duplicates = []
    
    for group in group_info:
        name = group['name']
        if name in seen_names:
            # This is a duplicate, mark for removal
            duplicates.append(group)
        else:
            seen_names[name] = group
    
    # Sort duplicates by line number in reverse order for safe removal
    duplicates.sort(key=lambda x: x['start_line'], reverse=True)
    
    # Mark lines for removal
    lines_to_remove = set()
    
    for dup_group in duplicates:
        start_line = dup_group['start_line']
        
        # Find the end of this group (start of next group or end of file)
        next_group_line = len(lines)
        for group in group_info:
            if group['start_line'] > start_line:
                next_group_line = group['start_line']
                break
        
        # Mark all lines from start to end for removal
        for i in range(start_line, next_group_line):
            lines_to_remove.add(i)
    
    # Remove the duplicate groups (in reverse order to maintain line numbers)
    lines_to_remove = sorted(list(lines_to_remove), reverse=True)
    for line_num in lines_to_remove:
        del lines[line_num]
    
    # Write the cleaned file
    with open(file_path, 'w') as f:
        f.writelines(lines)
    
    print(f"Removed {len(duplicates)} duplicate group entries.")
    for dup in duplicates:
        print(f"  - Removed duplicate group '{dup['name']}' at line {dup['start_line'] + 1}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python remove_duplicates.py <file_path>")
        sys.exit(1)
    
    file_path = sys.argv[1]
    remove_duplicate_groups(file_path)