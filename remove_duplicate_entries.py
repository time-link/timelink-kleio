#!/usr/bin/env python3
"""
Script to remove duplicate group and element entries in YAML structure files.
Keeps only the first occurrence of each name.
"""

import re
import sys

def remove_duplicate_entries(file_path):
    with open(file_path, 'r') as f:
        lines = f.readlines()
    
    # Find all group and element start positions with their names
    entries = []
    i = 0
    while i < len(lines):
        line = lines[i].rstrip()
        
        # Check for group or element start
        if line == '- group:' or line == '- element:':
            entry_type = 'group' if 'group' in line else 'element'
            start_line = i
            
            # Look for the name in the following lines
            j = i + 1
            name = None
            name_line = None
            while j < len(lines):
                # Stop if we hit another top-level entry
                if lines[j].startswith('- group:') or lines[j].startswith('- element:') or lines[j].startswith('- file:') or lines[j].startswith('- include:'):
                    break
                
                # Look for name field
                match = re.match(r'^\s+name:\s+(.+)', lines[j])
                if match:
                    name = match.group(1).strip()
                    name_line = j
                    break
                j += 1
            
            if name:
                entries.append({
                    'type': entry_type,
                    'name': name,
                    'start_line': start_line,
                    'name_line': name_line
                })
        
        i += 1
    
    # Find the end line for each entry
    for idx, entry in enumerate(entries):
        if idx < len(entries) - 1:
            entry['end_line'] = entries[idx + 1]['start_line']
        else:
            entry['end_line'] = len(lines)
    
    # Track seen names and identify duplicates
    seen_names = {}
    duplicates_to_remove = []
    
    for entry in entries:
        key = f"{entry['type']}:{entry['name']}"
        if key in seen_names:
            # This is a duplicate
            duplicates_to_remove.append(entry)
            print(f"Found duplicate {entry['type']} '{entry['name']}' at line {entry['start_line'] + 1}")
        else:
            seen_names[key] = entry
            print(f"Keeping first {entry['type']} '{entry['name']}' at line {entry['start_line'] + 1}")
    
    # Remove duplicates in reverse order to maintain line numbers
    duplicates_to_remove.sort(key=lambda x: x['start_line'], reverse=True)
    
    lines_to_remove = set()
    for dup in duplicates_to_remove:
        for line_num in range(dup['start_line'], dup['end_line']):
            lines_to_remove.add(line_num)
    
    # Create new file content without duplicate lines
    new_lines = [lines[i] for i in range(len(lines)) if i not in lines_to_remove]
    
    # Write back to file
    with open(file_path, 'w') as f:
        f.writelines(new_lines)
    
    print(f"\n✓ Removed {len(duplicates_to_remove)} duplicate entries")
    print(f"✓ Kept {len(seen_names)} unique entries")
    
    return len(duplicates_to_remove)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python remove_duplicate_entries.py <file_path>")
        sys.exit(1)
    
    file_path = sys.argv[1]
    count = remove_duplicate_entries(file_path)
    print(f"\n✓ Done! Removed {count} duplicate entries from {file_path}")
