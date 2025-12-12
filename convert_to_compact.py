#!/usr/bin/env python3
"""
Script to convert YAML file to compact notation.
Converts multi-line group/element entries to single-line format.
"""

import sys

def convert_to_compact(file_path):
    with open(file_path, 'r') as f:
        lines = f.readlines()
    
    new_lines = []
    i = 0
    
    while i < len(lines):
        line = lines[i].rstrip()
        
        # Check for group or element start
        if line == '- group:' or line == '- element:':
            entry_type = 'group' if 'group' in line else 'element'
            
            # Collect all properties for this entry
            properties = {}
            j = i + 1
            
            while j < len(lines):
                # Stop if we hit another top-level entry
                if lines[j].startswith('- group:') or lines[j].startswith('- element:') or lines[j].startswith('- file:') or lines[j].startswith('- include:'):
                    break
                
                # Check if line has content (not just whitespace)
                stripped = lines[j].strip()
                if not stripped:
                    j += 1
                    continue
                
                # Parse property line
                if ':' in stripped:
                    parts = stripped.split(':', 1)
                    key = parts[0].strip()
                    value = parts[1].strip() if len(parts) > 1 else ''
                    
                    if value:
                        properties[key] = value
                    else:
                        # Property might be a list, check next lines
                        k = j + 1
                        list_items = []
                        while k < len(lines) and lines[k].startswith('    - '):
                            list_items.append(lines[k].strip()[2:])  # Remove '- ' prefix
                            k += 1
                        
                        if list_items:
                            properties[key] = list_items
                            j = k - 1  # Adjust position
                
                j += 1
            
            # Build compact notation line
            if properties:
                compact_parts = [f"{entry_type}:"]
                for key, value in properties.items():
                    if isinstance(value, list):
                        # Format list as [item1, item2, ...]
                        list_str = '[' + ', '.join(value) + ']'
                        compact_parts.append(f"{key}: {list_str}")
                    else:
                        compact_parts.append(f"{key}: {value}")
                
                compact_line = f"- {{{', '.join(compact_parts)}}}\n"
                new_lines.append(compact_line)
            else:
                # No properties, keep as is
                new_lines.append(lines[i])
            
            i = j  # Skip processed lines
        else:
            # Keep non-group/element lines as is
            new_lines.append(lines[i])
            i += 1
    
    # Write back to file
    with open(file_path, 'w') as f:
        f.writelines(new_lines)
    
    print(f"✓ Converted to compact notation: {file_path}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python convert_to_compact.py <file_path>")
        sys.exit(1)
    
    file_path = sys.argv[1]
    convert_to_compact(file_path)
