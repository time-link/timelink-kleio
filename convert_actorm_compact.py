#!/usr/bin/env python3
"""
Convert pt-actorm.yaml to compact notation
"""

import re

def convert_to_compact_yaml(input_file, output_file):
    with open(input_file, 'r') as f:
        lines = f.readlines()
    
    result = []
    i = 0
    
    while i < len(lines):
        line = lines[i].rstrip()
        
        if line == '- group:':
            # Collect all properties for this group
            props = {}
            j = i + 1
            
            while j < len(lines) and not lines[j].startswith('- group:'):
                stripped = lines[j].strip()
                if not stripped:
                    j += 1
                    continue
                
                if ':' in stripped:
                    key, _, value = stripped.partition(':')
                    key = key.strip()
                    value = value.strip()
                    
                    if value:
                        # Single value - remove quotes if present
                        if value.startswith("'") and value.endswith("'"):
                            value = value[1:-1]
                        props[key] = value
                    elif key == 'part':
                        # Multi-line list
                        part_items = []
                        k = j + 1
                        while k < len(lines) and lines[k].startswith('    - '):
                            part_items.append(lines[k].strip()[2:])
                            k += 1
                        if part_items:
                            props[key] = part_items
                            j = k - 1
                
                j += 1
            
            # Build compact line
            if props:
                parts = []
                for key in ['name', 'description', 'source', 'part']:
                    if key in props:
                        value = props[key]
                        if isinstance(value, list):
                            value_str = '[' + ', '.join(value) + ']'
                            parts.append(f'{key}: {value_str}')
                        else:
                            parts.append(f'{key}: {value}')
                
                if parts:
                    compact_line = '- group: {' + ', '.join(parts) + '}\n'
                    result.append(compact_line)
                else:
                    result.append('- group: {}\n')
            
            i = j
        else:
            # Keep other lines as is
            if line.strip():
                result.append(lines[i])
            i += 1
    
    with open(output_file, 'w') as f:
        f.writelines(result)
    
    print(f"✓ Converted to compact notation")
    print(f"✓ Input: {len(lines)} lines")
    print(f"✓ Output: {len(result)} lines")

if __name__ == '__main__':
    input_file = '/Users/jrc/develop/timelink-kleio/tests/kleio-home/structures/pt-actorm.yaml'
    output_file = input_file
    convert_to_compact_yaml(input_file, output_file)
