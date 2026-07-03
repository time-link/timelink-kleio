"""YAML structure file loader for Kleio schema definitions.

This module loads YAML structure files that define the schema for Kleio data files.
It handles the parsing of database, group, and element definitions from YAML format.

Key terminology mappings (English YAML -> Latin internal):
- source -> fons (parent for inheritance)
- position -> locus (positional elements)
- guaranteed -> certe (required elements)
- also -> ceteri (optional elements)
- contains -> pars (subgroups)
- idprefix -> signum (ID prefix)
- order -> ordo (ordering)
- identification -> identificatio (is first entry ID?)
- prefix -> prae (prefix text)
- suffix -> post (suffix text)
- repeat -> repetitio (repeatable subgroups)
- always -> semper (auto-repeat name/count pairs)
- only -> solum (exclusive subgroups)
- note/description -> nota (documentation)
- type -> modus (data type)
- first -> primum (document root group)
- name -> nomen (name)
- database -> nomino (database/structure definition)
- element -> terminus (element definition)
"""
from __future__ import annotations

from pathlib import Path
from typing import Any, Optional, Union

import yaml

from kleio.schema.models import ElementDef, GroupDef, StructureDef
from kleio.errors import ErrorAccumulator


# English to Latin keyword mappings (from struSyntax.pl)
ENGLISH_TO_LATIN = {
    # Group parameters
    'source': 'fons',
    'position': 'locus',
    'guaranteed': 'certe',
    'also': 'ceteri',
    'contains': 'pars',
    'idprefix': 'signum',
    'order': 'ordo',
    'identification': 'identificatio',
    'prefix': 'prae',
    'suffix': 'post',
    'repeat': 'repetitio',
    'always': 'semper',
    'only': 'solum',
    'note': 'nota',
    'description': 'nota',
    'name': 'nomen',
    'arbitrary': 'repetitio',  # alternative name for repeat
    # Element parameters
    'type': 'modus',
    'first': 'primum',
    'primary_type': 'primum',
    'secondary_type': 'secundum',
    'without': 'sine',
    'signs': 'signa',
    'format': 'forma',
    'cumulate': 'cumule',
    'part': 'pars',
    # Database/structure parameters
    'database': 'nomino',
    'file': 'nomino',  # alternative name for database
    'doc_name': 'primum',
    'mode': 'modus',
    'overwrite': 'antiquum',
    'write': 'scribe',
    'multiple': 'plures',
}

# Latin to English keyword mappings (reverse)
LATIN_TO_ENGLISH = {v: k for k, v in ENGLISH_TO_LATIN.items()}
# Handle duplicates - prefer specific mappings
LATIN_TO_ENGLISH['nota'] = 'note'
LATIN_TO_ENGLISH['pars'] = 'contains'  # default for groups


def load_yaml_structure(filepath: Path, errors: Optional[ErrorAccumulator] = None) -> StructureDef:
    """Load a YAML structure file and return a StructureDef.
    
    Args:
        filepath: Path to the YAML structure file.
        errors: Optional error accumulator for collecting errors/warnings.
        
    Returns:
        StructureDef containing all groups and elements defined in the file.
        
    Raises:
        FileNotFoundError: If the file doesn't exist.
        yaml.YAMLError: If the YAML is malformed.
        ValueError: If required fields are missing.
    """
    if errors is None:
        errors = ErrorAccumulator()
    
    filepath = Path(filepath)
    if not filepath.exists():
        raise FileNotFoundError(f"Structure file not found: {filepath}")
    
    with open(filepath, 'r', encoding='utf-8') as f:
        data = yaml.safe_load(f)
    
    if data is None:
        raise ValueError(f"Empty YAML file: {filepath}")
    
    if not isinstance(data, list):
        raise ValueError(f"YAML structure file must be a list of entries: {filepath}")
    
    structure = StructureDef(source_file=str(filepath))
    
    # Process each entry in the YAML file
    for entry in data:
        if not isinstance(entry, dict):
            errors.warning(f"Skipping non-dict entry in {filepath}")
            continue
        
        _process_entry(entry, structure, errors, filepath)
    
    # Set the document root group name
    if structure.doc_name and structure.doc_name in structure.groups:
        structure.groups[structure.doc_name].is_doc = True
    
    return structure


def _process_entry(
    entry: dict[str, Any],
    structure: StructureDef,
    errors: ErrorAccumulator,
    filepath: Path
) -> None:
    """Process a single YAML entry (database, group, element, include, etc.)."""
    
    # Check for database/nomino definition
    if 'database' in entry:
        _process_database(entry['database'], structure, errors)
        return
    
    if 'file' in entry:
        _process_database(entry['file'], structure, errors)
        return
    
    # Check for group definition
    if 'group' in entry:
        _process_group(entry['group'], structure, errors)
        return
    
    # Check for element definition
    if 'element' in entry:
        _process_element(entry['element'], structure, errors)
        return
    
    # Check for include directive (to be handled by registry)
    if 'include' in entry:
        # Include processing is handled by SchemaRegistry
        # Just store the include reference for later processing
        if hasattr(structure, '_includes'):
            structure._includes.append(entry['include'])
        else:
            structure._includes = [entry['include']]
        return
    
    # Check for path (metadata, ignored)
    if 'path' in entry:
        return
    
    # Unknown entry type
    entry_keys = list(entry.keys())
    if entry_keys:
        errors.warning(f"Unknown entry type: {entry_keys[0]} in {filepath}")


def _process_database(
    data: dict[str, Any],
    structure: StructureDef,
    errors: ErrorAccumulator
) -> None:
    """Process a database/nomino definition."""
    if not isinstance(data, dict):
        errors.error("database entry must be a dict")
        return
    
    # Get the name
    name = data.get('name', data.get('nomen', ''))
    if not name:
        errors.error("database entry missing 'name' field")
        return
    
    structure.name = name
    
    # Get the document root group name
    doc_name = data.get('first', data.get('primum', data.get('doc_name', '')))
    if doc_name:
        structure.doc_name = doc_name
    
    # Get other optional parameters
    if 'mode' in data or 'modus' in data:
        structure.mode = data.get('mode', data.get('modus', 'permanens'))
    
    if 'overwrite' in data or 'antiquum' in data:
        structure.overwrite = data.get('overwrite', data.get('antiquum', 'non'))
    
    if 'multiple' in data or 'plures' in data:
        structure.multiple = data.get('multiple', data.get('plures', 'non'))
    
    if 'write' in data or 'scribe' in data:
        write_val = data.get('write', data.get('scribe', []))
        if isinstance(write_val, str):
            structure.write = [write_val]
        else:
            structure.write = list(write_val) if write_val else []


def _process_group(
    data: dict[str, Any],
    structure: StructureDef,
    errors: ErrorAccumulator
) -> None:
    """Process a group definition."""
    if not isinstance(data, dict):
        errors.error("group entry must be a dict")
        return
    
    # Get the name (required)
    name = data.get('name', data.get('nomen', ''))
    if not name:
        errors.error("group entry missing 'name' field")
        return
    
    # Handle list of names (creates multiple groups with same properties)
    names = [name] if isinstance(name, str) else list(name)
    
    for group_name in names:
        group = GroupDef(name=group_name)
        
        # Process all properties
        _set_group_properties(group, data, errors)
        
        # Store in structure (may overwrite existing)
        structure.groups[group_name] = group


def _set_group_properties(group: GroupDef, data: dict[str, Any], errors: ErrorAccumulator) -> None:
    """Set properties on a GroupDef from YAML data."""
    
    # Description/note
    if 'description' in data:
        group.description = data['description']
    if 'note' in data:
        group.note = data['note']
    
    # Source (fons) - parent group for inheritance
    if 'source' in data:
        group.source = data['source']
    if 'fons' in data:
        group.source = data['fons']
    
    # Position (locus) - positional elements
    position = data.get('position', data.get('locus', []))
    if isinstance(position, str):
        group.position = [position]
    else:
        group.position = list(position) if position else []
    
    # Guaranteed (certe) - required elements
    guaranteed = data.get('guaranteed', data.get('certe', []))
    if isinstance(guaranteed, str):
        group.guaranteed = [guaranteed]
    else:
        group.guaranteed = list(guaranteed) if guaranteed else []
    
    # Also (ceteri) - optional elements
    also = data.get('also', data.get('ceteri', []))
    if isinstance(also, str):
        group.also = [also]
    else:
        group.also = list(also) if also else []
    
    # Contains (pars) - subgroups
    contains = data.get('contains', data.get('pars', []))
    if isinstance(contains, str):
        group.contains = [contains]
    else:
        group.contains = list(contains) if contains else []
    
    # ID prefix (signum)
    if 'idprefix' in data:
        group.idprefix = data['idprefix']
    if 'signum' in data:
        group.idprefix = data['signum']
    
    # Order (ordo)
    order = data.get('order', data.get('ordo', ''))
    if order:
        group.order = order
    
    # Identification (identificatio). Normalize YAML booleans to sic/non.
    ident = data.get('identification', data.get('identificatio', ''))
    if ident is True or str(ident).strip().lower() in ('yes', 'true', 'sic'):
        group.identification = "sic"
    elif ident is False or str(ident).strip().lower() in ('no', 'false', 'non'):
        group.identification = "non"
    elif ident:
        group.identification = ident
    
    # Prefix (prae)
    if 'prefix' in data:
        group.prefix = data['prefix']
    if 'prae' in data:
        group.prefix = data['prae']
    
    # Suffix (post)
    if 'suffix' in data:
        group.suffix = data['suffix']
    if 'post' in data:
        group.suffix = data['post']
    
    # Repeat (repetitio) - also called 'arbitrary'
    repeat = data.get('repeat', data.get('repetitio', data.get('arbitrary', [])))
    if isinstance(repeat, str):
        group.repeat = [repeat]
    else:
        group.repeat = list(repeat) if repeat else []
    
    # Always (semper) - name/count pairs
    always = data.get('always', data.get('semper', []))
    if isinstance(always, str):
        group.always = [always]
    else:
        group.always = list(always) if always else []
    
    # Only (solum) - exclusive subgroups
    only = data.get('only', data.get('solum', []))
    if isinstance(only, str):
        group.only = [only]
    else:
        group.only = list(only) if only else []


def _process_element(
    data: dict[str, Any],
    structure: StructureDef,
    errors: ErrorAccumulator
) -> None:
    """Process an element definition."""
    if not isinstance(data, dict):
        errors.error("element entry must be a dict")
        return
    
    # Get the name (required)
    name = data.get('name', data.get('nomen', ''))
    if not name:
        errors.error("element entry missing 'name' field")
        return
    
    # Handle list of names (creates multiple elements with same properties)
    names = [name] if isinstance(name, str) else list(name)
    
    for element_name in names:
        element = ElementDef(name=element_name)
        
        # Process all properties
        _set_element_properties(element, data, errors)
        
        # Store in structure (may overwrite existing)
        structure.elements[element_name] = element


def _set_element_properties(element: ElementDef, data: dict[str, Any], errors: ErrorAccumulator) -> None:
    """Set properties on an ElementDef from YAML data."""
    
    # Description/note
    if 'description' in data:
        element.description = data['description']
    if 'note' in data:
        element.note = data['note']
    
    # Source (fons) - parent element for inheritance
    if 'source' in data:
        element.source = data['source']
    if 'fons' in data:
        element.source = data['fons']
    
    # Type (modus) - data type
    if 'type' in data:
        element.type = data['type']
    if 'modus' in data:
        element.type = data['modus']
    
    # Primary type (primum)
    if 'primary_type' in data:
        element.primary_type = data['primary_type']
    if 'primum' in data:
        element.primary_type = data['primum']
    
    # Secondary type (secundum)
    if 'secondary_type' in data:
        element.secondary_type = data['secondary_type']
    if 'secundum' in data:
        element.secondary_type = data['secundum']
    
    # Order (ordo)
    order = data.get('order', data.get('ordo', ''))
    if order:
        element.order = order
    
    # Identification (identificatio). YAML may parse "yes"/"no" or
    # "true"/"false" as booleans; normalize to the canonical "sic"/"non"
    # strings the rest of the code compares against.
    ident = data.get('identification', data.get('identificatio', ''))
    if ident is True or str(ident).strip().lower() in ('yes', 'true', 'sic'):
        element.identification = "sic"
    elif ident is False or str(ident).strip().lower() in ('no', 'false', 'non'):
        element.identification = "non"
    elif ident:
        element.identification = ident
    
    # Prefix (prae)
    if 'prefix' in data:
        element.prefix = data['prefix']
    if 'prae' in data:
        element.prefix = data['prae']
    
    # Suffix (post)
    if 'suffix' in data:
        element.suffix = data['suffix']
    if 'post' in data:
        element.suffix = data['post']
    
    # Part (pars) - containing group override
    if 'part' in data:
        element.part = data['part']
    if 'pars' in data:
        element.part = data['pars']
    
    # Without (sine)
    if 'without' in data:
        element.without = data['without']
    if 'sine' in data:
        element.without = data['sine']
    
    # Signs (signa)
    if 'signs' in data:
        element.signs = data['signs']
    if 'signa' in data:
        element.signs = data['signa']
    
    # Format (forma)
    if 'format' in data:
        element.format = data['format']
    if 'forma' in data:
        element.format = data['forma']
    
    # Also (ceteri)
    also = data.get('also', data.get('ceteri', ''))
    if also:
        element.also = also if isinstance(also, str) else str(also)
    
    # Cumulate (cumule)
    if 'cumulate' in data:
        element.cumulate = data['cumulate']
    if 'cumule' in data:
        element.cumulate = data['cumule']
    
    # Only (solum)
    if 'only' in data:
        element.only = data['only']
    if 'solum' in data:
        element.only = data['solum']
