"""Data models for Kleio structure (schema) definitions.

This module defines the data structures that represent Kleio schema
definitions, including groups (data containers) and elements (data fields).

The schema is defined in structure files (.str, .yaml) and determines:
- What groups can exist (e.g., act, person, place)
- What elements each group contains
- The hierarchy of nested groups
- Data types and validation rules for elements

Key terminology (Latin keywords from original Kleio system):
- pars: subgroup containment
- locus: positional elements
- certe: required/guaranteed elements
- ceteri: optional/additional elements
- fons: source/parent for inheritance
- signum: ID prefix for auto-generated identifiers
"""
from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional


@dataclass
class GroupDef:
    """Definition of a group in a Kleio structure file.
    
    Groups are hierarchical data containers (like act, person, etc.).
    They define what elements and subgroups are allowed.
    
    In Kleio notation, groups are defined with the 'pars' directive:
        pars nomen=act
             locus=[date,type,loc]
             certe=[the_date,the_type,loc]
             pars=[person,object]
    
    Property mappings (Latin -> English):
    - nomen -> name (group name)
    - fons -> source (parent group for inheritance)
    - locus -> position (positional elements)
    - certe -> guaranteed (required elements)
    - ceteri -> also (optional elements)
    - pars -> contains (subgroups)
    - signum -> idprefix (ID prefix for auto-generated IDs)
    - ordo -> order (ordering: sic/non)
    - identificatio -> identification (first entry is ID? sic/non)
    - prae -> prefix (prefix text)
    - post -> suffix (suffix text)
    - repetitio -> repeat (repeatable subgroups)
    - semper -> always (name/count pairs for auto-repeat)
    - solum -> only (exclusive subgroups)
    - nota -> note (documentation)
    """
    name: str
    description: str = ""
    source: str = ""           # fons - parent group for inheritance
    position: list[str] = field(default_factory=list)   # locus - positional elements
    guaranteed: list[str] = field(default_factory=list)  # certe - required elements
    also: list[str] = field(default_factory=list)        # ceteri - optional elements  
    contains: list[str] = field(default_factory=list)    # pars - subgroups
    idprefix: str = ""         # signum - prefix for auto-generated IDs
    order: str = ""            # ordo - ordering (sic/non)
    identification: str = "non"  # identificatio - first entry is ID? (sic/non)
    prefix: str = ""           # prae - prefix text
    suffix: str = ""           # post - suffix text
    repeat: list[str] = field(default_factory=list)  # repetitio - repeatable subgroups
    always: list[str] = field(default_factory=list)   # semper - name/count pairs for auto-repeat
    only: list[str] = field(default_factory=list)     # solum - exclusive subgroups (name/count)
    note: str = ""             # nota - documentation note

    # Runtime state (populated after loading)
    is_doc: bool = False       # True if this is the document root group
    base_class: str = ""       # Resolved base class name (from fons chain)

    @property
    def all_elements(self) -> list[str]:
        """All element names (position + guaranteed + also)."""
        seen = set()
        result = []
        for name in self.position + self.guaranteed + self.also:
            if name not in seen:
                seen.add(name)
                result.append(name)
        return result

    @property
    def elements(self) -> list[ElementDef]:
        """Element definitions for this group (populated by SchemaRegistry).
        
        This property returns ElementDef objects for all elements referenced
        by this group (position + guaranteed + also).
        """
        # This is populated by SchemaRegistry after loading
        if hasattr(self, '_elements'):
            return self._elements
        return []

    def _resolve_elements(self, structure: StructureDef) -> None:
        """Resolve element definitions from element names.
        
        This is called by SchemaRegistry after loading the structure.
        """
        self._elements = []
        seen = set()
        for name in self.all_elements:
            if name not in seen:
                seen.add(name)
                element_def = structure.get_element(name)
                if element_def:
                    self._elements.append(element_def)


@dataclass
class ElementDef:
    """Definition of an element (terminus) in a Kleio structure file.
    
    Elements are the leaf data fields within groups.
    
    In Kleio notation, elements are defined with the 'terminus' directive:
        terminus nomen=name
                 modus=lingua
                 ordo=simplex
    
    Property mappings (Latin -> English):
    - nomen -> name (element name)
    - fons -> source (parent element for inheritance)
    - modus -> type (data type)
    - primum -> primary_type (primary processing type)
    - secundum -> secondary_type (secondary processing type)
    - ordo -> order (simplex or multiplex)
    - identificatio -> identification (is this an ID element? sic/non)
    - prae -> prefix (prefix)
    - post -> suffix (suffix)
    - pars -> part (containing group override)
    - sine -> without (exclusion flag)
    - signa -> signs (sign handling)
    - forma -> format (formatting)
    - ceteri -> also (additional elements)
    - cumule -> cumulate (cumulation flag)
    - solum -> only (exclusivity flag)
    - nota -> note (documentation)
    
    Data types (modus):
    - lingua: text/language
    - tempora: date/time
    - numerus: number
    - condicio: condition/category
    - situs: location
    - relatio: relation/reference
    """
    name: str
    description: str = ""
    source: str = ""           # fons - parent element for inheritance
    type: str = ""             # modus - data type (lingua/tempora/numerus/condicio/situs/relatio)
    primary_type: str = ""     # primum - primary processing type
    secondary_type: str = ""   # secundum - secondary processing type
    order: str = "simplex"     # ordo - simplex or multiplex
    identification: str = "non"  # identificatio - is this an ID element? (sic/non)
    prefix: str = ""           # prae - prefix
    suffix: str = ""           # post - suffix  
    part: str = ""             # pars - containing group override
    without: str = ""          # sine - exclusion flag
    signs: str = ""            # signa - sign handling
    format: str = ""           # forma - formatting
    also: str = ""             # ceteri - additional elements
    cumulate: str = ""         # cumule - cumulation flag
    only: str = ""             # solum - exclusivity flag
    note: str = ""             # nota - documentation note

    # Runtime state
    base_class: str = ""       # Resolved base class name


@dataclass
class StructureDef:
    """Top-level structure definition loaded from a YAML/STR file.
    
    Contains all group and element definitions for one schema.
    
    In Kleio notation, the structure is defined with the 'nomino' directive:
        nomino nomen=mydb
               primum=document
               modus=permanens
    
    Property mappings (Latin -> English):
    - nomen -> name (database/structure name)
    - primum -> doc_name (document root group name)
    - modus -> mode (permanent or ad-hoc)
    - antiquum -> overwrite (overwrite existing? sic/non)
    - scribe -> write (output flags)
    - plures -> multiple (allow multiple entries? sic/non)
    """
    name: str = ""              # Database/structure name (from nomino/database)
    doc_name: str = ""          # Document root group name (from primum/first)
    groups: dict[str, GroupDef] = field(default_factory=dict)
    elements: dict[str, ElementDef] = field(default_factory=dict)
    source_file: str = ""       # Path to the structure file
    
    # nomino properties
    mode: str = "permanens"     # modus - permanent or ad-hoc
    overwrite: str = "non"      # antiquum - overwrite existing?
    write: list[str] = field(default_factory=list)  # scribe - output flags
    multiple: str = "non"       # plures - allow multiple entries?
    
    def get_group(self, name: str) -> Optional[GroupDef]:
        """Get a group definition by name."""
        return self.groups.get(name)
    
    def get_element(self, name: str) -> Optional[ElementDef]:
        """Get an element definition by name."""
        return self.elements.get(name)
    
    @property
    def doc_group(self) -> Optional[GroupDef]:
        """Get the document root group definition."""
        if self.doc_name:
            return self.groups.get(self.doc_name)
        return None
