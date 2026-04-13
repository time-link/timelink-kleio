"""Kleio schema module.

This module provides data models and utilities for Kleio structure (schema) definitions,
including:
- GroupDef, ElementDef, StructureDef: Data models for schema components
- SchemaRegistry: Registry for loaded structures with hierarchy queries
- load_yaml_structure: YAML structure file loader
"""

from kleio.schema.models import ElementDef, GroupDef, StructureDef
from kleio.schema.loader import load_yaml_structure
from kleio.schema.registry import SchemaRegistry

__all__ = [
    "ElementDef",
    "GroupDef", 
    "StructureDef",
    "SchemaRegistry",
    "load_yaml_structure",
]
