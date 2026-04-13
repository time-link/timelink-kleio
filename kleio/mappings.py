"""Data normalization mappings for Kleio translation.

Loads and applies normalization mappings that transform input data
during the export phase. Replaces the Prolog mappings.pl module.
"""
from __future__ import annotations
import json
import logging
from pathlib import Path
from typing import Any, Optional

logger = logging.getLogger(__name__)


class MappingStore:
    """Loads and applies data normalization mappings.
    
    Supports multiple mapping types:
    - Simple value mappings: {key: value} for data normalization
    - Class mappings: Map group names to entity classes
    - Attribute mappings: Define attribute properties for export
    """

    def __init__(self):
        """Initialize an empty mapping store."""
        self._mappings: dict[str, dict[str, str]] = {}
        self._class_mappings: dict[str, str] = {}  # group name -> class name
        self._class_definitions: dict[str, dict[str, Any]] = {}

    def load_mapping_file(self, filepath: Path) -> None:
        """Load a mapping file (YAML or JSON format).
        
        Handles both simple value mappings and class definition mappings.
        
        Args:
            filepath: Path to the mapping file.
        """
        suffix = filepath.suffix.lower()
        
        if suffix in (".yml", ".yaml"):
            self._load_yaml_mapping(filepath)
        elif suffix == ".json":
            self._load_json_mapping(filepath)
        elif suffix == ".pl":
            logger.debug(f"Skipping Prolog mapping file: {filepath}")
        else:
            logger.warning(f"Unknown mapping file format: {filepath}")

    def _load_yaml_mapping(self, filepath: Path) -> None:
        """Load a YAML mapping file.
        
        Handles the Kleio mapping YAML format which contains:
        - mapping entries: {name: group_name, class: class_name}
        - class definitions with attributes
        
        Args:
            filepath: Path to the YAML file.
        """
        try:
            import yaml
        except ImportError:
            logger.error("PyYAML not installed, cannot load YAML mappings")
            return

        with open(filepath, "r", encoding="utf-8") as f:
            data = yaml.safe_load(f)

        if data is None:
            return

        # Handle list format (standard Kleio mapping files)
        if isinstance(data, list):
            for item in data:
                self._process_mapping_item(item)
        elif isinstance(data, dict):
            # Simple key-value mapping
            mapping_name = filepath.stem
            self._mappings[mapping_name] = data

    def _process_mapping_item(self, item: dict[str, Any]) -> None:
        """Process a single mapping item from a YAML file.
        
        Args:
            item: A mapping item dictionary.
        """
        if "mapping" in item:
            # Mapping entry: {name: group_name, class: class_name}
            mapping = item["mapping"]
            group_name = mapping.get("name", "")
            class_name = mapping.get("class", "")
            if group_name and class_name:
                self._class_mappings[group_name] = class_name
                logger.debug(f"Loaded mapping: {group_name} -> {class_name}")
        
        elif "class" in item:
            # Class definition
            class_def = item["class"]
            class_name = class_def.get("name", "")
            if class_name:
                self._class_definitions[class_name] = class_def
                logger.debug(f"Loaded class definition: {class_name}")

    def _load_json_mapping(self, filepath: Path) -> None:
        """Load a JSON mapping file.
        
        Simple key-value mappings for data normalization.
        
        Args:
            filepath: Path to the JSON file.
        """
        with open(filepath, "r", encoding="utf-8") as f:
            data = json.load(f)
        
        mapping_name = filepath.stem
        if isinstance(data, dict):
            self._mappings[mapping_name] = data

    def load_directory(self, dirpath: Path) -> None:
        """Load all mapping files from a directory.
        
        Args:
            dirpath: Path to the directory containing mapping files.
        """
        if not dirpath.exists():
            logger.debug(f"Mapping directory does not exist: {dirpath}")
            return
        
        for fp in sorted(dirpath.iterdir()):
            if fp.is_file() and fp.suffix.lower() in (".json", ".yml", ".yaml"):
                self.load_mapping_file(fp)

    def apply(self, value: str, mapping_name: str) -> str:
        """Apply a named mapping to a value.
        
        Args:
            value: The value to transform.
            mapping_name: The name of the mapping to apply.
        
        Returns:
            The mapped value, or the original if no match found.
        """
        mapping = self._mappings.get(mapping_name, {})
        return mapping.get(value, value)

    def get_class_for_group(self, group_name: str) -> Optional[str]:
        """Get the mapped class name for a group.
        
        Args:
            group_name: The source group name.
        
        Returns:
            The mapped class name, or None if not found.
        """
        return self._class_mappings.get(group_name)

    def get_class_definition(self, class_name: str) -> Optional[dict[str, Any]]:
        """Get the definition of a mapped class.
        
        Args:
            class_name: The class name.
        
        Returns:
            The class definition dictionary, or None if not found.
        """
        return self._class_definitions.get(class_name)

    def has_mapping(self, name: str) -> bool:
        """Check if a value mapping exists.
        
        Args:
            name: The mapping name.
        
        Returns:
            True if the mapping exists.
        """
        return name in self._mappings

    def get_mapping(self, name: str) -> dict[str, str]:
        """Get a value mapping by name.
        
        Args:
            name: The mapping name.
        
        Returns:
            A copy of the mapping dictionary.
        """
        return dict(self._mappings.get(name, {}))

    def get_all_class_mappings(self) -> dict[str, str]:
        """Get all group-to-class mappings.
        
        Returns:
            A copy of the class mappings dictionary.
        """
        return dict(self._class_mappings)

    def clear(self) -> None:
        """Clear all loaded mappings."""
        self._mappings.clear()
        self._class_mappings.clear()
        self._class_definitions.clear()
