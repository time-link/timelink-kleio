"""JSON exporter for Kleio data.

This module implements JSON export functionality as a simpler alternative
to XML export. It produces structured JSON output from parsed Kleio groups.

Example JSON output:
    {
      "structure": "gacto2.str",
      "source": "test.cli",
      "translator": "kleio-python",
      "timestamp": "2024-01-01T12:00:00",
      "groups": [
        {
          "id": "fonte-1",
          "name": "fonte",
          "class": "source",
          "level": 1,
          "line": 2,
          "elements": [
            {"name": "id", "core": "test-source", "original": "", "comment": ""}
          ],
          "groups": [...]
        }
      ],
      "relations": [...],
      "attributes": [...]
    }
"""
from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path
from typing import Optional, Any

from kleio.export.base import Exporter
from kleio.parser.models import ParsedGroup, ParsedElement
from kleio.schema.registry import SchemaRegistry
from kleio.inference.models import InferenceResults, GeneratedRelation, GeneratedAttribute


class JsonExporter(Exporter):
    """Exports parsed Kleio data as JSON.
    
    This exporter generates structured JSON output from parsed Kleio groups,
    providing a simpler alternative to XML export.
    
    Attributes:
        _output_path: Path to the output JSON file
        _files_json_path: Path to the .files.json metadata file
        _schema: The loaded schema registry
        _source_file: Path to the source .cli file
        _structure_file: Path to the structure file
        _all_groups: List of all collected groups
        _group_counter: Counter for processed groups
        _translator_name: Name of the translator
        _obs: Observation/comment string
        _data: The JSON data structure being built
    """
    
    def __init__(self):
        """Initialize the JSON exporter."""
        self._output_path: Optional[Path] = None
        self._files_json_path: Optional[Path] = None
        self._output_dir: Optional[Path] = None
        self._schema: Optional[SchemaRegistry] = None
        self._source_file: str = ""
        self._structure_file: str = ""
        self._all_groups: list[ParsedGroup] = []
        self._group_counter: int = 0
        self._translator_name: str = "kleio-python"
        self._obs: str = ""
        self._data: dict[str, Any] = {}
        
    def init(self, source_file: str, output_dir: Path,
             schema: SchemaRegistry, **kwargs) -> None:
        """Initialize the exporter for a new translation.
        
        Args:
            source_file: Path to the source .cli file being translated
            output_dir: Directory for output files
            schema: The loaded structure schema
            **kwargs: Additional options:
                - structure_file: Path to the structure file
                - translator: Name of the translator (default: kleio-python)
                - obs: Observation/comment string
        """
        self._source_file = source_file
        self._schema = schema
        self._output_dir = Path(output_dir)
        
        # Get optional parameters
        self._structure_file = kwargs.get('structure_file', schema.name if schema else '')
        self._translator_name = kwargs.get('translator', 'kleio-python')
        self._obs = kwargs.get('obs', '')
        
        # Create output file paths
        source_path = Path(source_file)
        base_name = source_path.stem
        self._output_path = self._output_dir / f"{base_name}.json"
        self._files_json_path = self._output_dir / f"{base_name}.files.json"
        
        # Reset state
        self._all_groups = []
        self._group_counter = 0
        
        # Initialize data structure
        self._data = {
            'structure': self._structure_file,
            'source': source_file,
            'translator': self._translator_name,
            'timestamp': datetime.now().isoformat(),
            'obs': self._obs,
            'groups': [],
            'relations': [],
            'attributes': []
        }
        
    def export_group(self, group: ParsedGroup) -> None:
        """Export a single completed group.
        
        Collects groups for later tree building in close().
        
        Args:
            group: The parsed group to export
        """
        self._all_groups.append(group)
        
    def _get_group_class(self, group: ParsedGroup) -> str:
        """Get the base class for a group from schema.
        
        Args:
            group: The parsed group
            
        Returns:
            The base class name from schema, or the group name if not found
        """
        if self._schema:
            base = self._schema.base_class(group.name)
            if base:
                return base
        return group.name
        
    def _get_element_class(self, element: ParsedElement) -> str:
        """Get the base class for an element from schema.
        
        Args:
            element: The parsed element
            
        Returns:
            The base class name from schema, or the element name if not found
        """
        if self._schema:
            base = self._schema.element_base_class(element.name)
            if base:
                return base
        return element.name
        
    def _element_to_dict(self, element: ParsedElement) -> dict[str, Any]:
        """Convert a parsed element to a dictionary.
        
        Args:
            element: The parsed element
            
        Returns:
            Dictionary representation of the element
        """
        return {
            'name': element.name,
            'class': self._get_element_class(element),
            'core': element.get_core_text(),
            'original': element.get_original_text(),
            'comment': element.get_comment_text()
        }
        
    def _group_to_dict(self, group: ParsedGroup, level: int) -> dict[str, Any]:
        """Convert a parsed group to a dictionary.
        
        Args:
            group: The parsed group
            level: The nesting level
            
        Returns:
            Dictionary representation of the group
        """
        self._group_counter += 1
        
        group_dict: dict[str, Any] = {
            'id': group.id,
            'name': group.name,
            'class': self._get_group_class(group),
            'level': level,
            'line': group.line_number,
            'elements': [self._element_to_dict(e) for e in group.elements],
            'groups': []
        }
        
        return group_dict
        
    def _build_group_tree(self) -> list[dict[str, Any]]:
        """Build the group tree structure from collected groups.
        
        Returns:
            List of root group dictionaries with nested children
        """
        if not self._all_groups:
            return []
            
        # Build a map of groups by ID for quick lookup
        group_map: dict[str, ParsedGroup] = {}
        for group in self._all_groups:
            group_map[group.id] = group
            
        # Build a map of parent ID to children
        children_map: dict[str, list[ParsedGroup]] = {}
        root_groups: list[ParsedGroup] = []
        
        for group in self._all_groups:
            if group.path:
                # Get the immediate parent
                parent_name, parent_id = group.path[-1]
                if parent_id not in children_map:
                    children_map[parent_id] = []
                children_map[parent_id].append(group)
            else:
                root_groups.append(group)
                
        # Build tree recursively starting from root groups
        result: list[dict[str, Any]] = []
        for root_group in root_groups:
            group_dict = self._build_group_recursive(root_group, 1, children_map)
            result.append(group_dict)
            
        return result
        
    def _build_group_recursive(self, group: ParsedGroup,
                               level: int,
                               children_map: dict[str, list[ParsedGroup]]) -> dict[str, Any]:
        """Recursively build dictionary for a group and its children.
        
        Args:
            group: The current group
            level: The current nesting level
            children_map: Map of parent IDs to child groups
            
        Returns:
            Dictionary representation of the group with children
        """
        group_dict = self._group_to_dict(group, level)
        
        # Process children
        if group.id in children_map:
            for child in children_map[group.id]:
                child_dict = self._build_group_recursive(child, level + 1, children_map)
                group_dict['groups'].append(child_dict)
                
        return group_dict
        
    def _relation_to_dict(self, relation: GeneratedRelation) -> dict[str, Any]:
        """Convert a generated relation to a dictionary.
        
        Args:
            relation: The generated relation
            
        Returns:
            Dictionary representation of the relation
        """
        return {
            'rel_type': relation.rel_type,
            'value': relation.value,
            'origin_id': relation.origin_id,
            'dest_id': relation.dest_id,
            'source_rule': relation.source_rule
        }
        
    def _attribute_to_dict(self, attribute: GeneratedAttribute) -> dict[str, Any]:
        """Convert a generated attribute to a dictionary.
        
        Args:
            attribute: The generated attribute
            
        Returns:
            Dictionary representation of the attribute
        """
        return {
            'entity_id': attribute.entity_id,
            'attr_type': attribute.attr_type,
            'attr_value': attribute.attr_value,
            'source_rule': attribute.source_rule
        }
        
    def _add_inference_results(self, inference_results: InferenceResults) -> None:
        """Add inference results to the JSON data.
        
        Args:
            inference_results: The inference results to add
        """
        # Add relations
        for relation in inference_results.relations:
            self._data['relations'].append(self._relation_to_dict(relation))
            
        # Add attributes
        for attribute in inference_results.attributes:
            self._data['attributes'].append(self._attribute_to_dict(attribute))
            
    def _write_files_json(self) -> None:
        """Write the .files.json metadata file."""
        if not self._files_json_path:
            return
            
        files_info = {
            'stru': self._structure_file,
            'source': self._source_file,
            'json': str(self._output_path) if self._output_path else '',
            'translator': self._translator_name,
            'timestamp': datetime.now().isoformat(),
            'groups_processed': self._group_counter
        }
        
        with open(self._files_json_path, 'w', encoding='utf-8') as f:
            json.dump(files_info, f, indent=2)
            
    def close(self, inference_results: Optional[InferenceResults] = None) -> list[str]:
        """Finalize the export and write the JSON file.
        
        Args:
            inference_results: Optional inference results to include
            
        Returns:
            List of output file paths created
        """
        output_files: list[str] = []
        
        if not self._output_path:
            return output_files
            
        # Build the group tree
        self._data['groups'] = self._build_group_tree()
        
        # Add inference results if provided
        if inference_results:
            self._add_inference_results(inference_results)
            
        # Write JSON file with pretty formatting
        with open(self._output_path, 'w', encoding='utf-8') as f:
            json.dump(self._data, f, indent=2, ensure_ascii=False)
        output_files.append(str(self._output_path))
        
        # Write metadata file
        self._write_files_json()
        output_files.append(str(self._files_json_path))
        
        return output_files
