"""XML exporter for Kleio data.

This module implements XML export functionality, replacing gactoxml.pl's
XML output functionality. It generates XML output from parsed Kleio groups.

Example XML output:
    <?xml version="1.0" encoding="UTF-8"?>
    <KLEIO STRUCTURE="gacto2.str" SOURCE="test.cli" TRANSLATOR="Kleio" 
           WHEN="2024-01-01" OBS="" ORIGINAL="">
      <GROUP ID="fonte-1" NAME="fonte" CLASS="source" ORDER="1" LEVEL="1" LINE="2">
        <ELEMENT NAME="id" CLASS="id">
          <CORE>test-source</CORE>
        </ELEMENT>
        ...
      </GROUP>
    </KLEIO>
"""
from __future__ import annotations

import json
from datetime import datetime
from pathlib import Path
from typing import Optional

from lxml import etree

from kleio.export.base import Exporter
from kleio.parser.models import ParsedGroup, ParsedElement, Aspect
from kleio.schema.registry import SchemaRegistry
from kleio.inference.models import InferenceResults, GeneratedRelation, GeneratedAttribute


class XmlExporter(Exporter):
    """Exports parsed Kleio data as XML.
    
    This exporter generates XML output compatible with the format produced
    by the Prolog gactoxml.pl module. It uses lxml for efficient XML generation.
    
    Attributes:
        _root: The root KLEIO XML element
        _current_parent: The current parent XML element for nesting
        _group_stack: Stack of (group_name, xml_element) for tracking hierarchy
        _output_path: Path to the output XML file
        _files_json_path: Path to the .files.json metadata file
        _schema: The loaded schema registry
        _source_file: Path to the source .cli file
        _group_order: Counters per group type for ordering
        _all_groups: List of all collected groups for tree building
        _xml_tree: Root of the XML tree being built
    """
    
    def __init__(self):
        """Initialize the XML exporter."""
        self._root: Optional[etree._Element] = None
        self._current_parent: Optional[etree._Element] = None
        self._group_stack: list[tuple[str, etree._Element]] = []
        self._output_path: Optional[Path] = None
        self._files_json_path: Optional[Path] = None
        self._schema: Optional[SchemaRegistry] = None
        self._source_file: str = ""
        self._structure_file: str = ""
        self._group_order: dict[str, int] = {}
        self._all_groups: list[ParsedGroup] = []
        self._xml_tree: Optional[etree._ElementTree] = None
        self._group_counter: int = 0
        self._translator_name: str = "kleio-python"
        self._obs: str = ""
        
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
        self._output_path = self._output_dir / f"{base_name}.xml"
        self._files_json_path = self._output_dir / f"{base_name}.files.json"
        
        # Reset state
        self._group_order = {}
        self._all_groups = []
        self._group_counter = 0
        self._group_stack = []
        
        # Create root KLEIO element
        now = datetime.now()
        when = now.strftime("%Y-%m-%d %H:%M:%S")
        
        self._root = etree.Element('KLEIO')
        self._root.set('STRUCTURE', self._structure_file)
        self._root.set('SOURCE', source_file)
        self._root.set('TRANSLATOR', self._translator_name)
        self._root.set('WHEN', when)
        self._root.set('OBS', self._obs)
        self._root.set('ORIGINAL', '')
        
        self._current_parent = self._root
        
    def export_group(self, group: ParsedGroup) -> None:
        """Export a single completed group.
        
        Collects groups for later tree building in close().
        The actual XML generation happens in close() to ensure proper nesting.
        
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
        
    def _get_next_order(self, group_name: str) -> int:
        """Get the next order number for a group type.
        
        Args:
            group_name: The name of the group
            
        Returns:
            The next sequential order number
        """
        if group_name not in self._group_order:
            self._group_order[group_name] = 0
        self._group_order[group_name] += 1
        return self._group_order[group_name]
        
    def _build_group_xml(self, group: ParsedGroup, parent_xml: etree._Element,
                         level: int) -> etree._Element:
        """Build XML for a single group.
        
        Args:
            group: The parsed group to convert to XML
            parent_xml: The parent XML element
            level: The nesting level
            
        Returns:
            The created GROUP XML element
        """
        self._group_counter += 1
        order = self._get_next_order(group.name)
        group_class = self._get_group_class(group)
        
        # Create GROUP element
        group_elem = etree.SubElement(parent_xml, 'GROUP')
        group_elem.set('ID', group.id)
        group_elem.set('NAME', group.name)
        group_elem.set('CLASS', group_class)
        group_elem.set('ORDER', str(order))
        group_elem.set('LEVEL', str(level))
        group_elem.set('LINE', str(group.line_number))
        
        # Add built-in elements
        self._add_element_xml(group_elem, 'line', 'line', str(group.line_number))
        self._add_element_xml(group_elem, 'id', 'id', group.id)
        self._add_element_xml(group_elem, 'groupname', 'groupname', group.name)
        
        # Get ancestor ID for "inside" element
        ancestor_id = self._get_ancestor_id(group)
        self._add_element_xml(group_elem, 'inside', 'inside', ancestor_id)
        
        self._add_element_xml(group_elem, 'class', 'class', group_class)
        self._add_element_xml(group_elem, 'order', 'order', str(order))
        self._add_element_xml(group_elem, 'level', 'level', str(level))
        
        # Add user-defined elements
        for element in group.elements:
            self._add_user_element_xml(group_elem, element, group_class)
            
        return group_elem
        
    def _get_ancestor_id(self, group: ParsedGroup) -> str:
        """Get the ancestor ID for a group.
        
        Args:
            group: The parsed group
            
        Returns:
            The ancestor ID, or empty string if none
        """
        if group.path:
            # The last entry in path is the immediate parent
            return group.path[-1][1] if group.path else ''
        return ''
        
    def _add_element_xml(self, parent: etree._Element, name: str, 
                         class_name: str, core_value: str) -> None:
        """Add a simple element with only core value.
        
        Args:
            parent: The parent XML element
            name: The element name
            class_name: The element class
            core_value: The core value
        """
        elem = etree.SubElement(parent, 'ELEMENT')
        elem.set('NAME', name)
        elem.set('CLASS', class_name)
        
        core = etree.SubElement(elem, 'CORE')
        core.text = core_value
        
    def _add_user_element_xml(self, parent: etree._Element, 
                              element: ParsedElement,
                              group_class: str) -> None:
        """Add a user-defined element with all aspects.
        
        Args:
            parent: The parent XML element
            element: The parsed element
            group_class: The class of the containing group
        """
        element_class = self._get_element_class(element)
        
        elem = etree.SubElement(parent, 'ELEMENT')
        elem.set('NAME', element.name)
        elem.set('CLASS', element_class)
        
        # Add CORE aspect
        core_text = element.get_core_text()
        if core_text:
            core = etree.SubElement(elem, 'CORE')
            core.text = core_text
            
        # Add ORIGINAL aspect
        original_text = element.get_original_text()
        if original_text:
            original = etree.SubElement(elem, 'ORIGINAL')
            original.text = original_text
            
        # Add COMMENT aspect
        comment_text = element.get_comment_text()
        if comment_text:
            comment = etree.SubElement(elem, 'COMMENT')
            comment.text = comment_text
            
    def _build_xml_tree(self) -> None:
        """Build the complete XML tree from collected groups."""
        if not self._all_groups:
            return
            
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
                
        # Build XML recursively starting from root groups
        for root_group in root_groups:
            self._build_group_recursive(root_group, self._root, 1, children_map)
            
    def _build_group_recursive(self, group: ParsedGroup, 
                               parent_xml: etree._Element,
                               level: int,
                               children_map: dict[str, list[ParsedGroup]]) -> None:
        """Recursively build XML for a group and its children.
        
        Args:
            group: The current group
            parent_xml: The parent XML element
            level: The current nesting level
            children_map: Map of parent IDs to child groups
        """
        group_xml = self._build_group_xml(group, parent_xml, level)
        
        # Process children
        if group.id in children_map:
            for child in children_map[group.id]:
                self._build_group_recursive(child, group_xml, level + 1, children_map)
                
    def _add_inference_results(self, inference_results: InferenceResults) -> None:
        """Add inference results to the XML.
        
        Args:
            inference_results: The inference results to add
        """
        if not self._root:
            return
            
        # Add relations
        for relation in inference_results.relations:
            self._add_relation_xml(relation)
            
        # Add attributes
        for attribute in inference_results.attributes:
            self._add_attribute_xml(attribute)
            
    def _add_relation_xml(self, relation: GeneratedRelation) -> None:
        """Add a generated relation as a RELATION element.
        
        Args:
            relation: The generated relation
        """
        if not self._root:
            return
            
        rel_elem = etree.SubElement(self._root, 'RELATION')
        rel_elem.set('ID', f"{relation.origin_id}-{relation.value}")
        rel_elem.set('TYPE', relation.rel_type)
        rel_elem.set('VALUE', relation.value)
        rel_elem.set('ORIGIN', relation.origin_id)
        rel_elem.set('DESTINATION', relation.dest_id)
        
    def _add_attribute_xml(self, attribute: GeneratedAttribute) -> None:
        """Add a generated attribute as an ATTRIBUTE element.
        
        Args:
            attribute: The generated attribute
        """
        if not self._root:
            return
            
        attr_elem = etree.SubElement(self._root, 'ATTRIBUTE')
        attr_elem.set('ID', attribute.entity_id)
        attr_elem.set('TYPE', attribute.attr_type)
        attr_elem.set('VALUE', attribute.attr_value)
        
    def _write_files_json(self) -> None:
        """Write the .files.json metadata file."""
        if not self._files_json_path:
            return
            
        files_info = {
            'stru': self._structure_file,
            'source': self._source_file,
            'xml': str(self._output_path) if self._output_path else '',
            'translator': self._translator_name,
            'timestamp': datetime.now().isoformat(),
            'groups_processed': self._group_counter
        }
        
        with open(self._files_json_path, 'w', encoding='utf-8') as f:
            json.dump(files_info, f, indent=2)
            
    def close(self, inference_results: Optional[InferenceResults] = None) -> list[str]:
        """Finalize the export and write the XML file.
        
        Args:
            inference_results: Optional inference results to include
            
        Returns:
            List of output file paths created
        """
        output_files: list[str] = []
        
        if self._root is None or self._output_path is None:
            return output_files
            
        # Build the XML tree from collected groups
        self._build_xml_tree()
        
        # Add inference results if provided
        if inference_results:
            self._add_inference_results(inference_results)
            
        # Create the ElementTree and write to file
        tree = etree.ElementTree(self._root)
        
        # Write XML with pretty formatting
        tree.write(
            str(self._output_path),
            encoding='utf-8',
            xml_declaration=True,
            pretty_print=True
        )
        output_files.append(str(self._output_path))
        
        # Write metadata file
        self._write_files_json()
        output_files.append(str(self._files_json_path))
        
        return output_files
