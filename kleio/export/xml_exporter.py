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

from kleio.dates import parse_date, date_extra_info_json
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
        # Stack of enclosing act contexts (id, group_name, date) for
        # function-in-act relation generation. Pushed when entering an
        # act-descendant group, popped when leaving.
        self._act_context_stack: list[tuple[str, str, str]] = []
        # Counter for auto-generated relation ids (gensymbol_local(rela,...)).
        self._rel_counter: int = 0
        # Optional mapping store for group→class and element→baseclass
        # resolution (ports mappings.pl). When None, falls back to the
        # schema's base_class / element_base_class.
        self._mapping_store = None
        
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
                - mapping_store: Optional MappingStore for group→class resolution
        """
        self._source_file = source_file
        self._schema = schema
        self._output_dir = Path(output_dir)

        # Get optional parameters
        self._structure_file = kwargs.get('structure_file', schema.name if schema else '')
        self._translator_name = kwargs.get('translator', 'kleio-python')
        self._obs = kwargs.get('obs', '')
        self._mapping_store = kwargs.get('mapping_store')
        
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
        """Get the database class for a group.

        Resolution order (mirrors rch_class / group→class mapping from
        source_db_mappings_semantics.md):
        1. The mapping store with inheritance fallback (e.g. ``bap`` →
           ``act`` via ``bap → pt-acto → historical-act``).
        2. The schema's base_class (the root of the source/fons chain).
        3. The group name itself.

        Args:
            group: The parsed group

        Returns:
            The resolved class name.
        """
        # 1. Mapping store with inheritance fallback.
        if self._mapping_store and self._schema:
            mapped = self._mapping_store.resolve_class_for_group(
                group.name, self._schema
            )
            if mapped:
                return mapped
        # 2. Schema base class fallback.
        if self._schema:
            base = self._schema.base_class(group.name)
            if base:
                return base
        return group.name

    def _get_element_class(self, element: ParsedElement) -> str:
        """Get the base class for an element.

        Resolution order:
        1. The mapping store's class definition for the current group's
           class - if it has an attribute matching this element name,
           return the attribute's ``baseclass``.
        2. The schema's element_base_class (root of the element's source
           chain).
        3. The element name itself.

        Args:
            element: The parsed element

        Returns:
            The resolved base class name.
        """
        # 1. Check the mapping class definition for a matching attribute.
        if self._mapping_store and self._schema:
            # We need the current group's class to look up the class def.
            # The element's baseclass comes from the class definition's
            # attribute list. We check by element name first, then by
            # walking the element's source chain.
            elem_base = self._schema.element_base_class(element.name)
            # Try to find this baseclass in any class definition's attributes.
            # This is a simplified lookup; the full Prolog resolves via
            # the group's class specifically.
            for class_def in self._mapping_store._class_definitions.values():
                for attr in class_def.get('attributes', []):
                    if attr.get('baseclass') == elem_base or attr.get('name') == element.name:
                        return attr.get('baseclass', elem_base)
        # 2. Schema element base class fallback.
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

        # Add synthesized elements (port gactoxml.pl element inference).
        self._add_synthesized_elements(group_elem, group, group_class)

        return group_elem
        
    def _add_synthesized_elements(
        self, group_elem: etree._Element, group: ParsedGroup, group_class: str
    ) -> None:
        """Add inferred/synthesized elements to a group's XML.

        Mirrors the Prolog exporter's element-inference logic:
        - ``date`` on acts/sources: from explicit date/data element, or
          day/month/year combination, or 0 (gactoxml.pl:704-716).
        - ``date_extra_info`` on attributes: JSON describing the parsed date,
          or ``{}`` when the date is inherited/absent (gactoxml.pl:768-815).
        - ``type`` on acts/objects/geoentities: the group name
          (gactoxml.pl:726,747,758).
        - ``sex`` on persons: from explicit sex element, or the group's
          male/female ancestry (gactoxml.pl:1155-1168).

        Each is only added if the group doesn't already have an explicit
        element of that base class.
        """
        # Determine the set of element base-classes and literal names present.
        existing_bases: set[str] = set()
        existing_names: set[str] = set()
        for el in group.elements:
            existing_names.add(el.name)
            if self._schema:
                existing_bases.add(self._schema.element_base_class(el.name))
            else:
                existing_bases.add(el.name)

        # Resolve the group's schema base class for type-specific synthesis.
        schema_base = self._schema.base_class(group.name) if self._schema else group.name

        # --- date element (acts, sources) ---
        # The Prolog always emits the synthesized date; it does NOT suppress
        # it when a date-baseclass element (e.g. 'data') exists. So we check
        # for the literal name 'date' only.
        if schema_base in ('historical-act', 'historical-source') and 'date' not in existing_names:
            date_val = self._compute_act_date(group)
            self._add_element_xml(group_elem, 'date', 'date', date_val)

        # --- date_extra_info + implicit date (attributes) ---
        # The Prolog attribute_export (gactoxml.pl:768-815) always emits
        # id/entity/date_extra_info on attribute groups. If the attribute
        # has an explicit date element, it is parsed into the JSON; otherwise
        # the date is inherited from the enclosing act (DateType=implicit),
        # a date element is synthesized, and date_extra_info is "{}".
        if schema_base == 'attribute':
            self._add_attribute_date_info(group_elem, group)

        # --- type element (acts, objects, geoentities) ---
        # Same: check literal name, not baseclass.
        if schema_base in ('historical-act', 'object', 'geoentity') and 'type' not in existing_names:
            self._add_element_xml(group_elem, 'type', 'type', group.name)

        # --- sex element (persons) ---
        # Prolog checks member(sex, Els) where Els is the literal element
        # names — so a group with a 'sexo' element (not 'sex') still gets
        # the synthesized 'sex' element (gactoxml.pl:737-738).
        existing_names = {el.name for el in group.elements}
        if schema_base == 'person' and 'sex' not in existing_names:
            sex_val = self._infer_sex(group)
            self._add_element_xml(group_elem, 'sex', 'sex', sex_val)

    def _add_attribute_date_info(
        self, group_elem: etree._Element, group: ParsedGroup
    ) -> None:
        """Emit ``date_extra_info`` (and implicit ``date``) on attribute groups.

        Ports ``attribute_export/2`` (gactoxml.pl:768-815):

        1. If the attribute has an explicit date element (base class
           ``date``), parse it and emit ``date_extra_info`` with the JSON.
        2. Otherwise, if there is an enclosing act with a date, emit an
           implicit ``date`` element with the act's date value, and
           ``date_extra_info`` as ``{}``.
        3. Otherwise emit ``date_extra_info`` as ``{}``.
        """
        # 1. Look for an explicit date element in this attribute.
        explicit_date_text = None
        for el in group.elements:
            el_base = self._schema.element_base_class(el.name) if self._schema else el.name
            if el_base == 'date':
                core = el.get_core_text().strip()
                if core:
                    explicit_date_text = core
                    break

        if explicit_date_text is not None:
            # Explicit date: parse and emit JSON.
            json_str = date_extra_info_json(explicit_date_text)
            self._add_element_xml(group_elem, 'date_extra_info', 'undef', json_str)
        else:
            # No explicit date: inherit from enclosing act if any.
            if self._act_context_stack:
                _, _, act_date = self._act_context_stack[-1]
                if act_date and act_date != '0':
                    self._add_element_xml(group_elem, 'date', 'date', act_date)
            # date_extra_info is "{}" for inherited/absent dates.
            self._add_element_xml(group_elem, 'date_extra_info', 'undef', '{}')



    def _infer_sex(self, group: ParsedGroup) -> str:
        """Infer the sex of a person group.

        Mirrors infer_sex/2 (gactoxml.pl:1155-1168):
        1. If the group has an explicit sex element (base class ``sex``),
           use its value.
        2. Else if the group name or any ancestor in its source chain is
           ``male`` → ``m``, ``female`` → ``f``.
        3. Else ``?``.
        """
        # 1. Explicit sex element.
        for el in group.elements:
            el_base = self._schema.element_base_class(el.name) if self._schema else el.name
            if el_base == 'sex':
                val = el.get_core_text().strip()
                if val:
                    return val

        # 2. Group name or ancestry.
        if self._schema:
            chain = [group.name] + self._schema.super_groups(group.name)
            for ancestor in chain:
                if ancestor == 'male':
                    return 'm'
                if ancestor == 'female':
                    return 'f'

        return '?'

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

        Tracks the enclosing act context so that person/object groups can
        generate ``function-in-act`` relation children (mirrors
        ``process_function_in_act/2`` in gactoxml.pl:962-1005).

        Args:
            group: The current group
            parent_xml: The parent XML element
            level: The current nesting level
            children_map: Map of parent IDs to child groups
        """
        group_xml = self._build_group_xml(group, parent_xml, level)

        # Determine this group's base class for act/person/object checks.
        group_base = self._schema.base_class(group.name) if self._schema else group.name

        # Track enclosing act context. When we enter an act-descendant
        # group, push its (id, group_name, date) so descendant persons/
        # objects can generate function-in-act relations.
        is_act = group_base == 'historical-act'
        if is_act:
            act_date = self._compute_act_date(group)
            self._act_context_stack.append((group.id, group.name, act_date))

        # If this is a person/object inside an act, generate a
        # function-in-act relation child (gactoxml.pl:743,750,764).
        if group_base in ('person', 'object', 'geoentity') and self._act_context_stack:
            self._generate_function_in_act_relation(group, group_xml, level)

        # Process real children
        if group.id in children_map:
            for child in children_map[group.id]:
                self._build_group_recursive(child, group_xml, level + 1, children_map)

        # Pop act context on the way out.
        if is_act:
            self._act_context_stack.pop()

    def _compute_act_date(self, group: ParsedGroup) -> str:
        """Compute a normalized date string for an act group.

        Simplified port of get_date/get_y_m_d (gactoxml.pl:704-716,
        1202-1209, 1441-1446). Priority:
        1. An explicit date element (base class ``date`` - catches both
           ``date`` and ``data``). If it's already numeric, use verbatim.
        2. Day/month/year elements (base classes ``day``/``month``/``year``)
           combined as ``YYYYMMDD``.
        3. ``0`` if nothing parseable.

        Args:
            group: The act group.

        Returns:
            An 8-digit date string (or ``0``).
        """
        # 1. Explicit date element. Route through parse_date so ranges
        #    (from:to) and relative (>date) dates yield the sortable value
        #    rather than the raw text (gactoxml.pl:1202-1204, match_date/3).
        for el in group.elements:
            el_base = self._schema.element_base_class(el.name) if self._schema else el.name
            if el_base == 'date':
                core = el.get_core_text().strip()
                if core:
                    parsed = parse_date(core)
                    if parsed.type != 'error':
                        return parsed.value
                    # Fall back to raw text for unparseable dates.
                    return core

        # 2. Day/month/year combination.
        day = month = year = None
        for el in group.elements:
            el_base = self._schema.element_base_class(el.name) if self._schema else el.name
            core = el.get_core_text().strip()
            if el_base == 'day' and core:
                try:
                    day = int(core)
                except ValueError:
                    pass
            elif el_base == 'month' and core:
                try:
                    month = int(core)
                except ValueError:
                    pass
            elif el_base == 'year' and core:
                try:
                    year = int(core)
                except ValueError:
                    pass
        if year is not None:
            return f"{year * 10000 + (month or 0) * 100 + (day or 0)}"

        return "0"

    def _generate_function_in_act_relation(
        self, person_group: ParsedGroup, parent_xml: etree._Element, level: int
    ) -> None:
        """Generate a ``function-in-act`` relation child for a person/object.

        Mirrors ``process_function_in_act/2`` (gactoxml.pl:962-1005). The
        relation links the person (origin) to the enclosing act (destination)
        with type ``function-in-act`` and value = the person's group name
        (the role they play in the act).

        Args:
            person_group: The person/object group.
            parent_xml: The person's XML element (the relation's parent).
            level: The person's level (relation will be at level + 1).
        """
        if not self._act_context_stack:
            return

        act_id, act_group, act_date = self._act_context_stack[-1]
        self._rel_counter += 1
        # Prolog uses gensymbol_local(rela, Rid) which produces rela1, rela2, ...
        # The id is built from the ACT's id (get_ancestor), not the person's id
        # (gactoxml.pl:963-964,975: AncID-Rid).
        rel_id = f"{act_id}-rela{self._rel_counter}"
        rel_level = level + 1
        rel_order = self._get_next_order('relation')

        # Build the relation GROUP element.
        rel_elem = etree.SubElement(parent_xml, 'GROUP')
        rel_elem.set('ID', rel_id)
        rel_elem.set('NAME', 'relation')
        rel_elem.set('CLASS', 'relation')
        rel_elem.set('ORDER', str(rel_order))
        rel_elem.set('LEVEL', str(rel_level))
        rel_elem.set('LINE', str(person_group.line_number))

        # Elements (gactoxml.pl:977-1001).
        self._add_element_xml(rel_elem, 'line', 'line', str(person_group.line_number))
        self._add_element_xml(rel_elem, 'groupname', 'groupname', 'relation')
        self._add_element_xml(rel_elem, 'inside', 'inside', person_group.id)
        self._add_element_xml(rel_elem, 'class', 'class', 'relation')
        self._add_element_xml(rel_elem, 'order', 'order', str(rel_order))
        self._add_element_xml(rel_elem, 'level', 'level', str(rel_level))
        self._add_element_xml(rel_elem, 'type', 'type', 'function-in-act')
        self._add_element_xml(rel_elem, 'value', 'value', person_group.name)
        self._add_element_xml(rel_elem, 'destname', 'destname', act_group)
        self._add_element_xml(rel_elem, 'origin', 'origin', person_group.id)
        self._add_element_xml(rel_elem, 'destination', 'destination', act_id)
        self._add_element_xml(rel_elem, 'id', 'id', rel_id)
        self._add_element_xml(rel_elem, 'date', 'date', act_date)
                
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

    def _emit_class_definitions(self) -> None:
        """Emit ``<CLASS>`` definition blocks before the ``<GROUP>`` elements.

        Mirrors ``ensure_class/5`` (gactoxml.pl:2197-2222): one block per
        distinct non-entity database class used by a group in this file,
        emitted in first-use order with super-classes first. Each block
        carries ``<ATTRIBUTE>`` children from the mapping definition.

        The ``GROUP`` attribute on each ``<CLASS>`` is the original Kleio
        group name that first triggered the class (e.g. ``fonte`` for
        ``source``).
        """
        if not self._mapping_store or self._root is None:
            return

        emitted: set[str] = set()
        # Map class_name -> the first group_name that used it (for the
        # GROUP attribute on the <CLASS> element).
        class_to_group: dict[str, str] = {}

        # Determine which classes are used by scanning all collected groups.
        for group in self._all_groups:
            cls = self._get_group_class(group)
            if cls and cls not in class_to_group:
                class_to_group[cls] = group.name

        # Also account for auto-generated groups (function-in-act relations,
        # inference results) that are created during tree-building, not in
        # the collected _all_groups list. If any person/object group exists
        # inside an act, function-in-act relations WILL be generated.
        has_persons_in_acts = any(
            self._schema and
            self._schema.base_class(g.name) in ('person', 'object', 'geoentity')
            and any(
                self._schema.base_class(p_name) == 'historical-act'
                for p_name, _ in g.path
            )
            for g in self._all_groups
        ) if self._schema else False
        if has_persons_in_acts and 'relation' not in class_to_group:
            class_to_group['relation'] = 'relation'

        def _ensure_super(class_name: str) -> None:
            """Recursively emit super-class blocks first."""
            if class_name in emitted or class_name == 'entity':
                return
            class_def = self._mapping_store.get_class_definition(class_name)
            if class_def is None:
                return
            super_name = class_def.get('super', '')
            if super_name:
                _ensure_super(super_name)

            # Emit this class.
            group_name = class_to_group.get(class_name, '')
            class_elem = etree.SubElement(self._root, 'CLASS')
            class_elem.set('NAME', class_name)
            class_elem.set('SUPER', super_name)
            class_elem.set('TABLE', class_def.get('table', ''))
            class_elem.set('GROUP', group_name)
            for attr in class_def.get('attributes', []):
                attr_elem = etree.SubElement(class_elem, 'ATTRIBUTE')
                attr_elem.set('NAME', attr.get('name', ''))
                attr_elem.set('COLUMN', attr.get('column', ''))
                attr_elem.set('CLASS', attr.get('baseclass', ''))
                attr_elem.set('TYPE', attr.get('coltype', ''))
                attr_elem.set('SIZE', str(attr.get('colsize', '')))
                attr_elem.set('PRECISION', str(attr.get('colprecision', '')))
                attr_elem.set('PKEY', str(attr.get('pkey', '')))
            emitted.add(class_name)

        for class_name in class_to_group:
            _ensure_super(class_name)

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

        # Emit <CLASS> definition blocks before the <GROUP> elements
        # (mirrors ensure_class/5 in gactoxml.pl:2197-2222). One block per
        # distinct non-entity class used in the file, super-classes first.
        self._emit_class_definitions()

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
