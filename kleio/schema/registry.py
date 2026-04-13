"""Schema registry for Kleio structure definitions.

This module provides a registry for loaded structure definitions with
hierarchy queries and inheritance resolution.

Key features:
- Load and cache structure definitions
- Query group containment hierarchies
- Resolve inheritance chains (source/fons)
- Cache containment queries for performance
"""
from __future__ import annotations

from pathlib import Path
from typing import Optional

from kleio.schema.models import ElementDef, GroupDef, StructureDef
from kleio.schema.loader import load_yaml_structure
from kleio.errors import ErrorAccumulator


class SchemaRegistry:
    """Registry for loaded structure definitions with hierarchy queries.
    
    The registry manages a single structure definition and provides
    efficient queries for:
    - Group and element lookup
    - Containment relationships (contained_by, subgroups)
    - Element membership (element_of, group_elements)
    - Inheritance resolution (super_groups, resolve_inheritance)
    
    All containment queries are cached for performance.
    """
    
    def __init__(self):
        """Initialize an empty registry."""
        self._structure: Optional[StructureDef] = None
        self._contained_by_cache: dict[tuple[str, str], bool] = {}
        self._not_contained_by_cache: dict[tuple[str, str], bool] = {}
        self._includes: list[str] = []
        self._include_dirs: list[Path] = []
    
    def load(self, filepath: Path, errors: Optional[ErrorAccumulator] = None) -> None:
        """Load a structure file (YAML).
        
        Args:
            filepath: Path to the YAML structure file.
            errors: Optional error accumulator for collecting errors/warnings.
            
        Raises:
            FileNotFoundError: If the file doesn't exist.
            ValueError: If the file is invalid.
        """
        filepath = Path(filepath)
        
        if errors is None:
            errors = ErrorAccumulator()
        
        # Load the structure
        self._structure = load_yaml_structure(filepath, errors)
        
        # Store the directory for include resolution
        self._include_dirs = [filepath.parent]
        
        # Clear caches
        self._contained_by_cache.clear()
        self._not_contained_by_cache.clear()
        
        # Process includes if any
        if hasattr(self._structure, '_includes'):
            self._includes = self._structure._includes
            # Process includes recursively
            self._process_includes(filepath.parent, errors)
        
        # Resolve inheritance
        self.resolve_inheritance()
        
        # Resolve element definitions for each group
        self._resolve_group_elements()
    
    @property
    def structure(self) -> Optional[StructureDef]:
        """Get the loaded structure definition."""
        return self._structure
    
    @property
    def name(self) -> str:
        """Get the structure name."""
        return self._structure.name if self._structure else ""
    
    def get_group(self, name: str) -> Optional[GroupDef]:
        """Get a group definition by name.
        
        Args:
            name: The group name.
            
        Returns:
            The GroupDef if found, None otherwise.
        """
        if self._structure is None:
            return None
        return self._structure.groups.get(name)
    
    def get_element(self, name: str) -> Optional[ElementDef]:
        """Get an element definition by name.
        
        Args:
            name: The element name.
            
        Returns:
            The ElementDef if found, None otherwise.
        """
        if self._structure is None:
            return None
        return self._structure.elements.get(name)
    
    def is_doc(self, name: str) -> bool:
        """Check if a group is the document root.
        
        Args:
            name: The group name to check.
            
        Returns:
            True if the group is the document root.
        """
        if self._structure is None:
            return False
        return self._structure.doc_name == name
    
    def all_groups(self) -> list[str]:
        """Get all group names.
        
        Returns:
            List of all group names in the structure.
        """
        if self._structure is None:
            return []
        return list(self._structure.groups.keys())
    
    def all_elements(self) -> list[str]:
        """Get all element names.
        
        Returns:
            List of all element names in the structure.
        """
        if self._structure is None:
            return []
        return list(self._structure.elements.keys())
    
    def super_groups(self, group: str) -> list[str]:
        """Get the inheritance chain (source/fons) for a group.
        
        Returns groups that this group extends, from immediate parent
        to most distant ancestor.
        
        Args:
            group: The group name.
            
        Returns:
            List of ancestor group names (not including the group itself).
        """
        if self._structure is None:
            return []
        
        result = []
        visited = set()
        current = group
        
        while True:
            group_def = self._structure.groups.get(current)
            if group_def is None:
                break
            
            source = group_def.source
            if not source:
                break
            
            # Avoid infinite loops
            if source in visited:
                break
            visited.add(source)
            
            result.append(source)
            current = source
        
        return result
    
    def super_elements(self, element: str) -> list[str]:
        """Get the inheritance chain (source/fons) for an element.
        
        Args:
            element: The element name.
            
        Returns:
            List of ancestor element names (not including the element itself).
        """
        if self._structure is None:
            return []
        
        result = []
        visited = set()
        current = element
        
        while True:
            element_def = self._structure.elements.get(current)
            if element_def is None:
                break
            
            source = element_def.source
            if not source:
                break
            
            # Avoid infinite loops
            if source in visited:
                break
            visited.add(source)
            
            result.append(source)
            current = source
        
        return result
    
    def base_class(self, group: str) -> str:
        """Get the base class (root of inheritance chain) for a group.
        
        The base class is the group that has no source/fons parent.
        If the group has no parent, it is its own base class.
        
        Args:
            group: The group name.
            
        Returns:
            The name of the base class group.
        """
        supers = self.super_groups(group)
        if supers:
            return supers[-1]
        return group
    
    def element_base_class(self, element: str) -> str:
        """Get the base class (root of inheritance chain) for an element.
        
        Args:
            element: The element name.
            
        Returns:
            The name of the base class element.
        """
        supers = self.super_elements(element)
        if supers:
            return supers[-1]
        return element
    
    def _get_parts(self, group: str) -> list[str]:
        """Get all parts (subgroups) of a group from contains, repeat, always, only."""
        group_def = self.get_group(group)
        if group_def is None:
            return []
        
        parts = list(group_def.contains)
        
        # Add from repeat (arbitrary)
        for item in group_def.repeat:
            if isinstance(item, str):
                parts.append(item)
            elif isinstance(item, (list, tuple)) and len(item) > 0:
                parts.append(item[0])
        
        # Add from always (semper)
        for item in group_def.always:
            if isinstance(item, str):
                parts.append(item)
            elif isinstance(item, (list, tuple)) and len(item) > 0:
                parts.append(item[0])
        
        # Add from only (solum)
        for item in group_def.only:
            if isinstance(item, str):
                parts.append(item)
            elif isinstance(item, (list, tuple)) and len(item) > 0:
                parts.append(item[0])
        
        return parts
    
    def contained_by(self, group: str, ancestor: str) -> bool:
        """Check if group is contained by ancestor (directly or via inheritance chain).
        
        Uses caching for performance.
        
        The algorithm (from dataDictionary.pl):
        1. If group is the doc root, return False
        2. Avoid loops (group can't contain itself)
        3. Check if group is in ancestor's contains/repeat/always/only directly
        4. Check if group's super class is in ancestor's parts
        5. Check transitively through super groups of ancestor
        
        Args:
            group: The group name to check.
            ancestor: The potential container group.
            
        Returns:
            True if group is contained by ancestor (directly or indirectly).
        """
        if self._structure is None:
            return False
        
        # Document root is not contained by anything
        if self.is_doc(group):
            return False
        
        # A group can't contain itself
        if group == ancestor:
            return False
        
        # Check cache
        cache_key = (group, ancestor)
        if cache_key in self._contained_by_cache:
            return True
        if cache_key in self._not_contained_by_cache:
            return False
        
        # Compute containment
        result = self._compute_contained_by(group, ancestor)
        
        # Cache the result
        if result:
            self._contained_by_cache[cache_key] = True
        else:
            self._not_contained_by_cache[cache_key] = True
        
        return result
    
    def _compute_contained_by(self, group: str, ancestor: str) -> bool:
        """Compute containment without caching."""
        
        # Check direct containment
        ancestor_parts = self._get_parts(ancestor)
        if group in ancestor_parts:
            return True
        
        # Check if a super class of group is in ancestor's parts
        group_supers = [group] + self.super_groups(group)
        for g in group_supers:
            if g in ancestor_parts:
                return True
        
        # Check if group is in parts of a super class of ancestor
        ancestor_supers = self.super_groups(ancestor)
        for a_super in ancestor_supers:
            super_parts = self._get_parts(a_super)
            if super_parts:  # Only check if super has parts
                for g in group_supers:
                    if g in super_parts:
                        return True
        
        return False
    
    def subgroups(self, group: str) -> list[str]:
        """Get direct subgroups of a group.
        
        Returns groups that are directly contained in the given group
        (appear in contains, repeat, always, or only lists).
        
        Args:
            group: The group name.
            
        Returns:
            List of subgroup names.
        """
        if self._structure is None:
            return []
        
        result = []
        seen = set()
        
        for potential_sub in self._structure.groups.keys():
            if potential_sub == group:
                continue
            if self._is_directly_contained_by(potential_sub, group):
                if potential_sub not in seen:
                    seen.add(potential_sub)
                    result.append(potential_sub)
        
        return result
    
    def _is_directly_contained_by(self, group: str, ancestor: str) -> bool:
        """Check if group is directly in ancestor's parts list."""
        ancestor_parts = self._get_parts(ancestor)
        return group in ancestor_parts
    
    def element_of(self, element: str, group: str) -> bool:
        """Check if element belongs to a group.
        
        An element belongs to a group if it appears in the group's
        position (locus), guaranteed (certe), or also (ceteri) lists.
        
        Args:
            element: The element name.
            group: The group name.
            
        Returns:
            True if element belongs to the group.
        """
        if self._structure is None:
            return False
        
        group_def = self._structure.groups.get(group)
        if group_def is None:
            return False
        
        return (element in group_def.position or
                element in group_def.guaranteed or
                element in group_def.also)
    
    def group_elements(self, group: str) -> list[str]:
        """Get all elements of a group (including inherited ones).
        
        Returns unique element names from position, guaranteed, and also lists,
        including elements inherited from parent groups via source/fons.
        
        Args:
            group: The group name.
            
        Returns:
            List of element names belonging to the group.
        """
        if self._structure is None:
            return []
        
        group_def = self._structure.groups.get(group)
        if group_def is None:
            return []
        
        seen = set()
        result = []
        
        # Collect from this group
        for name in group_def.position + group_def.guaranteed + group_def.also:
            if name not in seen:
                seen.add(name)
                result.append(name)
        
        # Collect from parent groups (inheritance already resolved)
        # Note: after resolve_inheritance(), inherited elements should already be present
        
        return result
    
    def resolve_inheritance(self) -> None:
        """Resolve all inheritance chains, copying missing properties from parents.
        
        For each group with a source/fons:
        - Copy position, guaranteed, also, contains from parent if not defined
        - Merge lists: child values extend parent values (unique items)
        
        For each element with a source/fons:
        - Copy properties from parent if not defined
        
        This implements the copy_fons_g and copy_fons_e algorithms from dataDictionary.pl.
        """
        if self._structure is None:
            return
        
        # Process groups in topological order (parents before children)
        group_order = self._topological_order_groups()
        
        for group_name in group_order:
            group_def = self._structure.groups.get(group_name)
            if group_def is None or not group_def.source:
                continue
            
            parent_name = group_def.source
            parent_def = self._structure.groups.get(parent_name)
            if parent_def is None:
                continue
            
            # Inherit list properties from parent
            # position and guaranteed: child overrides parent (complete replacement)
            # also, contains, repeat, always, only: merge (parent + child, unique items)
            if not group_def.position:
                group_def.position = list(parent_def.position)
            if not group_def.guaranteed:
                group_def.guaranteed = list(parent_def.guaranteed)
            group_def.also = self._merge_lists(parent_def.also, group_def.also)
            group_def.contains = self._merge_lists(parent_def.contains, group_def.contains)
            group_def.repeat = self._merge_lists(parent_def.repeat, group_def.repeat)
            group_def.always = self._merge_lists(parent_def.always, group_def.always)
            group_def.only = self._merge_lists(parent_def.only, group_def.only)
            
            # Copy scalar properties if not defined
            if not group_def.description and parent_def.description:
                group_def.description = parent_def.description
            if not group_def.idprefix and parent_def.idprefix:
                group_def.idprefix = parent_def.idprefix
            if not group_def.order and parent_def.order:
                group_def.order = parent_def.order
            if not group_def.identification and parent_def.identification:
                group_def.identification = parent_def.identification
            if not group_def.prefix and parent_def.prefix:
                group_def.prefix = parent_def.prefix
            if not group_def.suffix and parent_def.suffix:
                group_def.suffix = parent_def.suffix
            
            # Set base class
            group_def.base_class = self.base_class(group_name)
        
        # Process elements in topological order
        element_order = self._topological_order_elements()
        
        for element_name in element_order:
            element_def = self._structure.elements.get(element_name)
            if element_def is None or not element_def.source:
                continue
            
            parent_name = element_def.source
            parent_def = self._structure.elements.get(parent_name)
            if parent_def is None:
                continue
            
            # Copy scalar properties if not defined
            if not element_def.description and parent_def.description:
                element_def.description = parent_def.description
            if not element_def.type and parent_def.type:
                element_def.type = parent_def.type
            if not element_def.primary_type and parent_def.primary_type:
                element_def.primary_type = parent_def.primary_type
            if not element_def.secondary_type and parent_def.secondary_type:
                element_def.secondary_type = parent_def.secondary_type
            if element_def.order == "simplex" and parent_def.order:
                element_def.order = parent_def.order
            if not element_def.identification and parent_def.identification:
                element_def.identification = parent_def.identification
            if not element_def.prefix and parent_def.prefix:
                element_def.prefix = parent_def.prefix
            if not element_def.suffix and parent_def.suffix:
                element_def.suffix = parent_def.suffix
            if not element_def.part and parent_def.part:
                element_def.part = parent_def.part
            if not element_def.without and parent_def.without:
                element_def.without = parent_def.without
            if not element_def.signs and parent_def.signs:
                element_def.signs = parent_def.signs
            if not element_def.format and parent_def.format:
                element_def.format = parent_def.format
            if not element_def.also and parent_def.also:
                element_def.also = parent_def.also
            if not element_def.cumulate and parent_def.cumulate:
                element_def.cumulate = parent_def.cumulate
            if not element_def.only and parent_def.only:
                element_def.only = parent_def.only
            
            # Set base class
            element_def.base_class = self.element_base_class(element_name)
    
    def _merge_lists(self, parent_list: list, child_list: list) -> list:
        """Merge two lists, keeping unique items in order.
        
        Parent items come first, followed by child items that weren't in parent.
        """
        seen = set()
        result = []
        
        for item in parent_list:
            if item not in seen:
                seen.add(item)
                result.append(item)
        
        for item in child_list:
            if item not in seen:
                seen.add(item)
                result.append(item)
        
        return result
    
    def _topological_order_groups(self) -> list[str]:
        """Return group names in topological order (parents before children)."""
        if self._structure is None:
            return []
        
        # Build dependency graph
        groups = list(self._structure.groups.keys())
        
        # Compute in-degree (number of groups depending on each)
        visited = set()
        result = []
        
        def visit(name: str) -> None:
            if name in visited:
                return
            visited.add(name)
            
            group_def = self._structure.groups.get(name)
            if group_def and group_def.source:
                visit(group_def.source)
            
            result.append(name)
        
        for name in groups:
            visit(name)
        
        return result
    
    def _topological_order_elements(self) -> list[str]:
        """Return element names in topological order (parents before children)."""
        if self._structure is None:
            return []
        
        elements = list(self._structure.elements.keys())
        
        visited = set()
        result = []
        
        def visit(name: str) -> None:
            if name in visited:
                return
            visited.add(name)
            
            element_def = self._structure.elements.get(name)
            if element_def and element_def.source:
                visit(element_def.source)
            
            result.append(name)
        
        for name in elements:
            visit(name)
        
        return result
    
    def clear_cache(self) -> None:
        """Clear the containment query cache."""
        self._contained_by_cache.clear()
        self._not_contained_by_cache.clear()
    
    def _process_includes(self, base_dir: Path, errors: ErrorAccumulator) -> None:
        """Process include directives recursively.
        
        Loads included YAML files and merges their groups and elements
        into the current structure.
        
        Args:
            base_dir: Base directory for resolving relative paths.
            errors: Error accumulator for collecting errors/warnings.
        """
        if not self._includes:
            return
        
        # Track already included files to avoid cycles
        included_files: set[Path] = set()
        
        def load_include(include_path: str, current_dir: Path) -> None:
            """Load a single include file."""
            filepath = current_dir / include_path
            filepath = filepath.resolve()
            
            if filepath in included_files:
                return  # Already included
            included_files.add(filepath)
            
            if not filepath.exists():
                errors.warning(f"Include file not found: {filepath}")
                return
            
            try:
                included_structure = load_yaml_structure(filepath, errors)
                
                # Merge groups (later definitions override earlier ones)
                for name, group_def in included_structure.groups.items():
                    self._structure.groups[name] = group_def
                
                # Merge elements
                for name, element_def in included_structure.elements.items():
                    self._structure.elements[name] = element_def
                
                # Process nested includes
                if hasattr(included_structure, '_includes'):
                    for nested_include in included_structure._includes:
                        load_include(nested_include, filepath.parent)
                        
            except Exception as e:
                errors.error(f"Error loading include file {filepath}: {e}")
        
        for include_path in self._includes:
            load_include(include_path, base_dir)
    
    def _resolve_group_elements(self) -> None:
        """Resolve element definitions for all groups."""
        if self._structure is None:
            return
        
        for group_def in self._structure.groups.values():
            group_def._resolve_elements(self._structure)
