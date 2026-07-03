"""Group builder for assembling parsed groups from syntax actions.

This module provides the GroupBuilder class which processes Action objects
from the syntax parser, maintains the parsing state, and produces completed
ParsedGroup objects.

Ported from dataCode.pl and dataCDS.pl in the original Prolog implementation.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Optional

from kleio.parser.models import (
    Aspect,
    Entry,
    NewGroup,
    NewElement,
    EndElement,
    NewEntry,
    NewAspect,
    StoreCore,
    ParsedElement,
    ParsedGroup,
)
from kleio.parser.lexer import tokenize_data
from kleio.parser.syntax import parse_line, QuoteState
from kleio.schema.registry import SchemaRegistry
from kleio.errors import ErrorAccumulator


@dataclass
class ParserState:
    """Maintains the current parsing state for group building.
    
    This mirrors the CDS (Current Data Storage) structure from dataCDS.pl:
    - path: ancestry chain of parent groups
    - current_group: name of the group being built
    - current_group_id: auto-generated ID for the current group
    - locus_count: counter for positional elements
    - elements: completed elements for the current group
    - current_element: name of the element being built
    - current_aspect: which aspect (core/original/comment) is active
    - core_entries, original_entries, comment_entries: entry lists for current element
    - current_core, current_original, current_comment: accumulators for current entry
    """
    path: list[tuple[str, str]] = field(default_factory=list)  # [(group_name, group_id), ...]
    current_group: str = ""
    current_group_id: str = ""
    locus_count: int = 0  # Counter for positional elements
    elements: list[ParsedElement] = field(default_factory=list)
    current_element: str = ""
    current_aspect: Aspect = Aspect.CORE
    core_entries: list[Entry] = field(default_factory=list)
    original_entries: list[Entry] = field(default_factory=list)
    comment_entries: list[Entry] = field(default_factory=list)
    current_core: list[str] = field(default_factory=list)
    current_original: list[str] = field(default_factory=list)
    current_comment: list[str] = field(default_factory=list)


class GroupBuilder:
    """Builds ParsedGroup objects from parser actions.
    
    Maintains the hierarchical parsing state and handles:
    - Group lifecycle (new, flush, close)
    - Path management (linking groups in hierarchy)
    - Element assembly (positional and named elements)
    - ID generation
    - Validation against schema
    
    This is the central orchestrator that processes Action objects from the
    syntax parser, mirroring the functionality of dataCode.pl in the original
    Prolog implementation.
    """
    
    def __init__(
        self,
        schema: SchemaRegistry,
        errors: ErrorAccumulator,
        on_group_complete: Callable[[ParsedGroup], None] | None = None,
    ):
        """
        Args:
            schema: The loaded structure schema
            errors: Error accumulator for validation errors
            on_group_complete: Callback invoked with each completed group
        """
        self.schema = schema
        self.errors = errors
        self.on_group_complete = on_group_complete
        
        # Initialize state
        self.state = ParserState()
        self.group_counters: dict[str, int] = {}
        self.completed_groups: list[ParsedGroup] = []
        self.line_number: int = 0
        self.line_text: str = ""
        
        # Initialize group counters
        self._init_group_counters()
    
    def _init_group_counters(self) -> None:
        """Initialize counters for all group types to zero.
        
        From dataCode.pl initGroupCounters/0.
        """
        if self.schema.structure:
            for group_name in self.schema.structure.groups.keys():
                self.group_counters[group_name] = 0
    
    def _reset_group_counters(self, group: str) -> None:
        """Reset counters of subgroups when a group is encountered.
        
        From dataCode.pl resetGroupCounters/1. If a subgroup has
        identificatio=sic, its counter is NOT reset.
        
        Args:
            group: The group whose subgroups should have counters reset.
        """
        subgroups = self.schema.subgroups(group)
        for subgroup in subgroups:
            group_def = self.schema.get_group(subgroup)
            if group_def and group_def.identification != "sic":
                self.group_counters[subgroup] = 0
    
    def _inc_group_count(self, group: str) -> int:
        """Increment the counter for a group and return the new count.
        
        From dataCode.pl inc_group_count/3.
        
        Args:
            group: The group name.
            
        Returns:
            The new counter value.
        """
        current = self.group_counters.get(group, 0)
        new_count = current + 1
        self.group_counters[group] = new_count
        return new_count
    
    def set_context(self, line_number: int, line_text: str) -> None:
        """Set the current file/line context for error reporting.
        
        Args:
            line_number: Current line number.
            line_text: Current line text content.
        """
        self.line_number = line_number
        self.line_text = line_text
        self.errors.set_context(line_number=line_number, line_text=line_text)
    
    def process_actions(self, actions: list) -> None:
        """Execute a batch of actions from one line.
        
        Args:
            actions: List of Action objects from parse_line().
        """
        for action in actions:
            if isinstance(action, NewGroup):
                self._new_group(action.name)
            elif isinstance(action, NewElement):
                self._new_element(action.name)
            elif isinstance(action, EndElement):
                self._end_element()
            elif isinstance(action, NewEntry):
                self._new_entry()
            elif isinstance(action, NewAspect):
                self._new_aspect(action.aspect)
            elif isinstance(action, StoreCore):
                self._store_core(action.value)
    
    def _new_group(self, name: str) -> None:
        """Called when NewGroup action is encountered.
        
        Flushes the previous group, updates the path, and initializes
        state for the new group.
        
        Args:
            name: The group name.
        """
        # Capture current group info before flushing
        old_group = self.state.current_group
        old_path = self.state.path[:]
        
        # Flush previous group if any - this generates the ID for the old group
        # After flush, current_group is cleared but current_group_id still holds the ID
        self._flush_group()
        
        # Get the ID that was generated during flush
        old_id = self.state.current_group_id
        
        # Update path using the captured group info
        new_path = self._update_path(old_group, old_id, name, old_path)
        
        # Reset counters for subgroups
        self._reset_group_counters(name)
        
        # Reset state for new group
        self.state = ParserState(
            path=new_path,
            current_group=name,
            current_group_id="",
            locus_count=0,
            elements=[],
            current_element="",
            current_aspect=Aspect.CORE,
            core_entries=[],
            original_entries=[],
            comment_entries=[],
            current_core=[],
            current_original=[],
            current_comment=[],
        )
    
    def _update_path(
        self,
        old_group: str,
        old_id: str,
        new_group: str,
        old_path: list[tuple[str, str]],
    ) -> list[tuple[str, str]]:
        """Update the path when a new group is encountered.
        
        This is the critical algorithm from dataCode.pl lines 217-267.
        It determines where a new group fits in the hierarchical path.
        
        Algorithm:
        a. If old_group == new_group: path unchanged (sibling)
        b. If new_group is a document (is_doc): path becomes empty
        c. If old_group is a doc and new_group is contained by it: path = [(doc, id)]
        d. If new_group is contained_by old_group: append old_group to path
        e. If old_group exists and is not a doc: append old_group to path
           (groups appearing sequentially inherit the path context)
        f. Otherwise, walk path from bottom to top, find deepest ancestor 
           that contains new_group, cut path at that point
        g. Also check via base classes: if base class of new_group is contained
           by base class of an ancestor
        h. Check for recursion: if new_group is already in the path, fail
        
        Args:
            old_group: The previous group name.
            old_id: The previous group ID.
            new_group: The new group name.
            old_path: The current path.
            
        Returns:
            The updated path.
        """
        # b. If new_group is a document: path becomes empty
        if self.schema.is_doc(new_group):
            return []
        
        # c. If old_group is a doc and new_group is contained by it
        if old_group and self.schema.is_doc(old_group):
            if self.schema.contained_by(new_group, old_group):
                return [(old_group, old_id)]
        
        # d. If new_group is contained_by old_group: append old_group to path
        if old_group and self.schema.contained_by(new_group, old_group):
            return old_path + [(old_group, old_id)]
        
        # a. If old_group == new_group: path unchanged (sibling)
        # This check comes after containment check to ensure proper nesting
        if old_group == new_group:
            return old_path[:]
        
        # e. If old_group exists and is not a doc, check for path inheritance
        # This handles cases where groups appear sequentially in source
        if old_group and not self.schema.is_doc(old_group):
            # Check if new_group can be a child of old_group's ancestors
            for i in range(len(old_path) - 1, -1, -1):
                ancestor_name, ancestor_id = old_path[i]
                if self.schema.contained_by(new_group, ancestor_name):
                    return old_path[: i + 1]
            
            # If new_group is the same type as old_group, it's a sibling
            if old_group == new_group:
                return old_path[:]
            
            # If new_group is not contained by old_group and not a sibling,
            # check if old_group could be a parent (even without schema containment)
            # Only do this if old_path is empty (top-level nesting)
            if not old_path:
                return [(old_group, old_id)]
            
            # Otherwise, cut path back - new_group is not related to old_group
            return []
        
        # f. Walk path from bottom to top to find deepest ancestor
        # that contains new_group
        for i in range(len(old_path) - 1, -1, -1):
            ancestor_name, ancestor_id = old_path[i]
            if self.schema.contained_by(new_group, ancestor_name):
                # Cut path at this ancestor (keep up to and including this ancestor)
                return old_path[: i + 1]
        
        # g. Check via base classes
        new_base = self.schema.base_class(new_group)
        for i in range(len(old_path) - 1, -1, -1):
            ancestor_name, _ = old_path[i]
            ancestor_base = self.schema.base_class(ancestor_name)
            if self.schema.contained_by(new_base, ancestor_base):
                return old_path[: i + 1]
        
        # h. Check for recursion - new_group should not already be in path
        for group_name, _ in old_path:
            if group_name == new_group:
                self.errors.error(
                    f"Recursive group containment detected: {new_group} is already in path",
                    line_number=self.line_number,
                    line_text=self.line_text,
                )
                return old_path[:]
        
        # Default: start fresh
        return []
    
    def _flush_group(self) -> None:
        """Finalize the current group.
        
        From dataCode.pl flushGroup/0. Ends the current element, generates
        an ID, checks guaranteed elements, creates the ParsedGroup, and
        invokes the callback.
        """
        if not self.state.current_group:
            return
        
        # End current element
        self._end_element()
        
        # Generate ID
        group_id = self._make_id()
        self.state.current_group_id = group_id
        
        # Check guaranteed elements
        self._check_elements(self.state.current_group, group_id)
        
        # Create ParsedGroup
        group = ParsedGroup(
            name=self.state.current_group,
            id=group_id,
            elements=self.state.elements[:],
            path=self.state.path[:],
            line_number=self.line_number,
            line_text=self.line_text,
            level=len(self.state.path),
        )
        
        # Store and callback
        self.completed_groups.append(group)
        if self.on_group_complete:
            self.on_group_complete(group)
        
        # Clear current group to prevent double-flushing
        self.state.current_group = ""
    
    def _make_id(self) -> str:
        """Generate an ID for the current group.
        
        From dataCDS.pl makeID/1. Uses the group's idprefix (signum) plus
        an auto-incremented counter, or an element with identificatio=sic.
        
        Returns:
            The generated ID string.
        """
        group_name = self.state.current_group
        group_def = self.schema.get_group(group_name)
        
        if group_def is None:
            return ""
        
        # Check if there's an element with identificatio=sic that should be the ID
        for element in self.state.elements:
            element_def = self.schema.get_element(element.name)
            if element_def and element_def.identification == "sic":
                # Use the core text of this element as the ID
                core_text = element.get_core_text()
                if core_text:
                    return core_text
        
        # Use counter-based ID
        idprefix = group_def.idprefix or group_name
        count = self._inc_group_count(group_name)
        return f"{idprefix}-{count}"
    
    def _check_elements(self, group: str, group_id: str) -> None:
        """Verify that guaranteed (certe) elements are present.
        
        From dataCode.pl check_elements/2. Warns if required elements
        are missing from the group.
        
        Args:
            group: The group name.
            group_id: The group ID.
        """
        group_def = self.schema.get_group(group)
        if group_def is None:
            return
        
        # Get list of element names present in the group
        present_elements = {el.name for el in self.state.elements}
        
        # Check guaranteed elements
        missing = []
        for required in group_def.guaranteed:
            if required not in present_elements:
                # Check if element extends a superclass that is present
                element_def = self.schema.get_element(required)
                if element_def and element_def.source:
                    if element_def.source in present_elements:
                        continue
                missing.append(required)
        
        if missing:
            missing_str = ", ".join(missing)
            self.errors.error(
                f"Missing element(s) in {group}({group_id}): must have {missing_str}",
                line_number=self.line_number,
                line_text=self.line_text,
            )
    
    def _end_element(self) -> None:
        """Finalize the current element.
        
        From dataCode.pl endElement/0. Ends the current entry, resolves
        implicit element names from the position/locus list, and adds
        the element to the group's element list.
        """
        # End current entry first
        self._end_entry()
        
        # If no current element name, use positional element from schema
        if not self.state.current_element:
            self._resolve_positional_element()
        
        # If we have an element name and entries, store it
        if self.state.current_element:
            element = ParsedElement(
                name=self.state.current_element,
                core_entries=self.state.core_entries[:],
                original_entries=self.state.original_entries[:],
                comment_entries=self.state.comment_entries[:],
            )
            self.state.elements.append(element)
            
            # Reset element state
            self.state.current_element = ""
            self.state.core_entries = []
            self.state.original_entries = []
            self.state.comment_entries = []
            self.state.current_aspect = Aspect.CORE
    
    def _resolve_positional_element(self) -> None:
        """Resolve implicit element name from position/locus list.
        
        From dataCode.pl check_ename/0. When no explicit element name
        is given, use the next element from the group's position list.
        """
        group_def = self.schema.get_group(self.state.current_group)
        if group_def is None:
            return
        
        # Get position list
        position_list = group_def.position
        if not position_list:
            return
        
        # Increment locus count and get next element
        self.state.locus_count += 1
        index = self.state.locus_count - 1  # 0-based index
        
        if index < len(position_list):
            self.state.current_element = position_list[index]
        else:
            # No more positional elements available
            self.errors.error(
                f"Undefined element in {self.state.current_group}: "
                f"no more positional elements available",
                line_number=self.line_number,
                line_text=self.line_text,
            )
            self.state.current_element = "UNDEFINED"
    
    def _end_entry(self) -> None:
        """Finalize the current entry.
        
        From dataCode.pl endEntry/1. Strips leading spaces from current
        accumulators and creates Entry objects.
        """
        # Process core entries
        if self.state.current_core:
            # Strip leading spaces
            values = self._strip_leading_spaces(self.state.current_core)
            if values:
                self.state.core_entries.append(Entry(values=values))
            self.state.current_core = []
        
        # Process original entries
        if self.state.current_original:
            values = self._strip_leading_spaces(self.state.current_original)
            if values:
                self.state.original_entries.append(Entry(values=values))
            self.state.current_original = []
        
        # Process comment entries
        if self.state.current_comment:
            values = self._strip_leading_spaces(self.state.current_comment)
            if values:
                self.state.comment_entries.append(Entry(values=values))
            self.state.current_comment = []
    
    def _strip_leading_spaces(self, values: list[str]) -> list[str]:
        """Strip leading spaces from a list of string values.
        
        From dataCode.pl rmv_lead_space/2.
        
        Args:
            values: List of string values.
            
        Returns:
            List with leading spaces removed from first value.
        """
        if not values:
            return values
        
        result = values[:]
        # Remove leading spaces from first value
        while result and result[0] == " ":
            result.pop(0)
        
        return result
    
    def _new_element(self, name: str) -> None:
        """Called when NewElement action is encountered.
        
        Validates the element name against the schema and sets it as
        the current element.
        
        Args:
            name: The element name.
        """
        # Validate element belongs to current group
        self._verify_element(name)
        
        # End any previous element
        if self.state.current_element or self.state.current_core:
            self._end_element()
        
        self.state.current_element = name
    
    def _verify_element(self, name: str) -> bool:
        """Check if an element belongs to the current group.
        
        From dataCode.pl verify_element/1 and velement/2. Checks if the
        element is in the group's element lists, or if it extends a
        superclass that is in the group.
        
        Args:
            name: The element name.
            
        Returns:
            True if valid, False otherwise (error already recorded).
        """
        group = self.state.current_group
        if not group:
            return False
        
        # Check direct membership
        if self.schema.element_of(name, group):
            return True
        
        # Check if element extends a superclass that is in the group
        element_def = self.schema.get_element(name)
        if element_def and element_def.source:
            if self.schema.element_of(element_def.source, group):
                return True
        
        # Element not found - record warning but allow it
        self.errors.warning(
            f"{group}: unknown element: {name}",
            line_number=self.line_number,
            line_text=self.line_text,
        )
        return False
    
    def _new_entry(self) -> None:
        """Called when NewEntry action is encountered.
        
        Ends the current entry and starts a new one.
        """
        self._end_entry()
    
    def _new_aspect(self, aspect: Aspect) -> None:
        """Called when NewAspect action is encountered.
        
        Ends the current entry and switches to a new aspect.
        
        Args:
            aspect: The new aspect (ORIGINAL or COMMENT).
        """
        self._end_entry()
        self.state.current_aspect = aspect
    
    def _store_core(self, value: str) -> None:
        """Called when StoreCore action is encountered.
        
        Appends the value to the current aspect's accumulator.
        
        Args:
            value: The value to store.
        """
        if self.state.current_aspect == Aspect.CORE:
            self.state.current_core.append(value)
        elif self.state.current_aspect == Aspect.ORIGINAL:
            self.state.current_original.append(value)
        elif self.state.current_aspect == Aspect.COMMENT:
            self.state.current_comment.append(value)
    
    def close(self) -> None:
        """Called at end of file to flush the final group."""
        self._flush_group()


def translate_file(
    source_path: str | Path,
    schema: SchemaRegistry,
    errors: ErrorAccumulator,
    on_group: Callable[[ParsedGroup], None] | None = None,
    on_line: Callable[[int, str], None] | None = None,
) -> list[ParsedGroup]:
    """Translate a .cli file to a list of ParsedGroup objects.

    This is the main entry point that wires together:
    lexer -> syntax parser -> group builder

    Args:
        source_path: Path to the .cli file to translate.
        schema: The loaded structure schema.
        errors: Error accumulator for validation errors.
        on_group: Optional callback invoked with each completed group.
        on_line: Optional callback invoked with (line_number, line_text) for
            each non-empty source line. Used to echo source lines into the
            translation report when echo is enabled.

    Returns:
        List of completed ParsedGroup objects.
    """
    source_path = Path(source_path)

    # Create builder
    builder = GroupBuilder(schema, errors, on_group_complete=on_group)

    # Read and process file line by line
    quote_state = QuoteState()

    with open(source_path, "r", encoding="utf-8") as f:
        for line_number, line in enumerate(f, start=1):
            # Remove trailing newline but preserve other whitespace
            line = line.rstrip("\n\r")

            # Skip empty lines
            if not line.strip():
                continue

            # Echo the source line to the report (no-op unless echo=yes).
            if on_line:
                on_line(line_number, line)

            # Set context for error reporting
            builder.set_context(line_number, line)

            # Tokenize
            tokens = tokenize_data(line)

            # Parse tokens into actions
            actions = parse_line(tokens, quote_state)

            # Process actions
            builder.process_actions(actions)

    # Close builder to flush final group
    builder.close()

    return builder.completed_groups


def translate_string(
    source: str,
    schema: SchemaRegistry,
    errors: ErrorAccumulator,
    on_group: Callable[[ParsedGroup], None] | None = None,
    source_name: str = "<string>",
) -> list[ParsedGroup]:
    """Translate a string containing Kleio notation to ParsedGroup objects.
    
    Similar to translate_file but operates on a string instead of a file.
    Useful for testing and inline translation.
    
    Args:
        source: The Kleio notation source string.
        schema: The loaded structure schema.
        errors: Error accumulator for validation errors.
        on_group: Optional callback invoked with each completed group.
        source_name: Name to use for error reporting (default "<string>").
        
    Returns:
        List of completed ParsedGroup objects.
    """
    # Create builder
    builder = GroupBuilder(schema, errors, on_group_complete=on_group)
    
    # Process lines
    quote_state = QuoteState()
    
    for line_number, line in enumerate(source.split("\n"), start=1):
        # Remove trailing newline but preserve other whitespace
        line = line.rstrip("\n\r")
        
        # Skip empty lines
        if not line.strip():
            continue
        
        # Set context for error reporting
        builder.set_context(line_number, line)
        errors.set_context(file=source_name, line_number=line_number, line_text=line)
        
        # Tokenize
        tokens = tokenize_data(line)
        
        # Parse tokens into actions
        actions = parse_line(tokens, quote_state)
        
        # Process actions
        builder.process_actions(actions)
    
    # Close builder to flush final group
    builder.close()
    
    return builder.completed_groups
