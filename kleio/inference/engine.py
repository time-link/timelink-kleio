"""Inference engine for Kleio rule matching.

The inference engine matches rules against the hierarchical group structure
from parsed .cli files. Rules define patterns like:

    if [sequence(_), extends(actorm, N), pai(P)] 
    then relation(parentesco, pai, P, N)

This generates a father relationship when a 'pai' group is found within
an 'actorm' (male actor) group.
"""
from __future__ import annotations

from typing import Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from pathlib import Path
    from ..parser.models import ParsedGroup
    from ..schema.models import StructureDef

from .models import (
    InferenceRule,
    InferenceResults,
    Condition,
    ConditionType,
    Action,
    ActionType,
    GeneratedRelation,
    GeneratedAttribute,
)


class InferenceEngine:
    """Engine for applying inference rules to parsed Kleio data.
    
    The engine walks the group hierarchy tree and tries to match rule
    conditions against paths from root to leaf. When all conditions match,
    the rule's actions are executed using the bound variables.
    """

    def __init__(self) -> None:
        """Initialize the inference engine."""
        self._rules: list[InferenceRule] = []
        self._results: InferenceResults = InferenceResults()
        self._scope: dict[str, str] = {}  # Variable bindings for current scope

    def register_rule(self, rule: InferenceRule) -> None:
        """Register an inference rule.
        
        Args:
            rule: The inference rule to register
        """
        self._rules.append(rule)
        # Sort by priority (higher priority first)
        self._rules.sort(key=lambda r: r.priority, reverse=True)

    def load_rules_from_yaml(self, filepath: Path) -> None:
        """Load rules from a YAML file.
        
        Args:
            filepath: Path to the YAML file containing rules
        """
        from .loader import load_rules_from_yaml
        rules = load_rules_from_yaml(filepath)
        for rule in rules:
            self.register_rule(rule)

    def apply_rules(
        self, 
        groups: list[ParsedGroup], 
        schema_registry: Optional[StructureDef] = None
    ) -> InferenceResults:
        """Apply all rules to the parsed groups.
        
        Walks the group tree, and for each path from root to leaf,
        tries matching each rule's conditions. When all conditions match,
        executes the rule's actions using the bound variables.
        
        Args:
            groups: List of top-level parsed groups
            schema_registry: Optional schema for type lookups
            
        Returns:
            InferenceResults containing all generated relations and attributes
        """
        self._results = InferenceResults()
        self._scope = {}
        
        # Get all root-to-leaf paths
        paths = self._get_all_paths(groups)
        
        # Try each rule against each path
        for path in paths:
            for rule in self._rules:
                bindings = self._match_rule(rule, path, schema_registry)
                if bindings is not None:
                    self._execute_actions(rule, bindings)
        
        return self._results

    def _get_all_paths(self, groups: list[ParsedGroup]) -> list[list[ParsedGroup]]:
        """Get all root-to-leaf paths from the group tree.
        
        Args:
            groups: List of top-level groups
            
        Returns:
            List of paths, where each path is a list of groups from root to leaf
        """
        paths: list[list[ParsedGroup]] = []
        
        for group in groups:
            self._collect_paths(group, [], paths)
        
        return paths

    def _collect_paths(
        self, 
        group: ParsedGroup, 
        current_path: list[ParsedGroup], 
        all_paths: list[list[ParsedGroup]]
    ) -> None:
        """Recursively collect all paths from root to leaves.
        
        Args:
            group: Current group being processed
            current_path: Path from root to current group's parent
            all_paths: List to collect all complete paths
        """
        new_path = current_path + [group]
        
        if not group.children:
            # Leaf node - add path
            all_paths.append(new_path)
        else:
            # Internal node - recurse into children
            for child in group.children:
                self._collect_paths(child, new_path, all_paths)
            
            # Also add the path to this group itself (for rules that match at this level)
            all_paths.append(new_path)

    def _match_rule(
        self, 
        rule: InferenceRule, 
        path: list[ParsedGroup],
        schema_registry: Optional[StructureDef]
    ) -> Optional[dict[str, str]]:
        """Try to match a rule against a group path.
        
        Args:
            rule: The inference rule to match
            path: List of groups from root to some node
            schema_registry: Optional schema for type lookups
            
        Returns:
            Variable bindings if match succeeds, None otherwise
        """
        # If rule starts with SEQUENCE, match from beginning
        if rule.conditions and rule.conditions[0].type == ConditionType.SEQUENCE:
            return self._match_rule_from(
                rule, path, 0, schema_registry
            )
        
        # Otherwise, try matching from each position in the path
        # This allows rules like [group(pai, P)] to match pai anywhere in the path
        for start_idx in range(len(path)):
            result = self._match_rule_from(
                rule, path, start_idx, schema_registry
            )
            if result is not None:
                return result
        
        return None

    def _match_rule_from(
        self,
        rule: InferenceRule,
        path: list[ParsedGroup],
        start_idx: int,
        schema_registry: Optional[StructureDef]
    ) -> Optional[dict[str, str]]:
        """Try to match a rule starting from a specific position in the path.
        
        Args:
            rule: The inference rule to match
            path: List of groups from root to some node
            start_idx: Index in path to start matching from
            schema_registry: Optional schema for type lookups
            
        Returns:
            Variable bindings if match succeeds, None otherwise
        """
        bindings: dict[str, str] = {}
        path_idx = start_idx
        cond_idx = 0
        
        while cond_idx < len(rule.conditions):
            condition = rule.conditions[cond_idx]
            
            if condition.type == ConditionType.SEQUENCE:
                # Sequence matches zero or more groups - try all possibilities
                # Try matching remaining conditions against remaining path
                result = self._match_sequence(
                    rule.conditions, cond_idx, path, path_idx, bindings, schema_registry
                )
                return result if result else None
            
            elif path_idx >= len(path):
                # No more groups but still have non-sequence conditions
                return None
            
            else:
                # Try to match current condition against current group
                group = path[path_idx]
                new_bindings = self._match_condition(
                    condition, group, bindings, schema_registry
                )
                
                if new_bindings is None:
                    return None
                
                bindings = new_bindings
                path_idx += 1
                cond_idx += 1
        
        # All conditions matched
        return bindings if cond_idx == len(rule.conditions) else None

    def _match_sequence(
        self,
        conditions: list[Condition],
        seq_idx: int,
        path: list[ParsedGroup],
        path_idx: int,
        bindings: dict[str, str],
        schema_registry: Optional[StructureDef]
    ) -> Optional[dict[str, str]]:
        """Match a sequence condition and subsequent conditions.
        
        A sequence can match zero or more groups. We try all possible
        match lengths and see if the remaining conditions can match.
        
        Args:
            conditions: All conditions in the rule
            seq_idx: Index of the sequence condition
            path: Current path being matched
            path_idx: Current position in path
            bindings: Current variable bindings
            schema_registry: Optional schema for type lookups
            
        Returns:
            Variable bindings if match succeeds, None otherwise
        """
        # Try consuming 0, 1, 2, ... groups with the sequence
        for skip in range(len(path) - path_idx + 1):
            new_path_idx = path_idx + skip
            new_cond_idx = seq_idx + 1
            new_bindings = dict(bindings)
            
            # Try to match remaining conditions
            success = True
            while new_cond_idx < len(conditions):
                if new_path_idx >= len(path):
                    success = False
                    break
                
                condition = conditions[new_cond_idx]
                group = path[new_path_idx]
                
                result = self._match_condition(
                    condition, group, new_bindings, schema_registry
                )
                
                if result is None:
                    success = False
                    break
                
                new_bindings = result
                new_path_idx += 1
                new_cond_idx += 1
            
            if success:
                return new_bindings
        
        return None

    def _match_condition(
        self, 
        condition: Condition, 
        group: ParsedGroup,
        bindings: dict[str, str], 
        schema_registry: Optional[StructureDef]
    ) -> Optional[dict[str, str]]:
        """Match a single condition against a group.
        
        Args:
            condition: The condition to match
            group: The group to match against
            bindings: Current variable bindings
            schema_registry: Optional schema for type lookups
            
        Returns:
            Updated bindings if match succeeds, None otherwise
        """
        new_bindings = dict(bindings)
        
        if condition.type == ConditionType.GROUP:
            # Match specific group name
            if group.name != condition.group_name:
                return None
            if condition.bind_var:
                new_bindings[condition.bind_var] = group.id
                
        elif condition.type == ConditionType.EXTENDS:
            # Match group that extends a base class
            if not self._group_extends(group, condition.group_name, schema_registry):
                return None
            if condition.bind_var:
                new_bindings[condition.bind_var] = group.id
                
        elif condition.type == ConditionType.ELEMENT:
            # Extract element value
            element = group.get_element(condition.element_name)
            if element is None:
                return None
            if condition.bind_var:
                new_bindings[condition.bind_var] = element.get_core_text()
                
        elif condition.type == ConditionType.CLAUSE:
            # Call arbitrary predicate
            if condition.predicate is None:
                return None
            if not condition.predicate(group, new_bindings):
                return None
        
        return new_bindings

    def _group_extends(
        self, 
        group: ParsedGroup, 
        base_class: str,
        schema_registry: Optional[StructureDef]
    ) -> bool:
        """Check if a group extends a base class.
        
        Args:
            group: The group to check
            base_class: The base class name to check against
            schema_registry: Optional schema for type lookups
            
        Returns:
            True if the group extends the base class
        """
        # Direct match
        if group.name == base_class:
            return True
        
        # Check schema if available
        if schema_registry is not None:
            group_def = schema_registry.groups.get(group.name)
            if group_def is not None:
                # Check base_class field
                if group_def.base_class == base_class:
                    return True
                # Check source (fons) chain
                current = group_def
                while current and current.source:
                    if current.source == base_class:
                        return True
                    current = schema_registry.groups.get(current.source)
        
        # Fallback: check common inheritance patterns
        # These are based on the Prolog inference.pl patterns
        # Maps group name to list of base classes it extends
        inheritance_map = {
            'actorm': ['actorm', 'person', 'actor'],
            'actorf': ['actorf', 'person', 'actor'],
            'person': ['person', 'actor'],
            'actor': ['actor'],
        }
        
        # Get the inheritance chain for this group
        group_bases = inheritance_map.get(group.name, [group.name])
        return base_class in group_bases

    def _execute_actions(self, rule: InferenceRule, bindings: dict[str, str]) -> None:
        """Execute rule actions with the given variable bindings.
        
        Args:
            rule: The rule whose actions to execute
            bindings: Variable bindings from condition matching
        """
        for action in rule.actions:
            if action.type == ActionType.RELATION:
                self._execute_relation_action(action, bindings, rule.name)
            elif action.type == ActionType.ATTRIBUTE:
                self._execute_attribute_action(action, bindings, rule.name)
            elif action.type == ActionType.NEW_SCOPE:
                self._scope = {}

    def _execute_relation_action(
        self, 
        action: Action, 
        bindings: dict[str, str],
        rule_name: str
    ) -> None:
        """Execute a relation action.
        
        Args:
            action: The relation action to execute
            bindings: Variable bindings
            rule_name: Name of the source rule
        """
        origin_id = bindings.get(action.origin_var, action.origin_var)
        dest_id = bindings.get(action.dest_var, action.dest_var)
        
        if not origin_id or not dest_id:
            return
        
        relation = GeneratedRelation(
            rel_type=action.relation_type,
            value=action.relation_value,
            origin_id=origin_id,
            dest_id=dest_id,
            source_rule=rule_name
        )
        
        self._results.add_relation(relation)

    def _execute_attribute_action(
        self, 
        action: Action, 
        bindings: dict[str, str],
        rule_name: str
    ) -> None:
        """Execute an attribute action.
        
        Args:
            action: The attribute action to execute
            bindings: Variable bindings
            rule_name: Name of the source rule
        """
        entity_id = bindings.get(action.attr_entity_var, action.attr_entity_var)
        
        if not entity_id:
            return
        
        # Handle both literal values and variable references
        if action.attr_value_is_var:
            attr_value = bindings.get(action.attr_value, action.attr_value)
        else:
            attr_value = action.attr_value
        
        attribute = GeneratedAttribute(
            entity_id=entity_id,
            attr_type=action.attr_type,
            attr_value=attr_value,
            source_rule=rule_name
        )
        
        self._results.add_attribute(attribute)

    def clear_rules(self) -> None:
        """Clear all registered rules."""
        self._rules = []

    def get_rules(self) -> list[InferenceRule]:
        """Get all registered rules.
        
        Returns:
            List of registered inference rules
        """
        return list(self._rules)
