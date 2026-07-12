"""Inference engine for Kleio rule matching.

The inference engine matches rules against the hierarchical group structure
from parsed .cli files. Rules define patterns like:

    if [sequence(_), extends(actorm, N), pai(P)] 
    then relation(parentesco, pai, P, N)

This generates a father relationship when a 'pai' group is found within
an 'actorm' (male actor) group.
"""
from __future__ import annotations

import logging
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

logger = logging.getLogger(__name__)


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
        # Names of groups referenced as a ``source`` parent that are absent
        # from the schema, for which a warning has already been emitted.
        # Avoids log spam when the same missing intermediate is hit by many
        # rules/paths in a single apply_rules pass.
        self._missing_source_warned: set[str] = set()

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

        Note: the nesting hierarchy is determined by the schema (the
        builder records each group's ancestry in its ``path`` metadata),
        not by source-file indentation. ``build_tree`` is used here to
        reconstruct parent->children links from that metadata so the
        path enumeration below reflects the logical structure.

        Args:
            groups: List of parsed groups (flat list with ``path``
                metadata, as produced by the parser/builder).
            schema_registry: Optional schema for type lookups

        Returns:
            InferenceResults containing all generated relations and attributes
        """
        from ..parser.models import build_tree

        self._results = InferenceResults()
        self._scope = {}
        self._missing_source_warned.clear()

        # Reconstruct the parent->children tree from each group's ``path``
        # metadata (schema-derived), then enumerate root-to-leaf paths.
        roots = build_tree(groups)
        paths = self._get_all_paths(roots)

        # Try each rule against each path
        for path in paths:
            for rule in self._rules:
                # Multi-path (cross-path AND) rules are evaluated against
                # the full path set, not a single path.
                if rule.condition_paths:
                    continue
                bindings = self._match_rule(rule, path, schema_registry)
                if bindings is not None:
                    self._execute_actions(rule, bindings)

        # Evaluate multi-path rules once against the full path set
        for rule in self._rules:
            if not rule.condition_paths:
                continue
            for joint_bindings in self._match_multi_path(
                rule, paths, schema_registry
            ):
                self._execute_actions(rule, joint_bindings)

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
        return self._match_path(rule.conditions, path, schema_registry)

    def _match_path(
        self,
        conditions: list[Condition],
        path: list[ParsedGroup],
        schema_registry: Optional[StructureDef]
    ) -> Optional[dict[str, str]]:
        """Try to match a list of conditions against a single path.

        This is the single-path matcher. If the first condition is a
        SEQUENCE, matching is anchored at index 0; otherwise every
        starting position is tried (so a rule like ``[group(pai, P)]``
        can match ``pai`` anywhere in the path).

        Args:
            conditions: Ordered list of conditions to match
            path: List of groups from root to some node
            schema_registry: Optional schema for type lookups

        Returns:
            Variable bindings if match succeeds, None otherwise
        """
        if conditions and conditions[0].type == ConditionType.SEQUENCE:
            return self._match_conditions_from(
                conditions, path, 0, schema_registry
            )

        for start_idx in range(len(path)):
            result = self._match_conditions_from(
                conditions, path, start_idx, schema_registry
            )
            if result is not None:
                return result

        return None

    def _match_path_all_solutions(
        self,
        conditions: list[Condition],
        paths: list[list[ParsedGroup]],
        schema_registry: Optional[StructureDef]
    ) -> list[dict[str, str]]:
        """Match a condition list against every path, returning all solutions.

        Used to gather candidate bindings for one sub-path of a
        multi-path rule. Distinct solutions are returned in insertion
        order. Each path is matched with the same semantics as
        :meth:`_match_path` (first success per path).

        Args:
            conditions: Ordered list of conditions for this sub-path
            paths: All root-to-node paths in the group tree
            schema_registry: Optional schema for type lookups

        Returns:
            List of distinct binding dicts (may be empty)
        """
        solutions: list[dict[str, str]] = []
        seen: set[tuple[tuple[str, str], ...]] = set()

        for path in paths:
            bindings = self._match_path(conditions, path, schema_registry)
            if bindings is None:
                continue
            key = tuple(sorted(bindings.items()))
            if key in seen:
                continue
            seen.add(key)
            solutions.append(bindings)

        return solutions

    def _match_multi_path(
        self,
        rule: InferenceRule,
        paths: list[list[ParsedGroup]],
        schema_registry: Optional[StructureDef]
    ) -> list[dict[str, str]]:
        """Match a multi-path (cross-path AND) rule.

        Each sub-path in ``rule.condition_paths`` is matched
        independently against all paths; the resulting bindings are
        joined by unification on shared ``bind_var`` names. Every
        consistent joint binding fires the rule's actions once.

        Args:
            rule: Rule with non-empty ``condition_paths``
            paths: All root-to-node paths in the group tree
            schema_registry: Optional schema for type lookups

        Returns:
            List of joint binding dicts (one per consistent combination)
        """
        if not rule.condition_paths:
            return []

        # Seed with the solutions of the first sub-path
        first_solutions = self._match_path_all_solutions(
            rule.condition_paths[0], paths, schema_registry
        )
        if not first_solutions:
            return []

        accumulated: list[dict[str, str]] = list(first_solutions)

        for sub_path in rule.condition_paths[1:]:
            sub_solutions = self._match_path_all_solutions(
                sub_path, paths, schema_registry
            )
            if not sub_solutions:
                return []

            next_acc: list[dict[str, str]] = []
            for acc in accumulated:
                for sol in sub_solutions:
                    merged = self._merge_bindings(acc, sol)
                    if merged is not None:
                        next_acc.append(merged)
            accumulated = next_acc
            if not accumulated:
                return []

        # Deduplicate joint bindings (different combinations may unify
        # to the same dict).
        unique: list[dict[str, str]] = []
        seen: set[tuple[tuple[str, str], ...]] = set()
        for bindings in accumulated:
            key = tuple(sorted(bindings.items()))
            if key in seen:
                continue
            seen.add(key)
            unique.append(bindings)

        return unique

    @staticmethod
    def _merge_bindings(
        a: dict[str, str],
        b: dict[str, str]
    ) -> Optional[dict[str, str]]:
        """Unify two binding dicts by shared variable names.

        Returns a new dict containing all entries from both inputs when
        every variable present in both maps to the same value; otherwise
        returns ``None``.

        Args:
            a: First binding dict
            b: Second binding dict

        Returns:
            Merged dict, or None if a shared variable conflicts
        """
        merged = dict(a)
        for key, value in b.items():
            if key in merged:
                if merged[key] != value:
                    return None
            else:
                merged[key] = value
        return merged

    def _match_conditions_from(
        self,
        conditions: list[Condition],
        path: list[ParsedGroup],
        start_idx: int,
        schema_registry: Optional[StructureDef]
    ) -> Optional[dict[str, str]]:
        """Try to match a condition list starting from a specific position.

        Args:
            conditions: Ordered list of conditions to match
            path: List of groups from root to some node
            start_idx: Index in path to start matching from
            schema_registry: Optional schema for type lookups

        Returns:
            Variable bindings if match succeeds, None otherwise
        """
        bindings: dict[str, str] = {}
        path_idx = start_idx
        cond_idx = 0

        while cond_idx < len(conditions):
            condition = conditions[cond_idx]

            if condition.type == ConditionType.SEQUENCE:
                # Sequence matches zero or more groups - try all possibilities
                # Try matching remaining conditions against remaining path
                result = self._match_sequence(
                    conditions, cond_idx, path, path_idx, bindings, schema_registry
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
        return bindings if cond_idx == len(conditions) else None

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

        Walks the full ``source`` (fons) inheritance chain. A group is
        considered to extend ``base_class`` if any of the following holds:

        1. The group's own name equals ``base_class``.
        2. ``base_class`` appears anywhere along the group's ``source``
           chain (e.g. for ``n`` the chain is
           ``n -> actorm -> male -> person -> entity``, so ``n`` extends
           ``actorm``, ``male``, ``person`` and ``entity``).
        3. The schema-resolved ``base_class`` field of the group (or any
           ancestor reached along the chain) equals ``base_class``. This
           covers schemas where intermediate links are missing but the
           root was pre-resolved at load time.

        The walk is defended against missing intermediate definitions
        and against cycles in the ``source`` chain. When a group
        referenced as a ``source`` parent is absent from the schema, a
        warning is emitted (once per missing name per engine) and the
        walk stops at that point.

        Args:
            group: The group to check
            base_class: The base class name to check against
            schema_registry: Optional schema for type lookups

        Returns:
            True if the group extends the base class
        """
        # 1. Direct name match.
        if group.name == base_class:
            return True

        # 2/3. Walk the source chain via the schema, if available.
        if schema_registry is not None:
            current_name = group.name
            visited: set[str] = set()
            while current_name and current_name not in visited:
                visited.add(current_name)
                group_def = schema_registry.groups.get(current_name)
                if group_def is None:
                    # An intermediate group referenced in the source chain
                    # is absent from the schema. The walk cannot continue,
                    # so report it once per missing name.
                    if current_name not in self._missing_source_warned:
                        self._missing_source_warned.add(current_name)
                        logger.warning(
                            "Inference extends check: group %r (referenced "
                            "in the source chain starting from %r) is not "
                            "defined in the schema; inheritance lookup "
                            "stopped at this point.",
                            current_name, group.name,
                        )
                    break
                # The pre-resolved base_class field counts as an ancestor.
                if group_def.base_class == base_class:
                    return True
                # Step up to the parent in the inheritance hierarchy.
                parent = group_def.source
                if parent == base_class:
                    return True
                current_name = parent

        # 4. Last-resort fallback: a small built-in inheritance map for
        # the common Kleio actor classes, used when no schema is provided
        # or the schema lacks the relevant group definitions. This keeps
        # the inference engine usable on minimal/in-memory test fixtures.
        inheritance_map = {
            'actorm': ['actorm', 'male', 'person', 'entity', 'actor'],
            'actorf': ['actorf', 'female', 'person', 'entity', 'actor'],
            'male': ['male', 'person', 'entity'],
            'female': ['female', 'person', 'entity'],
            'person': ['person', 'entity'],
            'n': ['n', 'actorm', 'male', 'person', 'entity'],
        }
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

        If ``origin_var`` or ``dest_var`` is not present in ``bindings``,
        the action is silently skipped. This prevents emitting relations
        with the literal variable name as an entity id when a multi-path
        rule failed to bind all of its variables.

        Args:
            action: The relation action to execute
            bindings: Variable bindings
            rule_name: Name of the source rule
        """
        if action.origin_var not in bindings or action.dest_var not in bindings:
            return

        origin_id = bindings[action.origin_var]
        dest_id = bindings[action.dest_var]

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

        If ``attr_entity_var`` (or, when ``attr_value_is_var`` is set,
        ``attr_value``) is not present in ``bindings``, the action is
        silently skipped.

        Args:
            action: The attribute action to execute
            bindings: Variable bindings
            rule_name: Name of the source rule
        """
        if action.attr_entity_var not in bindings:
            return

        entity_id = bindings[action.attr_entity_var]

        if not entity_id:
            return

        # Handle both literal values and variable references
        if action.attr_value_is_var:
            if action.attr_value not in bindings:
                return
            attr_value = bindings[action.attr_value]
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
