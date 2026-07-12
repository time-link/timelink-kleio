"""YAML loader for inference rules.

This module provides functionality to load inference rules from YAML files.
The YAML format follows the structure:

    rules:
      - name: father_relation
        description: "Generate father relationship"
        priority: 0
        when:
          - sequence: any
          - extends: {base: actorm, bind: child_id}
          - group: {name: pai, bind: father_id}
        then:
          - relation: {type: parentesco, value: pai, origin: father_id, dest: child_id}

A rule may also express a cross-path conjunction (``and`` between several
sub-paths) using ``when_any:`` instead of ``when:``. Each entry under
``when_any:`` is itself a list of conditions describing one sub-path. The
sub-paths are matched independently and joined on any shared ``bind``
variable names:

    - name: parents_couple
      when_any:
        - - sequence: any
          - extends: {base: actorm, bind: child_id}
          - group: {name: pai, bind: father_id}
        - - sequence: any
          - extends: {base: actorm, bind: child_id}
          - group: {name: mae, bind: mother_id}
      then:
        - relation: {type: parentesco, value: marido, origin: father_id, dest: mother_id}
"""
from __future__ import annotations

from pathlib import Path
from typing import Any

from .models import (
    InferenceRule,
    Condition,
    ConditionType,
    Action,
    ActionType,
)


def load_rules_from_yaml(filepath: Path) -> list[InferenceRule]:
    """Load inference rules from a YAML file.
    
    Args:
        filepath: Path to the YAML file containing rules
        
    Returns:
        List of loaded inference rules
        
    Raises:
        FileNotFoundError: If the file doesn't exist
        ValueError: If the YAML format is invalid
    """
    import yaml
    
    if not filepath.exists():
        raise FileNotFoundError(f"Rules file not found: {filepath}")
    
    with open(filepath, 'r', encoding='utf-8') as f:
        data = yaml.safe_load(f)
    
    rules: list[InferenceRule] = []
    
    if isinstance(data, dict):
        # Dict root: expect 'rules' key containing list of rules
        rules_data = data.get('rules', [])
        if not isinstance(rules_data, list):
            raise ValueError("'rules' must be a list")
        for rule_data in rules_data:
            rule = _parse_rule(rule_data)
            if rule:
                rules.append(rule)
    elif isinstance(data, list):
        # List root: each item is a rule entry (with 'rule:', 'inference:', etc.)
        for item in data:
            if isinstance(item, dict):
                rule = _parse_rule(item)
                if rule:
                    rules.append(rule)
    else:
        raise ValueError("YAML root must be a dictionary or list")
    
    return rules


def _parse_rule(data: dict[str, Any]) -> InferenceRule | None:
    """Parse a single rule from YAML data.
    
    Args:
        data: Dictionary containing rule definition
        
    Returns:
        Parsed InferenceRule or None if invalid
    """
    if not isinstance(data, dict):
        return None
    
    name = data.get('name', '')
    if not name:
        return None
    
    rule = InferenceRule(
        name=name,
        description=data.get('description', ''),
        priority=data.get('priority', 0)
    )
    
    # Parse cross-path conjunction (when_any: list of sub-paths).
    # Each sub-path is itself a list of condition dicts. When present,
    # this takes precedence over `when:` and is stored in
    # `rule.condition_paths`; the engine then joins the sub-paths by
    # their shared bind_var names.
    when_any_data = data.get('when_any')
    if isinstance(when_any_data, list):
        for sub_path_data in when_any_data:
            if not isinstance(sub_path_data, list):
                continue
            sub_path: list[Condition] = []
            for cond_data in sub_path_data:
                condition = _parse_condition(cond_data)
                if condition:
                    sub_path.append(condition)
            if sub_path:
                rule.condition_paths.append(sub_path)

    # Parse conditions (single-path form)
    when_data = data.get('when', [])
    if isinstance(when_data, list):
        for cond_data in when_data:
            condition = _parse_condition(cond_data)
            if condition:
                rule.conditions.append(condition)

    # Parse actions
    then_data = data.get('then', [])
    if isinstance(then_data, list):
        for action_data in then_data:
            action = _parse_action(action_data)
            if action:
                rule.actions.append(action)

    return rule


def _parse_condition(data: dict[str, Any]) -> Condition | None:
    """Parse a condition from YAML data.
    
    Args:
        data: Dictionary containing condition definition
        
    Returns:
        Parsed Condition or None if invalid
    """
    if not isinstance(data, dict):
        return None
    
    # sequence: any or sequence: _
    if 'sequence' in data:
        return Condition(
            type=ConditionType.SEQUENCE,
            bind_var='' if data['sequence'] in ('any', '_') else str(data['sequence'])
        )
    
    # group: {name: xxx, bind: yyy}
    if 'group' in data:
        group_data = data['group']
        if isinstance(group_data, dict):
            return Condition(
                type=ConditionType.GROUP,
                group_name=group_data.get('name', ''),
                bind_var=group_data.get('bind', '')
            )
        elif isinstance(group_data, str):
            # Shorthand: group: groupname
            return Condition(
                type=ConditionType.GROUP,
                group_name=group_data,
                bind_var=''
            )
    
    # extends: {base: xxx, bind: yyy}
    if 'extends' in data:
        extends_data = data['extends']
        if isinstance(extends_data, dict):
            return Condition(
                type=ConditionType.EXTENDS,
                group_name=extends_data.get('base', ''),
                bind_var=extends_data.get('bind', '')
            )
        elif isinstance(extends_data, str):
            # Shorthand: extends: basename
            return Condition(
                type=ConditionType.EXTENDS,
                group_name=extends_data,
                bind_var=''
            )
    
    # element: {name: xxx, bind: yyy}
    if 'element' in data:
        element_data = data['element']
        if isinstance(element_data, dict):
            return Condition(
                type=ConditionType.ELEMENT,
                element_name=element_data.get('name', ''),
                bind_var=element_data.get('bind', '')
            )
        elif isinstance(element_data, str):
            # Shorthand: element: elementname
            return Condition(
                type=ConditionType.ELEMENT,
                element_name=element_data,
                bind_var=''
            )
    
    # clause: predicate_name (would need to be resolved to a callable)
    if 'clause' in data:
        return Condition(
            type=ConditionType.CLAUSE,
            # predicate would be resolved elsewhere
        )
    
    return None


def _parse_action(data: dict[str, Any]) -> Action | None:
    """Parse an action from YAML data.
    
    Args:
        data: Dictionary containing action definition
        
    Returns:
        Parsed Action or None if invalid
    """
    if not isinstance(data, dict):
        return None
    
    # relation: {type: xxx, value: yyy, origin: aaa, dest: bbb}
    if 'relation' in data:
        rel_data = data['relation']
        if isinstance(rel_data, dict):
            return Action(
                type=ActionType.RELATION,
                relation_type=rel_data.get('type', ''),
                relation_value=rel_data.get('value', ''),
                origin_var=rel_data.get('origin', ''),
                dest_var=rel_data.get('dest', '')
            )
    
    # attribute: {entity: xxx, type: yyy, value: zzz}
    if 'attribute' in data:
        attr_data = data['attribute']
        if isinstance(attr_data, dict):
            value = attr_data.get('value', '')
            value_is_var = attr_data.get('value_is_var', False)
            return Action(
                type=ActionType.ATTRIBUTE,
                attr_entity_var=attr_data.get('entity', ''),
                attr_type=attr_data.get('type', ''),
                attr_value=value,
                attr_value_is_var=value_is_var
            )
    
    # newscope
    if 'newscope' in data or 'new_scope' in data:
        return Action(type=ActionType.NEW_SCOPE)
    
    return None


def save_rules_to_yaml(rules: list[InferenceRule], filepath: Path) -> None:
    """Save inference rules to a YAML file.
    
    Args:
        rules: List of inference rules to save
        filepath: Path to write the YAML file
    """
    import yaml
    
    data = {'rules': []}
    
    for rule in rules:
        rule_data: dict[str, Any] = {
            'name': rule.name,
            'description': rule.description,
            'priority': rule.priority,
            'then': []
        }

        if rule.condition_paths:
            # Cross-path conjunction: emit when_any as a list of sub-paths,
            # each a list of condition dicts.
            when_any: list[list[dict[str, Any]]] = []
            for sub_path in rule.condition_paths:
                sub_data: list[dict[str, Any]] = []
                for cond in sub_path:
                    cond_data = _serialize_condition(cond)
                    if cond_data:
                        sub_data.append(cond_data)
                when_any.append(sub_data)
            rule_data['when_any'] = when_any
        else:
            rule_data['when'] = []
            for cond in rule.conditions:
                cond_data = _serialize_condition(cond)
                if cond_data:
                    rule_data['when'].append(cond_data)

        # Serialize actions
        for action in rule.actions:
            action_data = _serialize_action(action)
            if action_data:
                rule_data['then'].append(action_data)

        data['rules'].append(rule_data)
    
    with open(filepath, 'w', encoding='utf-8') as f:
        yaml.dump(data, f, allow_unicode=True, sort_keys=False)


def _serialize_condition(condition: Condition) -> dict[str, Any] | None:
    """Serialize a condition to YAML-compatible dictionary.
    
    Args:
        condition: The condition to serialize
        
    Returns:
        Dictionary representation or None
    """
    if condition.type == ConditionType.SEQUENCE:
        return {'sequence': 'any'}
    
    elif condition.type == ConditionType.GROUP:
        if condition.bind_var:
            return {'group': {'name': condition.group_name, 'bind': condition.bind_var}}
        else:
            return {'group': condition.group_name}
    
    elif condition.type == ConditionType.EXTENDS:
        if condition.bind_var:
            return {'extends': {'base': condition.group_name, 'bind': condition.bind_var}}
        else:
            return {'extends': condition.group_name}
    
    elif condition.type == ConditionType.ELEMENT:
        if condition.bind_var:
            return {'element': {'name': condition.element_name, 'bind': condition.bind_var}}
        else:
            return {'element': condition.element_name}
    
    elif condition.type == ConditionType.CLAUSE:
        return {'clause': 'custom'}
    
    return None


def _serialize_action(action: Action) -> dict[str, Any] | None:
    """Serialize an action to YAML-compatible dictionary.
    
    Args:
        action: The action to serialize
        
    Returns:
        Dictionary representation or None
    """
    if action.type == ActionType.RELATION:
        return {
            'relation': {
                'type': action.relation_type,
                'value': action.relation_value,
                'origin': action.origin_var,
                'dest': action.dest_var
            }
        }
    
    elif action.type == ActionType.ATTRIBUTE:
        attr_data: dict[str, Any] = {
            'entity': action.attr_entity_var,
            'type': action.attr_type,
            'value': action.attr_value
        }
        if action.attr_value_is_var:
            attr_data['value_is_var'] = True
        return {'attribute': attr_data}
    
    elif action.type == ActionType.NEW_SCOPE:
        return {'newscope': True}
    
    return None
