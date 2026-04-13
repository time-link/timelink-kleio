"""Kleio inference module.

This module provides inference functionality for Kleio data processing.
It includes rule definitions, a matching engine, and YAML loading capabilities.

Example usage:
    from kleio.inference import InferenceEngine, get_default_rules
    
    engine = InferenceEngine()
    rules = get_default_rules()
    for rule in rules:
        engine.register_rule(rule)
    
    results = engine.apply_rules(parsed_groups, schema)
    
    for rel in results.relations:
        print(f"{rel.origin_id} --{rel.value}--> {rel.dest_id}")
"""

from .models import (
    Condition,
    ConditionType,
    Action,
    ActionType,
    InferenceRule,
    GeneratedRelation,
    GeneratedAttribute,
    InferenceResults,
)
from .engine import InferenceEngine
from .loader import load_rules_from_yaml, save_rules_to_yaml
from .rules import get_default_rules

__all__ = [
    # Models
    "Condition",
    "ConditionType",
    "Action",
    "ActionType",
    "InferenceRule",
    "GeneratedRelation",
    "GeneratedAttribute",
    "InferenceResults",
    # Engine
    "InferenceEngine",
    # Loader
    "load_rules_from_yaml",
    "save_rules_to_yaml",
    # Rules
    "get_default_rules",
]
