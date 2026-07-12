"""Built-in inference rules for Portuguese genealogical patterns.

This module provides a default set of inference rules based on the
Prolog inference engine in src/inference.pl. These rules cover the
main Portuguese genealogical patterns for historical documents.
"""
from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .models import InferenceRule


def get_default_rules() -> list[InferenceRule]:
    """Load the built-in default inference rules.
    
    Returns:
        List of default inference rules covering Portuguese genealogical patterns
    """
    from .loader import load_rules_from_yaml
    
    # Get the directory where this module is located
    module_dir = Path(__file__).parent
    rules_file = module_dir / 'default_rules.yaml'
    
    if rules_file.exists():
        return load_rules_from_yaml(rules_file)
    
    # Fallback: return programmatically defined rules
    return _get_programmatic_rules()


def _get_programmatic_rules() -> list[InferenceRule]:
    """Get rules defined programmatically (fallback if YAML file missing).
    
    Returns:
        List of inference rules
    """
    from .models import InferenceRule, Condition, ConditionType, Action, ActionType
    
    rules: list[InferenceRule] = []
    
    # Father relation: pai -> actorm/actorf
    # if [sequence(_),extends(actorm,N),pai(P)] then relation(parentesco,pai,P,N)
    rules.append(InferenceRule(
        name="father_of_male_actor",
        description="Generate father relationship from pai to male actor",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.EXTENDS, group_name="actorm", bind_var="N"),
            Condition(type=ConditionType.GROUP, group_name="pai", bind_var="P"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="pai",
                origin_var="P",
                dest_var="N"
            )
        ]
    ))
    
    # Father relation: pai -> actorf (female actor)
    rules.append(InferenceRule(
        name="father_of_female_actor",
        description="Generate father relationship from pai to female actor",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.EXTENDS, group_name="actorf", bind_var="N"),
            Condition(type=ConditionType.GROUP, group_name="pai", bind_var="P"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="pai",
                origin_var="P",
                dest_var="N"
            )
        ]
    ))
    
    # Mother relation: mae -> actorm/actorf
    rules.append(InferenceRule(
        name="mother_of_male_actor",
        description="Generate mother relationship from mae to male actor",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.EXTENDS, group_name="actorm", bind_var="N"),
            Condition(type=ConditionType.GROUP, group_name="mae", bind_var="M"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="mae",
                origin_var="M",
                dest_var="N"
            )
        ]
    ))
    
    rules.append(InferenceRule(
        name="mother_of_female_actor",
        description="Generate mother relationship from mae to female actor",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.EXTENDS, group_name="actorf", bind_var="N"),
            Condition(type=ConditionType.GROUP, group_name="mae", bind_var="M"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="mae",
                origin_var="M",
                dest_var="N"
            )
        ]
    ))
    
    # Parents as a couple: pai(P) and mae(M) sharing the same actorm child.
    # Cross-path rule: each sub-path anchors on the same actorm (joined via
    # the shared bind_var "child_id") and contributes one parent.
    rules.append(InferenceRule(
        name="parents_couple",
        description="Generate husband relation from pai to mae (parents as couple)",
        condition_paths=[
            [
                Condition(type=ConditionType.SEQUENCE),
                Condition(type=ConditionType.EXTENDS, group_name="actorm", bind_var="child_id"),
                Condition(type=ConditionType.GROUP, group_name="pai", bind_var="father_id"),
            ],
            [
                Condition(type=ConditionType.SEQUENCE),
                Condition(type=ConditionType.EXTENDS, group_name="actorm", bind_var="child_id"),
                Condition(type=ConditionType.GROUP, group_name="mae", bind_var="mother_id"),
            ],
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="marido",
                origin_var="father_id",
                dest_var="mother_id"
            ),
            Action(
                type=ActionType.ATTRIBUTE,
                attr_entity_var="father_id",
                attr_type="ec",
                attr_value="c"
            ),
            Action(
                type=ActionType.ATTRIBUTE,
                attr_entity_var="mother_id",
                attr_type="ec",
                attr_value="c"
            )
        ]
    ))
    
    # Son relation: filho -> person
    rules.append(InferenceRule(
        name="son_of_person",
        description="Generate son relationship from filho to person",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.EXTENDS, group_name="person", bind_var="N"),
            Condition(type=ConditionType.GROUP, group_name="filho", bind_var="F"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="filho",
                origin_var="F",
                dest_var="N"
            )
        ]
    ))
    
    # Daughter relation: filha -> person
    rules.append(InferenceRule(
        name="daughter_of_person",
        description="Generate daughter relationship from filha to person",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.EXTENDS, group_name="person", bind_var="N"),
            Condition(type=ConditionType.GROUP, group_name="filha", bind_var="F"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="filha",
                origin_var="F",
                dest_var="N"
            )
        ]
    ))
    
    # Brother relation: irmao -> person
    rules.append(InferenceRule(
        name="brother_of_person",
        description="Generate brother relationship from irmao to person",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.EXTENDS, group_name="person", bind_var="N"),
            Condition(type=ConditionType.GROUP, group_name="irmao", bind_var="F"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="irmao",
                origin_var="F",
                dest_var="N"
            )
        ]
    ))
    
    # Sister relation: irma -> person
    rules.append(InferenceRule(
        name="sister_of_person",
        description="Generate sister relationship from irma to person",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.EXTENDS, group_name="person", bind_var="N"),
            Condition(type=ConditionType.GROUP, group_name="irma", bind_var="F"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="irma",
                origin_var="F",
                dest_var="N"
            )
        ]
    ))
    
    # Husband/Wife relations
    # Male actor with wife (mulher)
    rules.append(InferenceRule(
        name="male_actor_with_wife",
        description="Generate husband relation from male actor to wife",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.EXTENDS, group_name="actorm", bind_var="N"),
            Condition(type=ConditionType.GROUP, group_name="mulher", bind_var="M"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="marido",
                origin_var="N",
                dest_var="M"
            ),
            Action(
                type=ActionType.ATTRIBUTE,
                attr_entity_var="N",
                attr_type="ec",
                attr_value="c"
            ),
            Action(
                type=ActionType.ATTRIBUTE,
                attr_entity_var="M",
                attr_type="ec",
                attr_value="c"
            )
        ]
    ))
    
    # Female actor with husband (marido)
    rules.append(InferenceRule(
        name="female_actor_with_husband",
        description="Generate wife relation from female actor to husband",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.EXTENDS, group_name="actorf", bind_var="N"),
            Condition(type=ConditionType.GROUP, group_name="marido", bind_var="M"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="mulher",
                origin_var="N",
                dest_var="M"
            ),
            Action(
                type=ActionType.ATTRIBUTE,
                attr_entity_var="N",
                attr_type="ec",
                attr_value="c"
            ),
            Action(
                type=ActionType.ATTRIBUTE,
                attr_entity_var="M",
                attr_type="ec",
                attr_value="c"
            )
        ]
    ))
    
    # Grandfather relations (ppai = paternal grandfather)
    rules.append(InferenceRule(
        name="paternal_grandfather",
        description="Generate father relation from paternal grandfather to father",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.GROUP, group_name="pai", bind_var="Son"),
            Condition(type=ConditionType.GROUP, group_name="ppai", bind_var="Parent"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="pai",
                origin_var="Parent",
                dest_var="Son"
            )
        ]
    ))
    
    # Paternal grandmother (mpai = mother of father)
    rules.append(InferenceRule(
        name="paternal_grandmother",
        description="Generate mother relation from paternal grandmother to father",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.GROUP, group_name="pai", bind_var="Son"),
            Condition(type=ConditionType.GROUP, group_name="mpai", bind_var="Parent"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="mae",
                origin_var="Parent",
                dest_var="Son"
            )
        ]
    ))

    # Maternal grandfather (pmae = father of mother)
    rules.append(InferenceRule(
        name="maternal_grandfather",
        description="Generate father relation from maternal grandfather to mother",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.GROUP, group_name="mae", bind_var="Son"),
            Condition(type=ConditionType.GROUP, group_name="pmae", bind_var="Parent"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="pai",
                origin_var="Parent",
                dest_var="Son"
            )
        ]
    ))
    
    # Maternal grandmother (mmae)
    rules.append(InferenceRule(
        name="maternal_grandmother",
        description="Generate mother relation from maternal grandmother to mother",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.GROUP, group_name="mae", bind_var="Son"),
            Condition(type=ConditionType.GROUP, group_name="mmae", bind_var="Parent"),
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="mae",
                origin_var="Parent",
                dest_var="Son"
            )
        ]
    ))
    
    # Marriage: noivo (groom) and noiva (bride) under the same cas record.
    # Cross-path rule: each sub-path anchors on the same cas (joined via
    # the shared bind_var "cas_id") and contributes one spouse.
    rules.append(InferenceRule(
        name="marriage_groom_bride",
        description="Generate husband relation between groom and bride",
        condition_paths=[
            [
                Condition(type=ConditionType.SEQUENCE),
                Condition(type=ConditionType.GROUP, group_name="cas", bind_var="cas_id"),
                Condition(type=ConditionType.GROUP, group_name="noivo", bind_var="groom_id"),
            ],
            [
                Condition(type=ConditionType.SEQUENCE),
                Condition(type=ConditionType.GROUP, group_name="cas", bind_var="cas_id"),
                Condition(type=ConditionType.GROUP, group_name="noiva", bind_var="bride_id"),
            ],
        ],
        actions=[
            Action(
                type=ActionType.RELATION,
                relation_type="parentesco",
                relation_value="marido",
                origin_var="groom_id",
                dest_var="bride_id"
            ),
            Action(
                type=ActionType.ATTRIBUTE,
                attr_entity_var="groom_id",
                attr_type="ec",
                attr_value="c"
            ),
            Action(
                type=ActionType.ATTRIBUTE,
                attr_entity_var="bride_id",
                attr_type="ec",
                attr_value="c"
            )
        ]
    ))
    
    # New scope rules - reset context at certain group types
    rules.append(InferenceRule(
        name="new_scope_historical_act",
        description="Reset scope when entering a historical act",
        conditions=[
            Condition(type=ConditionType.SEQUENCE),
            Condition(type=ConditionType.EXTENDS, group_name="historical-act"),
        ],
        actions=[
            Action(type=ActionType.NEW_SCOPE)
        ]
    ))
    
    return rules
