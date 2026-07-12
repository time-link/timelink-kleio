"""Tests for Kleio inference engine.

Tests cover:
1. Rule matching against group hierarchies
2. Variable binding from conditions
3. Relation and attribute generation
4. Duplicate prevention
5. YAML rule loading
"""
from __future__ import annotations

import pytest
from pathlib import Path

from kleio.inference.models import (
    InferenceRule,
    Condition,
    ConditionType,
    Action,
    ActionType,
    GeneratedRelation,
    GeneratedAttribute,
    InferenceResults,
)
from kleio.inference.engine import InferenceEngine
from kleio.inference.loader import load_rules_from_yaml, save_rules_to_yaml
from kleio.inference.rules import get_default_rules
from kleio.parser.models import ParsedGroup, ParsedElement


class TestInferenceModels:
    """Tests for inference data models."""
    
    def test_condition_creation(self):
        """Test creating conditions."""
        cond = Condition(
            type=ConditionType.GROUP,
            group_name="pai",
            bind_var="father_id"
        )
        assert cond.type == ConditionType.GROUP
        assert cond.group_name == "pai"
        assert cond.bind_var == "father_id"
    
    def test_action_creation(self):
        """Test creating actions."""
        action = Action(
            type=ActionType.RELATION,
            relation_type="parentesco",
            relation_value="pai",
            origin_var="father_id",
            dest_var="child_id"
        )
        assert action.type == ActionType.RELATION
        assert action.relation_type == "parentesco"
        assert action.relation_value == "pai"
        assert action.origin_var == "father_id"
        assert action.dest_var == "child_id"
    
    def test_rule_creation(self):
        """Test creating inference rules."""
        rule = InferenceRule(
            name="test_rule",
            description="Test rule",
            priority=1,
            conditions=[
                Condition(type=ConditionType.SEQUENCE),
                Condition(type=ConditionType.GROUP, group_name="pai", bind_var="P"),
            ],
            actions=[
                Action(type=ActionType.RELATION, relation_type="test", relation_value="val", 
                       origin_var="P", dest_var="P"),
            ]
        )
        assert rule.name == "test_rule"
        assert len(rule.conditions) == 2
        assert len(rule.actions) == 1
    
    def test_generated_relation_equality(self):
        """Test relation equality and hashing."""
        rel1 = GeneratedRelation("parentesco", "pai", "p1", "c1", "rule1")
        rel2 = GeneratedRelation("parentesco", "pai", "p1", "c1", "rule2")
        rel3 = GeneratedRelation("parentesco", "mae", "p1", "c1", "rule1")
        
        assert rel1 == rel2  # Same content, different source rule
        assert rel1 != rel3  # Different value
        assert hash(rel1) == hash(rel2)
    
    def test_generated_attribute_equality(self):
        """Test attribute equality and hashing."""
        attr1 = GeneratedAttribute("p1", "ec", "c", "rule1")
        attr2 = GeneratedAttribute("p1", "ec", "c", "rule2")
        attr3 = GeneratedAttribute("p1", "ec", "v", "rule1")
        
        assert attr1 == attr2  # Same content, different source rule
        assert attr1 != attr3  # Different value
        assert hash(attr1) == hash(attr2)
    
    def test_inference_results_duplicate_prevention(self):
        """Test that results prevent duplicates."""
        results = InferenceResults()
        
        rel1 = GeneratedRelation("parentesco", "pai", "p1", "c1", "rule1")
        rel2 = GeneratedRelation("parentesco", "pai", "p1", "c1", "rule2")
        
        results.add_relation(rel1)
        results.add_relation(rel2)  # Duplicate, should not be added
        
        assert len(results.relations) == 1
        
        attr1 = GeneratedAttribute("p1", "ec", "c", "rule1")
        attr2 = GeneratedAttribute("p1", "ec", "c", "rule2")
        
        results.add_attribute(attr1)
        results.add_attribute(attr2)  # Duplicate, should not be added
        
        assert len(results.attributes) == 1


class TestInferenceEngine:
    """Tests for the inference engine."""
    
    def create_group(self, name: str, id: str, children: list = None) -> ParsedGroup:
        """Helper to create a ParsedGroup."""
        return ParsedGroup(
            name=name,
            id=id,
            children=children or []
        )
    
    def test_engine_initialization(self):
        """Test engine initialization."""
        engine = InferenceEngine()
        assert len(engine.get_rules()) == 0
    
    def test_register_rule(self):
        """Test registering rules."""
        engine = InferenceEngine()
        rule = InferenceRule(
            name="test_rule",
            conditions=[Condition(type=ConditionType.SEQUENCE)],
            actions=[]
        )
        engine.register_rule(rule)
        assert len(engine.get_rules()) == 1
    
    def test_simple_group_match(self):
        """Test simple group matching."""
        engine = InferenceEngine()
        
        # Create rule: match group "pai" and generate relation
        rule = InferenceRule(
            name="test_father",
            conditions=[
                Condition(type=ConditionType.GROUP, group_name="pai", bind_var="P"),
            ],
            actions=[
                Action(
                    type=ActionType.RELATION,
                    relation_type="test",
                    relation_value="father",
                    origin_var="P",
                    dest_var="P"
                )
            ]
        )
        engine.register_rule(rule)
        
        # Create group hierarchy
        pai = self.create_group("pai", "p1")
        root = self.create_group("root", "r1", [pai])
        
        # Apply rules
        results = engine.apply_rules([root], None)
        
        assert len(results.relations) == 1
        assert results.relations[0].value == "father"
        assert results.relations[0].origin_id == "p1"
    
    def test_extends_condition(self):
        """Test extends condition matching."""
        engine = InferenceEngine()
        
        # Create rule: match group extending "person"
        rule = InferenceRule(
            name="test_person",
            conditions=[
                Condition(type=ConditionType.EXTENDS, group_name="person", bind_var="P"),
            ],
            actions=[
                Action(
                    type=ActionType.RELATION,
                    relation_type="test",
                    relation_value="is_person",
                    origin_var="P",
                    dest_var="P"
                )
            ]
        )
        engine.register_rule(rule)
        
        # Create group hierarchy - "actorm" extends "person"
        actor = self.create_group("actorm", "a1")
        root = self.create_group("root", "r1", [actor])
        
        # Apply rules
        results = engine.apply_rules([root], None)
        
        assert len(results.relations) == 1
        assert results.relations[0].origin_id == "a1"
    
    def test_sequence_matching(self):
        """Test sequence condition matching."""
        engine = InferenceEngine()
        
        # Create rule: sequence(_) followed by pai
        rule = InferenceRule(
            name="test_sequence",
            conditions=[
                Condition(type=ConditionType.SEQUENCE),
                Condition(type=ConditionType.GROUP, group_name="pai", bind_var="P"),
            ],
            actions=[
                Action(
                    type=ActionType.RELATION,
                    relation_type="test",
                    relation_value="found",
                    origin_var="P",
                    dest_var="P"
                )
            ]
        )
        engine.register_rule(rule)
        
        # Create nested hierarchy: root -> middle -> pai
        pai = self.create_group("pai", "p1")
        middle = self.create_group("middle", "m1", [pai])
        root = self.create_group("root", "r1", [middle])
        
        # Apply rules
        results = engine.apply_rules([root], None)
        
        assert len(results.relations) == 1
        assert results.relations[0].origin_id == "p1"
    
    def test_father_child_relation(self):
        """Test father-child relation generation."""
        engine = InferenceEngine()
        
        # Create rule: if [sequence(_),extends(actorm,N),pai(P)] then relation(parentesco,pai,P,N)
        rule = InferenceRule(
            name="father_of_male_actor",
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
        )
        engine.register_rule(rule)
        
        # Create: actorm$a1 containing pai$p1
        pai = self.create_group("pai", "p1")
        actor = self.create_group("actorm", "a1", [pai])
        root = self.create_group("root", "r1", [actor])
        
        # Apply rules
        results = engine.apply_rules([root], None)
        
        assert len(results.relations) == 1
        rel = results.relations[0]
        assert rel.rel_type == "parentesco"
        assert rel.value == "pai"
        assert rel.origin_id == "p1"  # father
        assert rel.dest_id == "a1"    # child
    
    def test_attribute_generation(self):
        """Test attribute generation."""
        engine = InferenceEngine()
        
        # Create rule that generates an attribute
        rule = InferenceRule(
            name="test_attribute",
            conditions=[
                Condition(type=ConditionType.GROUP, group_name="person", bind_var="P"),
            ],
            actions=[
                Action(
                    type=ActionType.ATTRIBUTE,
                    attr_entity_var="P",
                    attr_type="status",
                    attr_value="active"
                )
            ]
        )
        engine.register_rule(rule)
        
        # Create group
        person = self.create_group("person", "p1")
        root = self.create_group("root", "r1", [person])
        
        # Apply rules
        results = engine.apply_rules([root], None)
        
        assert len(results.attributes) == 1
        attr = results.attributes[0]
        assert attr.entity_id == "p1"
        assert attr.attr_type == "status"
        assert attr.attr_value == "active"
    
    def test_multiple_actions(self):
        """Test rule with multiple actions."""
        engine = InferenceEngine()
        
        # Create rule with multiple actions
        rule = InferenceRule(
            name="multi_action",
            conditions=[
                Condition(type=ConditionType.GROUP, group_name="person", bind_var="P"),
            ],
            actions=[
                Action(
                    type=ActionType.RELATION,
                    relation_type="rel1",
                    relation_value="val1",
                    origin_var="P",
                    dest_var="P"
                ),
                Action(
                    type=ActionType.ATTRIBUTE,
                    attr_entity_var="P",
                    attr_type="attr1",
                    attr_value="val1"
                ),
            ]
        )
        engine.register_rule(rule)
        
        # Create group
        person = self.create_group("person", "p1")
        root = self.create_group("root", "r1", [person])
        
        # Apply rules
        results = engine.apply_rules([root], None)
        
        assert len(results.relations) == 1
        assert len(results.attributes) == 1


class TestYAMLLoader:
    """Tests for YAML rule loading."""
    
    def test_load_simple_rule(self, tmp_path: Path):
        """Test loading a simple rule from YAML."""
        yaml_content = """
rules:
  - name: test_rule
    description: "Test rule"
    priority: 1
    when:
      - sequence: any
      - group: {name: pai, bind: father_id}
    then:
      - relation: {type: parentesco, value: pai, origin: father_id, dest: child_id}
"""
        rules_file = tmp_path / "test_rules.yaml"
        rules_file.write_text(yaml_content)
        
        rules = load_rules_from_yaml(rules_file)
        
        assert len(rules) == 1
        assert rules[0].name == "test_rule"
        assert rules[0].priority == 1
        assert len(rules[0].conditions) == 2
        assert len(rules[0].actions) == 1
    
    def test_load_extends_condition(self, tmp_path: Path):
        """Test loading extends condition."""
        yaml_content = """
rules:
  - name: extends_rule
    when:
      - sequence: any
      - extends: {base: actorm, bind: actor_id}
    then:
      - relation: {type: test, value: val, origin: actor_id, dest: actor_id}
"""
        rules_file = tmp_path / "test_rules.yaml"
        rules_file.write_text(yaml_content)
        
        rules = load_rules_from_yaml(rules_file)
        
        assert len(rules) == 1
        assert rules[0].conditions[1].type == ConditionType.EXTENDS
        assert rules[0].conditions[1].group_name == "actorm"
        assert rules[0].conditions[1].bind_var == "actor_id"
    
    def test_load_attribute_action(self, tmp_path: Path):
        """Test loading attribute action."""
        yaml_content = """
rules:
  - name: attribute_rule
    when:
      - group: {name: person, bind: person_id}
    then:
      - attribute: {entity: person_id, type: ec, value: c}
"""
        rules_file = tmp_path / "test_rules.yaml"
        rules_file.write_text(yaml_content)
        
        rules = load_rules_from_yaml(rules_file)
        
        assert len(rules) == 1
        action = rules[0].actions[0]
        assert action.type == ActionType.ATTRIBUTE
        assert action.attr_entity_var == "person_id"
        assert action.attr_type == "ec"
        assert action.attr_value == "c"
    
    def test_load_newscope_action(self, tmp_path: Path):
        """Test loading newscope action."""
        yaml_content = """
rules:
  - name: scope_rule
    when:
      - group: act
    then:
      - newscope: true
"""
        rules_file = tmp_path / "test_rules.yaml"
        rules_file.write_text(yaml_content)
        
        rules = load_rules_from_yaml(rules_file)
        
        assert len(rules) == 1
        assert rules[0].actions[0].type == ActionType.NEW_SCOPE
    
    def test_file_not_found(self):
        """Test error when file doesn't exist."""
        with pytest.raises(FileNotFoundError):
            load_rules_from_yaml(Path("/nonexistent/rules.yaml"))
    
    def test_save_and_load_rules(self, tmp_path: Path):
        """Test saving and loading rules roundtrip."""
        # Create rules
        rules = [
            InferenceRule(
                name="test_rule",
                description="Test description",
                priority=5,
                conditions=[
                    Condition(type=ConditionType.SEQUENCE),
                    Condition(type=ConditionType.GROUP, group_name="pai", bind_var="P"),
                ],
                actions=[
                    Action(
                        type=ActionType.RELATION,
                        relation_type="parentesco",
                        relation_value="pai",
                        origin_var="P",
                        dest_var="C"
                    ),
                ]
            )
        ]
        
        # Save rules
        rules_file = tmp_path / "saved_rules.yaml"
        save_rules_to_yaml(rules, rules_file)
        
        # Load rules back
        loaded_rules = load_rules_from_yaml(rules_file)
        
        assert len(loaded_rules) == 1
        assert loaded_rules[0].name == "test_rule"
        assert loaded_rules[0].description == "Test description"
        assert loaded_rules[0].priority == 5
        assert len(loaded_rules[0].conditions) == 2
        assert len(loaded_rules[0].actions) == 1


class TestDefaultRules:
    """Tests for default built-in rules."""
    
    def test_get_default_rules(self):
        """Test loading default rules."""
        rules = get_default_rules()
        
        # Should have multiple rules
        assert len(rules) > 0
        
        # Check for expected rule names
        rule_names = {r.name for r in rules}
        assert "father_of_male_actor" in rule_names
        assert "mother_of_male_actor" in rule_names
    
    def test_father_rule_structure(self):
        """Test structure of father rule."""
        rules = get_default_rules()
        
        father_rule = next(r for r in rules if r.name == "father_of_male_actor")
        
        # Check conditions
        assert len(father_rule.conditions) == 3
        assert father_rule.conditions[0].type == ConditionType.SEQUENCE
        assert father_rule.conditions[1].type == ConditionType.EXTENDS
        assert father_rule.conditions[1].group_name == "actorm"
        assert father_rule.conditions[2].type == ConditionType.GROUP
        assert father_rule.conditions[2].group_name == "pai"
        
        # Check actions
        assert len(father_rule.actions) == 1
        assert father_rule.actions[0].type == ActionType.RELATION
        assert father_rule.actions[0].relation_value == "pai"
    
    def test_default_rules_with_engine(self):
        """Test applying default rules with engine."""
        engine = InferenceEngine()
        
        # Load default rules
        rules = get_default_rules()
        for rule in rules:
            engine.register_rule(rule)
        
        # Create a simple hierarchy: actorm with pai
        pai = ParsedGroup(name="pai", id="p1")
        actor = ParsedGroup(name="actorm", id="a1", children=[pai])
        root = ParsedGroup(name="root", id="r1", children=[actor])
        
        # Apply rules
        results = engine.apply_rules([root], None)
        
        # Should generate father relation
        father_rels = [r for r in results.relations if r.value == "pai"]
        assert len(father_rels) >= 1
        
        # Check the relation
        rel = father_rels[0]
        assert rel.origin_id == "p1"
        assert rel.dest_id == "a1"


class TestComplexScenarios:
    """Tests for complex inference scenarios."""
    
    def create_group(self, name: str, id: str, children: list = None) -> ParsedGroup:
        """Helper to create a ParsedGroup."""
        return ParsedGroup(
            name=name,
            id=id,
            children=children or []
        )
    
    def test_full_family_tree(self):
        """Test inference on a full family tree."""
        engine = InferenceEngine()
        
        # Load default rules
        rules = get_default_rules()
        for rule in rules:
            engine.register_rule(rule)
        
        # Create family tree:
        # actorm$a1
        #   pai$p1
        #   mae$m1
        #   irmao$s1
        mae = self.create_group("mae", "m1")
        pai = self.create_group("pai", "p1")
        irmao = self.create_group("irmao", "s1")
        actor = self.create_group("actorm", "a1", [pai, mae, irmao])
        root = self.create_group("root", "r1", [actor])
        
        # Apply rules
        results = engine.apply_rules([root], None)
        
        # Should have father, mother, and brother relations
        values = {r.value for r in results.relations}
        assert "pai" in values
        assert "mae" in values
        assert "irmao" in values
    
    def test_marriage_relations(self):
        """Test marriage relation generation."""
        engine = InferenceEngine()
        
        # Add marriage rule
        rule = InferenceRule(
            name="male_actor_with_wife",
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
            ]
        )
        engine.register_rule(rule)
        
        # Create: actorm with mulher
        mulher = self.create_group("mulher", "w1")
        actor = self.create_group("actorm", "h1", [mulher])
        root = self.create_group("root", "r1", [actor])
        
        # Apply rules
        results = engine.apply_rules([root], None)
        
        # Should have husband relation and attribute
        assert len(results.relations) == 1
        assert results.relations[0].value == "marido"
        assert len(results.attributes) == 1
        assert results.attributes[0].attr_type == "ec"


class TestMultiPathConditions:
    """Tests for cross-path AND rules (condition_paths)."""

    def create_group(self, name: str, id: str, children: list = None) -> ParsedGroup:
        """Helper to create a ParsedGroup."""
        return ParsedGroup(
            name=name,
            id=id,
            children=children or []
        )

    def _parents_couple_rule(self) -> InferenceRule:
        """A cross-path parents_couple rule joining on child_id."""
        return InferenceRule(
            name="parents_couple",
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
                    dest_var="mother_id",
                ),
                Action(
                    type=ActionType.ATTRIBUTE,
                    attr_entity_var="father_id",
                    attr_type="ec",
                    attr_value="c",
                ),
                Action(
                    type=ActionType.ATTRIBUTE,
                    attr_entity_var="mother_id",
                    attr_type="ec",
                    attr_value="c",
                ),
            ],
        )

    def test_multi_path_parents_couple(self):
        """actorm with both pai and mae -> marido relation + ec=c on both."""
        engine = InferenceEngine()
        engine.register_rule(self._parents_couple_rule())

        pai = self.create_group("pai", "p1")
        mae = self.create_group("mae", "m1")
        actor = self.create_group("actorm", "a1", [pai, mae])
        root = self.create_group("fonte", "f1", [actor])

        results = engine.apply_rules([root], None)

        assert len(results.relations) == 1
        rel = results.relations[0]
        assert rel.rel_type == "parentesco"
        assert rel.value == "marido"
        assert rel.origin_id == "p1"
        assert rel.dest_id == "m1"

        # Both parents get ec=c
        ec_entities = {a.entity_id for a in results.attributes if a.attr_type == "ec"}
        assert ec_entities == {"p1", "m1"}

    def test_multi_path_parents_couple_no_mae(self):
        """Without mae the cross-path rule must not fire."""
        engine = InferenceEngine()
        engine.register_rule(self._parents_couple_rule())

        pai = self.create_group("pai", "p1")
        actor = self.create_group("actorm", "a1", [pai])
        root = self.create_group("fonte", "f1", [actor])

        results = engine.apply_rules([root], None)
        assert results.relations == []
        assert results.attributes == []

    def test_multi_path_join_on_shared_ancestor(self):
        """Two actorm siblings each with pai+mae: father must link only to
        the mother of the *same* child (join via child_id)."""
        engine = InferenceEngine()
        engine.register_rule(self._parents_couple_rule())

        a1 = self.create_group("actorm", "a1", [
            self.create_group("pai", "p1"),
            self.create_group("mae", "m1"),
        ])
        a2 = self.create_group("actorm", "a2", [
            self.create_group("pai", "p2"),
            self.create_group("mae", "m2"),
        ])
        root = self.create_group("fonte", "f1", [a1, a2])

        results = engine.apply_rules([root], None)

        marido_pairs = {
            (r.origin_id, r.dest_id)
            for r in results.relations
            if r.value == "marido"
        }
        assert marido_pairs == {("p1", "m1"), ("p2", "m2")}

    def test_multi_path_marriage_groom_bride(self):
        """cas with noivo+noiva -> marido/mulher relations joining on cas."""
        rule = InferenceRule(
            name="marriage_groom_bride",
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
                    dest_var="bride_id",
                ),
            ],
        )
        engine = InferenceEngine()
        engine.register_rule(rule)

        cas = self.create_group("cas", "c1", [
            self.create_group("noivo", "g1"),
            self.create_group("noiva", "b1"),
        ])
        root = self.create_group("fonte", "f1", [cas])

        results = engine.apply_rules([root], None)
        assert len(results.relations) == 1
        rel = results.relations[0]
        assert rel.value == "marido"
        assert rel.origin_id == "g1"
        assert rel.dest_id == "b1"

    def test_unbound_variable_skips_action(self):
        """A rule that references an unbound variable in `then` must emit
        nothing (rather than using the literal var name as an id)."""
        rule = InferenceRule(
            name="rule_with_unbound_var",
            conditions=[
                Condition(type=ConditionType.SEQUENCE),
                Condition(type=ConditionType.GROUP, group_name="pai", bind_var="father_id"),
            ],
            # mother_id is never bound by any condition
            actions=[
                Action(
                    type=ActionType.RELATION,
                    relation_type="parentesco",
                    relation_value="marido",
                    origin_var="father_id",
                    dest_var="mother_id",
                ),
            ],
        )
        engine = InferenceEngine()
        engine.register_rule(rule)

        pai = self.create_group("pai", "p1")
        root = self.create_group("fonte", "f1", [pai])

        results = engine.apply_rules([root], None)
        assert results.relations == []
        assert results.attributes == []


class TestYAMLWhenAnyLoader:
    """Tests for loading/saving the when_any (cross-path) YAML form."""

    def test_load_when_any_rule(self, tmp_path):
        """A YAML rule with when_any is parsed into condition_paths."""
        from kleio.inference.loader import load_rules_from_yaml

        yaml_text = """
rules:
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
        path = tmp_path / "rules.yaml"
        path.write_text(yaml_text)

        rules = load_rules_from_yaml(path)
        assert len(rules) == 1
        rule = rules[0]
        assert rule.name == "parents_couple"
        assert len(rule.condition_paths) == 2
        # The flat conditions list is empty (no when: key was given)
        assert rule.conditions == []
        # Join variable is present in both sub-paths
        sub0_vars = {c.bind_var for c in rule.condition_paths[0]}
        sub1_vars = {c.bind_var for c in rule.condition_paths[1]}
        assert "child_id" in sub0_vars
        assert "child_id" in sub1_vars

    def test_save_and_load_when_any_roundtrip(self, tmp_path):
        """A rule with condition_paths survives save/load round-trip."""
        from kleio.inference.loader import load_rules_from_yaml, save_rules_to_yaml

        original = InferenceRule(
            name="rt_rule",
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
                    dest_var="mother_id",
                ),
            ],
        )
        path = tmp_path / "rules.yaml"
        save_rules_to_yaml([original], path)
        reloaded = load_rules_from_yaml(path)

        assert len(reloaded) == 1
        r = reloaded[0]
        assert r.name == "rt_rule"
        assert len(r.condition_paths) == 2
        assert r.condition_paths[0][1].group_name == "actorm"
        assert r.condition_paths[0][2].group_name == "pai"

    def test_default_rules_contain_multi_path_rules(self):
        """The packaged default rules now use when_any for the three
        previously-broken cross-path rules."""
        from kleio.inference import get_default_rules

        names = {r.name: r for r in get_default_rules()}
        for expected in ("parents_couple", "marriage_groom_bride", "marriage_bride_groom"):
            assert expected in names, f"missing rule {expected}"
            assert len(names[expected].condition_paths) >= 2, (
                f"{expected} should have >=2 sub-paths"
            )


class TestGroupExtends:
    """Tests for InferenceEngine._group_extends source-chain resolution.

    The inference rules use ``extends: {base: <class>}`` conditions. The
    engine must walk the full ``source`` (fons) inheritance chain so that
    e.g. ``n`` (which extends ``actorm -> male -> person -> entity``) is
    recognised as extending ``actorm``, ``male``, ``person`` and
    ``entity``.
    """

    def create_group(self, name: str, id: str = "x") -> ParsedGroup:
        return ParsedGroup(name=name, id=id, children=[])

    def test_no_schema_fallback_map(self):
        """Without a schema, the built-in fallback map covers the core
        actor classes and their ancestor chain."""
        engine = InferenceEngine()
        # n extends the full male lineage
        for base in ("actorm", "male", "person", "entity"):
            assert engine._group_extends(self.create_group("n"), base, None) is True
        # n does NOT extend the female lineage
        assert engine._group_extends(self.create_group("n"), "actorf", None) is False
        assert engine._group_extends(self.create_group("n"), "female", None) is False
        # actorf extends the female lineage
        for base in ("actorf", "female", "person", "entity"):
            assert engine._group_extends(self.create_group("actorf"), base, None) is True
        # actorf does NOT extend the male lineage
        assert engine._group_extends(self.create_group("actorf"), "actorm", None) is False

    def test_chain_walk_with_synthetic_schema(self):
        """A custom schema: n -> actorm -> male -> person -> entity.
        Every ancestor in the chain must be matched; an unrelated class
        must not."""
        from kleio.schema.models import StructureDef, GroupDef

        struct = StructureDef()
        chain = {
            "n": "actorm",
            "actorm": "male",
            "male": "person",
            "person": "entity",
            "entity": "",
        }
        struct.groups = {name: GroupDef(name=name, source=src) for name, src in chain.items()}

        engine = InferenceEngine()
        for ancestor in ("actorm", "male", "person", "entity"):
            assert engine._group_extends(self.create_group("n"), ancestor, struct) is True
        # Direct name match also works
        assert engine._group_extends(self.create_group("n"), "n", struct) is True
        # Unrelated class
        assert engine._group_extends(self.create_group("n"), "actorf", struct) is False

    def test_chain_walk_with_base_class_field(self):
        """If the schema only has the pre-resolved ``base_class`` field
        (e.g. base_class='person' on a group whose source chain is
        incomplete), it is still treated as an ancestor."""
        from kleio.schema.models import StructureDef, GroupDef

        struct = StructureDef()
        # n -> actorm -> (actorm.source='' so chain would stop), but
        # actorm.base_class='person' is pre-resolved.
        struct.groups = {
            "n": GroupDef(name="n", source="actorm", base_class="person"),
            "actorm": GroupDef(name="actorm", source="", base_class="person"),
        }

        engine = InferenceEngine()
        # Reaches 'person' via actorm.base_class even though source is empty
        assert engine._group_extends(self.create_group("n"), "person", struct) is True
        assert engine._group_extends(self.create_group("n"), "actorm", struct) is True

    def test_chain_walk_handles_cycles(self):
        """A cycle in the ``source`` chain must terminate, not hang."""
        from kleio.schema.models import StructureDef, GroupDef

        struct = StructureDef()
        struct.groups = {
            "a": GroupDef(name="a", source="b"),
            "b": GroupDef(name="b", source="a"),
        }
        engine = InferenceEngine()
        # Should return False quickly without infinite-looping.
        assert engine._group_extends(self.create_group("a"), "zzz", struct) is False

    def test_chain_walk_handles_missing_intermediate(self, caplog):
        """If an intermediate group is absent from the schema, the walk
        stops gracefully at that point without raising, and a warning is
        emitted naming the missing group."""
        import logging
        from kleio.schema.models import StructureDef, GroupDef

        struct = StructureDef()
        # zz -> actorm, but 'actorm' is not in the schema and the
        # group is not in the fallback map either.
        struct.groups = {"zz": GroupDef(name="zz", source="actorm")}
        engine = InferenceEngine()
        with caplog.at_level(logging.WARNING, logger="kleio.inference.engine"):
            # 'actorm' is still matched because it is the direct source of zz.
            assert engine._group_extends(self.create_group("zz"), "actorm", struct) is True
            # Beyond 'actorm' the chain is unknown; an unrelated class is False.
            assert engine._group_extends(self.create_group("zz"), "person", struct) is False

        # The missing intermediate ('actorm') must be reported once.
        warnings = [r for r in caplog.records if r.levelno == logging.WARNING]
        assert any("actorm" in r.getMessage() and "not defined" in r.getMessage()
                   for r in warnings), (
            f"expected a warning about missing 'actorm'; got: {[r.getMessage() for r in warnings]}"
        )

    def test_missing_intermediate_warning_emitted_once(self, caplog):
        """The missing-intermediate warning is deduplicated: calling the
        engine on many paths that hit the same missing group emits the
        warning only once per engine."""
        import logging
        from kleio.schema.models import StructureDef, GroupDef

        struct = StructureDef()
        # a -> missing, b -> missing
        struct.groups = {
            "a": GroupDef(name="a", source="missing-parent"),
            "b": GroupDef(name="b", source="missing-parent"),
        }
        engine = InferenceEngine()
        with caplog.at_level(logging.WARNING, logger="kleio.inference.engine"):
            # Hit the same missing parent from two different starting groups.
            engine._group_extends(self.create_group("a"), "zzz", struct)
            engine._group_extends(self.create_group("a"), "zzz", struct)
            engine._group_extends(self.create_group("b"), "zzz", struct)

        warnings = [r for r in caplog.records
                    if r.levelno == logging.WARNING and "missing-parent" in r.getMessage()]
        assert len(warnings) == 1, (
            f"expected exactly one warning for 'missing-parent'; got {len(warnings)}"
        )

    def test_gacto2_schema_full_chain(self):
        """Against the real gacto2.str.yaml, ``n`` extends the full male
        lineage and ``bap`` extends ``historical-act``."""
        from kleio.schema.registry import SchemaRegistry
        from kleio.errors import ErrorAccumulator

        schema = SchemaRegistry()
        errors = ErrorAccumulator()
        gacto2 = Path("tests/kleio-home/structures/gacto2.str.yaml")
        if not gacto2.exists():
            pytest.skip("gacto2.str.yaml not found")
        schema.load(gacto2, errors)

        engine = InferenceEngine()
        struct = schema.structure
        # n extends every male ancestor
        for ancestor in ("actorm", "male", "person"):
            assert engine._group_extends(self.create_group("n"), ancestor, struct) is True
        # n does not extend female ancestors
        assert engine._group_extends(self.create_group("n"), "actorf", struct) is False
        assert engine._group_extends(self.create_group("n"), "female", struct) is False
        # bap extends historical-act (bap -> pt-acto -> historical-act)
        assert engine._group_extends(self.create_group("bap"), "historical-act", struct) is True


class TestSubgroupFieldMerging:
    """Tests for the loader merging all subgroup fields (part/arbitrary/
    always/only/contains) into a single containment set.

    The Prolog parser used pars/repetitio/semper/solum with subtle
    distinctions; in the Python version these are treated as synonyms
    at this stage, merged into ``group_def.contains`` at load time.
    This fixes the root cause where ``pai.part: [ppai, mpai]`` in
    gacto2 was being ignored and the parser produced flat sibling
    paths instead of the Prolog-style nested paths.
    """

    def test_loader_merges_all_subgroup_fields(self):
        """``part``, ``arbitrary``, ``always``, ``only`` and ``contains``
        all contribute to ``group_def.contains`` after loading gacto2."""
        from kleio.schema.registry import SchemaRegistry
        from kleio.errors import ErrorAccumulator

        schema = SchemaRegistry()
        errors = ErrorAccumulator()
        gacto2 = Path("tests/kleio-home/structures/gacto2.str.yaml")
        if not gacto2.exists():
            pytest.skip("gacto2.str.yaml not found")
        schema.load(gacto2, errors)

        # pai uses the YAML "part:" field for its subgroups.
        pai = schema.structure.groups.get("pai")
        assert pai is not None
        assert "ppai" in pai.contains, "pai.contains should include ppai (from part:)"
        assert "mpai" in pai.contains, "pai.contains should include mpai (from part:)"

        # mae uses "part:" too.
        mae = schema.structure.groups.get("mae")
        assert mae is not None
        assert "pmae" in mae.contains
        assert "mmae" in mae.contains

        # contained_by sees the merged set.
        assert schema.contained_by("ppai", "pai") is True
        assert schema.contained_by("mpai", "pai") is True
        assert schema.contained_by("pmae", "mae") is True
        assert schema.contained_by("mmae", "mae") is True

        # Groups defined with "arbitrary:" still have their subgroups in contains.
        # (n inherits actorm's arbitrary list.)
        n = schema.structure.groups.get("n")
        assert n is not None
        assert "pai" in n.contains

    def test_parser_nests_ppai_under_pai(self):
        """With the loader fix, the parser attaches ppai as a child of
        pai (Prolog-style longest-path attachment), not as a flat
        sibling under n."""
        from kleio.schema.registry import SchemaRegistry
        from kleio.parser.builder import translate_string
        from kleio.errors import ErrorAccumulator

        schema = SchemaRegistry()
        errors = ErrorAccumulator()
        gacto2 = Path("tests/kleio-home/structures/gacto2.str.yaml")
        if not gacto2.exists():
            pytest.skip("gacto2.str.yaml not found")
        schema.load(gacto2, errors)

        source = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1800\n"
            "         n$joao/m/id=c1\n"
            "            pai$pedro/id=f1\n"
            "               ppai$luis/id=gf1\n"
        )
        groups = translate_string(source, schema, errors)

        # Find the ppai group and check its path ends in pai, not n.
        ppai = next(g for g in groups if g.name == "ppai")
        ancestor_names = [name for name, _ in ppai.path]
        assert ancestor_names[-1] == "pai", (
            f"ppai should be nested under pai, but path ends in "
            f"{ancestor_names[-1]!r}; full path: {ancestor_names}"
        )

    def test_grandparent_rules_fire_on_nested_structure(self):
        """End-to-end: the four grandparent rules fire on a po-style
        act where pai contains ppai/mpai and mae contains pmae/mmae.

        Group-name semantics (per inference.pl:247-258):
          - ppai = father of pai -> relation(pai, ppai, pai)
          - mpai = mother of pai -> relation(mae, mpai, pai)
          - pmae = father of mae -> relation(pai, pmae, mae)
          - mmae = mother of mae -> relation(mae, mmae, mae)
        """
        from kleio.schema.registry import SchemaRegistry
        from kleio.parser.builder import translate_string
        from kleio.errors import ErrorAccumulator

        schema = SchemaRegistry()
        errors = ErrorAccumulator()
        gacto2 = Path("tests/kleio-home/structures/gacto2.str.yaml")
        if not gacto2.exists():
            pytest.skip("gacto2.str.yaml not found")
        schema.load(gacto2, errors)

        source = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      po$p1/cx.1/18000000\n"
            "         n$joao/m/id=c1\n"
            "            pai$pedro/id=f1\n"
            "               ppai$luis/id=gf1\n"
            "               mpai$rita/id=gm1\n"
            "            mae$ana/id=m1\n"
            "               pmae$jose/id=gf2\n"
            "               mmae$clara/id=gm2\n"
        )
        groups = translate_string(source, schema, errors)

        engine = InferenceEngine()
        for r in get_default_rules():
            engine.register_rule(r)
        results = engine.apply_rules(groups, schema.structure)

        grandparent_rels = {
            (r.source_rule, r.value, r.origin_id, r.dest_id)
            for r in results.relations
            if r.source_rule in (
                "paternal_grandfather", "paternal_grandmother",
                "maternal_grandfather", "maternal_grandmother",
            )
        }
        # Each grandparent rule fires once with the right origin/dest.
        assert ("paternal_grandfather", "pai", "gf1", "f1") in grandparent_rels
        assert ("paternal_grandmother", "mae", "gm1", "f1") in grandparent_rels
        assert ("maternal_grandfather", "pai", "gf2", "m1") in grandparent_rels
        assert ("maternal_grandmother", "mae", "gm2", "m1") in grandparent_rels
