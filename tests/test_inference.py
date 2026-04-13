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
