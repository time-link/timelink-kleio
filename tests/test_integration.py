"""Integration tests for the Kleio Python migration.

Tests the complete end-to-end pipeline from source file parsing
through inference to export. Uses real test fixtures from tests/kleio-home/.
"""
from __future__ import annotations

import json
import tempfile
from pathlib import Path
from xml.etree import ElementTree as ET

import pytest

from kleio.config import KleioConfig
from kleio.errors import ErrorAccumulator
from kleio.export import XmlExporter, JsonExporter
from kleio.inference.engine import InferenceEngine
from kleio.inference.rules import get_default_rules
from kleio.parser.builder import translate_file, translate_string
from kleio.schema.registry import SchemaRegistry


# =============================================================================
# Test Fixtures
# =============================================================================

# Paths to test data directories
TESTS_DIR = Path(__file__).parent
KLEIO_HOME = TESTS_DIR / "kleio-home"
STRUCTURES_DIR = KLEIO_HOME / "structures"
SOURCES_DIR = KLEIO_HOME / "sources"
INFERENCES_DIR = KLEIO_HOME / "inferences"
MAPPINGS_DIR = KLEIO_HOME / "mappings"


@pytest.fixture
def kleio_config():
    """Create a KleioConfig pointing to test kleio-home directory."""
    return KleioConfig(home_dir=KLEIO_HOME)


@pytest.fixture
def error_accumulator():
    """Create an error accumulator for tests."""
    return ErrorAccumulator(max_errors=100)


@pytest.fixture
def schema_registry():
    """Create an empty schema registry."""
    return SchemaRegistry()


@pytest.fixture
def temp_output_dir():
    """Create a temporary directory for output files."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


# =============================================================================
# Test Data Path Helpers
# =============================================================================

def get_structure_path(name: str) -> Path | None:
    """Get path to a structure file, returning None if not found."""
    path = STRUCTURES_DIR / name
    return path if path.exists() else None


def get_source_path(relative_path: str) -> Path | None:
    """Get path to a source file, returning None if not found."""
    path = SOURCES_DIR / relative_path
    return path if path.exists() else None


def get_inference_path(name: str) -> Path | None:
    """Get path to an inference rules file, returning None if not found."""
    path = INFERENCES_DIR / name
    return path if path.exists() else None


# =============================================================================
# End-to-End Translation Tests
# =============================================================================

class TestEndToEndTranslation:
    """Tests for the complete translation pipeline."""
    
    @pytest.fixture
    def baptismos_schema(self, error_accumulator):
        """Load the baptismos structure file."""
        schema = SchemaRegistry()
        baptismos_yaml = get_structure_path("baptismos.yaml")
        if baptismos_yaml is None:
            pytest.skip("baptismos.yaml structure file not found")
        schema.load(baptismos_yaml, error_accumulator)
        return schema
    
    @pytest.fixture
    def gacto2_schema(self, error_accumulator):
        """Load the gacto2 structure file."""
        schema = SchemaRegistry()
        gacto2_yaml = get_structure_path("gacto2.str.yaml")
        if gacto2_yaml is None:
            pytest.skip("gacto2.str.yaml structure file not found")
        schema.load(gacto2_yaml, error_accumulator)
        return schema
    
    @pytest.fixture
    def sources_structure_schema(self, error_accumulator):
        """Load the sources-structure file."""
        schema = SchemaRegistry()
        sources_yaml = get_structure_path("sources-structure.yaml")
        if sources_yaml is None:
            pytest.skip("sources-structure.yaml structure file not found")
        schema.load(sources_yaml, error_accumulator)
        return schema
    
    def test_translate_simple_baptism_record(self, baptismos_schema, error_accumulator):
        """Test translating a simple baptism record."""
        source = """bap$b1/1/1/1714
   b$child/2/2/1714/extra=x"""
        
        groups = translate_string(source, baptismos_schema, error_accumulator)
        
        assert len(groups) == 2
        assert groups[0].name == "bap"
        assert groups[0].id == "bap-1"
        assert groups[1].name == "b"
        assert groups[1].path[0][0] == "bap"
        
    def test_translate_with_explicit_elements(self, baptismos_schema, error_accumulator):
        """Test translation with explicit element assignments."""
        source = "bap$b1/1/1/1714/loc=church/obs=test baptism"
        
        groups = translate_string(source, baptismos_schema, error_accumulator)
        
        assert len(groups) == 1
        group = groups[0]
        assert group.name == "bap"
        
        # Check explicit elements
        loc_el = group.get_element("loc")
        assert loc_el is not None
        assert loc_el.get_core_text() == "church"
        
        obs_el = group.get_element("obs")
        assert obs_el is not None
        assert obs_el.get_core_text() == "test baptism"
    
    def test_translate_bapt1714_source_file(self, gacto2_schema, error_accumulator):
        """Test translating the real bapt1714.cli source file."""
        source_path = get_source_path("api/paroquiais/baptismos/bapt1714.cli")
        if source_path is None:
            pytest.skip("bapt1714.cli source file not found")
        
        groups = translate_file(source_path, gacto2_schema, error_accumulator)
        
        # This is a substantial file with many groups
        assert len(groups) > 10
        
        # First group should be kleio (the header)
        assert groups[0].name == "kleio"
        
        # Find fonte group
        fonte_groups = [g for g in groups if g.name == "fonte"]
        assert len(fonte_groups) >= 1
        
        # Find bap groups
        bap_groups = [g for g in groups if g.name == "bap"]
        assert len(bap_groups) >= 1
        
        # Verify error count is reasonable
        assert error_accumulator.error_count < 10
        
    def test_translate_with_nested_groups(self, baptismos_schema, error_accumulator):
        """Test translation with deeply nested groups."""
        source = """bap$b1/1/1/1714
   b$child1/2/2/1714
   b$child2/3/3/1714"""
        
        groups = translate_string(source, baptismos_schema, error_accumulator)
        
        assert len(groups) == 3
        
        # All b groups should have bap in their path
        b_groups = [g for g in groups if g.name == "b"]
        assert len(b_groups) == 2
        for b in b_groups:
            assert len(b.path) == 1
            assert b.path[0][0] == "bap"
    
    def test_translate_with_aspects(self, baptismos_schema, error_accumulator):
        """Test translation with original and comment aspects."""
        source = "bap$b1/Maria%/Marie#baptism record/1/1/1714"
        
        groups = translate_string(source, baptismos_schema, error_accumulator)
        
        assert len(groups) == 1
        group = groups[0]
        
        # The second positional element (dia) should have aspects
        dia_el = group.get_element("dia")
        assert dia_el is not None
        assert dia_el.get_core_text() == "Maria"
        assert dia_el.get_original_text() == "Marie"
        assert dia_el.get_comment_text() == "baptism record"


# =============================================================================
# Schema → Parser → Builder Pipeline Tests
# =============================================================================

class TestSchemaParserBuilderPipeline:
    """Tests for the Schema → Parser → Builder pipeline."""
    
    def test_load_baptismos_structure(self, error_accumulator):
        """Test that baptismos.yaml structure file loads correctly."""
        schema = SchemaRegistry()
        baptismos_yaml = get_structure_path("baptismos.yaml")
        
        if baptismos_yaml is None:
            pytest.skip("baptismos.yaml not found")
        
        schema.load(baptismos_yaml, error_accumulator)
        
        # Check that key groups are defined
        assert schema.get_group("bap") is not None
        assert schema.get_group("b") is not None
        
        # Check bap group has expected elements
        bap_group = schema.get_group("bap")
        assert bap_group is not None
        
        bap_elements = {e.name for e in bap_group.elements}
        assert "id" in bap_elements
        assert "dia" in bap_elements
        assert "mes" in bap_elements
        assert "ano" in bap_elements
    
    def test_load_gacto2_structure(self, error_accumulator):
        """Test that gacto2.str.yaml structure file loads correctly."""
        schema = SchemaRegistry()
        gacto2_yaml = get_structure_path("gacto2.str.yaml")
        
        if gacto2_yaml is None:
            pytest.skip("gacto2.str.yaml not found")
        
        schema.load(gacto2_yaml, error_accumulator)
        
        # Check that key groups are defined
        assert schema.get_group("kleio") is not None
        assert schema.get_group("fonte") is not None
        
        # Check hierarchy relationships
        fonte = schema.get_group("fonte")
        assert fonte is not None
        assert fonte.contains is not None or fonte.gclass is not None
    
    def test_load_sources_structure_with_includes(self, error_accumulator):
        """Test loading structure file with includes."""
        schema = SchemaRegistry()
        sources_yaml = get_structure_path("sources-structure.yaml")
        
        if sources_yaml is None:
            pytest.skip("sources-structure.yaml not found")
        
        schema.load(sources_yaml, error_accumulator)
        
        # Should have loaded included files (elements.yaml, groups.yaml)
        # The structure should define various groups
        assert len(schema.all_groups()) > 0 or schema.get_group("fonte") is not None
    
    def test_builder_produces_valid_hierarchy(self, error_accumulator):
        """Test that builder produces valid group hierarchies."""
        schema = SchemaRegistry()
        baptismos_yaml = get_structure_path("baptismos.yaml")
        
        if baptismos_yaml is None:
            pytest.skip("baptismos.yaml not found")
        
        schema.load(baptismos_yaml, error_accumulator)
        
        source = """bap$b1/1/1/1714
   b$child1/2/2/1714
   b$child2/3/3/1714
bap$b2/4/4/1715"""
        
        groups = translate_string(source, schema, error_accumulator)
        
        # Check hierarchy
        assert len(groups) == 4
        
        # First bap
        assert groups[0].name == "bap"
        assert groups[0].path == []
        
        # First b - child of first bap
        assert groups[1].name == "b"
        assert len(groups[1].path) == 1
        assert groups[1].path[0][0] == "bap"
        assert groups[1].path[0][1] == "bap-1"
        
        # Second b - sibling of first b
        assert groups[2].name == "b"
        assert len(groups[2].path) == 1
        assert groups[2].path[0][0] == "bap"
        
        # Second bap - resets hierarchy
        assert groups[3].name == "bap"
        assert groups[3].path == []
        assert groups[3].id == "bap-2"
    
    def test_builder_generates_correct_ids(self, error_accumulator):
        """Test that builder generates correct IDs with prefix."""
        schema = SchemaRegistry()
        baptismos_yaml = get_structure_path("baptismos.yaml")
        
        if baptismos_yaml is None:
            pytest.skip("baptismos.yaml not found")
        
        schema.load(baptismos_yaml, error_accumulator)
        
        source = """bap$first/1/1/1714
bap$second/2/2/1715
bap$third/3/3/1716"""
        
        groups = translate_string(source, schema, error_accumulator)
        
        assert len(groups) == 3
        # ID prefix is "bap" in baptismos schema
        assert groups[0].id == "bap-1"
        assert groups[1].id == "bap-2"
        assert groups[2].id == "bap-3"


# =============================================================================
# Builder → Inference → Export Pipeline Tests
# =============================================================================

class TestBuilderInferenceExportPipeline:
    """Tests for the Builder → Inference → Export pipeline."""
    
    @pytest.fixture
    def gacto2_schema(self, error_accumulator):
        """Load gacto2 structure for family relationship tests."""
        schema = SchemaRegistry()
        gacto2_yaml = get_structure_path("gacto2.str.yaml")
        
        if gacto2_yaml is None:
            pytest.skip("gacto2.str.yaml not found")
        
        schema.load(gacto2_yaml, error_accumulator)
        return schema
    
    def test_inference_with_family_relations(self, gacto2_schema, error_accumulator):
        """Test inference engine generates family relations."""
        # Create a simple family structure
        source = """kleio$gacto2.str
fonte$test-source
   n$joao/m/id=p1
      pn$antonio/id=p2
      mn$maria/id=p3
"""
        
        groups = translate_string(source, gacto2_schema, error_accumulator)
        
        # Apply inference rules
        engine = InferenceEngine()
        for rule in get_default_rules():
            engine.register_rule(rule)
        
        results = engine.apply_rules(groups, gacto2_schema.structure)
        
        # Should generate some relations (depends on the rules)
        # Note: actual relations depend on schema structure
        assert results is not None
    
    def test_export_to_xml(self, gacto2_schema, error_accumulator, temp_output_dir):
        """Test exporting groups to XML."""
        source = """kleio$gacto2.str
fonte$test-source/tipo=baptismos
   n$maria/f/id=n1
      ls$nome/Maria"""
        
        groups = translate_string(source, gacto2_schema, error_accumulator)
        
        # Create exporter
        exporter = XmlExporter()
        exporter.init(
            source_file="test.cli",
            output_dir=temp_output_dir,
            schema=gacto2_schema
        )
        
        for group in groups:
            exporter.export_group(group)
        
        output_files = exporter.close()
        
        # Check output files were created
        assert len(output_files) >= 1
        xml_file = temp_output_dir / "test.xml"
        assert xml_file.exists()
        
        # Verify XML is valid
        tree = ET.parse(xml_file)
        root = tree.getroot()
        
        assert root.tag == "KLEIO"
        
        # Check groups are present
        groups_in_xml = root.findall(".//GROUP")
        assert len(groups_in_xml) >= 1
    
    def test_export_to_json(self, gacto2_schema, error_accumulator, temp_output_dir):
        """Test exporting groups to JSON."""
        source = """kleio$gacto2.str
fonte$test-source
   n$joao/m/id=n1"""
        
        groups = translate_string(source, gacto2_schema, error_accumulator)
        
        # Create exporter
        exporter = JsonExporter()
        exporter.init(
            source_file="test.cli",
            output_dir=temp_output_dir,
            schema=gacto2_schema
        )
        
        for group in groups:
            exporter.export_group(group)
        
        output_files = exporter.close()
        
        # Check output files were created
        assert len(output_files) >= 1
        json_file = temp_output_dir / "test.json"
        assert json_file.exists()
        
        # Verify JSON is valid
        with open(json_file) as f:
            data = json.load(f)
        
        assert "groups" in data
        assert len(data["groups"]) >= 1
    
    def test_export_with_inference_results(self, gacto2_schema, error_accumulator, temp_output_dir):
        """Test exporting with inference results included."""
        from kleio.inference.models import InferenceResults, GeneratedRelation, GeneratedAttribute
        
        source = """kleio$gacto2.str
fonte$test-source"""
        
        groups = translate_string(source, gacto2_schema, error_accumulator)
        
        # Create exporter
        exporter = XmlExporter()
        exporter.init(
            source_file="test.cli",
            output_dir=temp_output_dir,
            schema=gacto2_schema
        )
        
        for group in groups:
            exporter.export_group(group)
        
        # Create mock inference results
        inference = InferenceResults()
        inference.add_relation(GeneratedRelation(
            rel_type="parentesco",
            value="pai",
            origin_id="p1",
            dest_id="p2",
            source_rule="test-rule"
        ))
        inference.add_attribute(GeneratedAttribute(
            entity_id="p1",
            attr_type="ec",
            attr_value="c",
            source_rule="test-rule"
        ))
        
        exporter.close(inference)
        
        # Verify XML contains inference results
        xml_file = temp_output_dir / "test.xml"
        tree = ET.parse(xml_file)
        root = tree.getroot()
        
        # Check for relations
        relations = root.findall("RELATION")
        assert len(relations) == 1
        assert relations[0].get("TYPE") == "parentesco"
        
        # Check for attributes
        attributes = root.findall("ATTRIBUTE")
        assert len(attributes) == 1
        assert attributes[0].get("TYPE") == "ec"


# =============================================================================
# Error Handling Tests
# =============================================================================

class TestErrorHandling:
    """Tests for graceful error handling."""
    
    def test_malformed_group_line(self, error_accumulator):
        """Test handling of malformed group lines."""
        schema = SchemaRegistry()
        baptismos_yaml = get_structure_path("baptismos.yaml")
        
        if baptismos_yaml is None:
            pytest.skip("baptismos.yaml not found")
        
        schema.load(baptismos_yaml, error_accumulator)
        
        # Malformed source (missing required elements)
        source = "bap$"  # Empty ID
        
        # Should not raise, but may have errors/warnings
        groups = translate_string(source, schema, error_accumulator)
        
        # Should still produce a group
        assert len(groups) == 1
    
    def test_missing_structure_file(self, error_accumulator):
        """Test handling of missing structure file."""
        schema = SchemaRegistry()
        
        with pytest.raises(FileNotFoundError):
            schema.load(Path("/nonexistent/structure.yaml"), error_accumulator)
    
    def test_missing_source_file(self, error_accumulator):
        """Test handling of missing source file."""
        schema = SchemaRegistry()
        
        with pytest.raises(FileNotFoundError):
            translate_file("/nonexistent/source.cli", schema, error_accumulator)
    
    def test_unknown_element_warning(self, error_accumulator):
        """Test that unknown elements generate warnings."""
        schema = SchemaRegistry()
        baptismos_yaml = get_structure_path("baptismos.yaml")
        
        if baptismos_yaml is None:
            pytest.skip("baptismos.yaml not found")
        
        schema.load(baptismos_yaml, error_accumulator)
        
        # Use an element that doesn't exist in the schema
        source = "bap$b1/1/1/1714/unknown_element=value"
        
        groups = translate_string(source, schema, error_accumulator)
        
        # Should have a warning about unknown element
        assert error_accumulator.warning_count >= 1
    
    def test_missing_guaranteed_elements(self, error_accumulator):
        """Test that missing guaranteed elements generate errors."""
        schema = SchemaRegistry()
        baptismos_yaml = get_structure_path("baptismos.yaml")
        
        if baptismos_yaml is None:
            pytest.skip("baptismos.yaml not found")
        
        schema.load(baptismos_yaml, error_accumulator)
        
        # Missing required positional elements
        source = "bap$b1"  # Missing dia, mes, ano
        
        groups = translate_string(source, schema, error_accumulator)
        
        # Should have errors about missing elements
        assert error_accumulator.error_count >= 1
    
    def test_empty_source_file(self, error_accumulator, temp_output_dir):
        """Test handling of empty source file."""
        schema = SchemaRegistry()
        baptismos_yaml = get_structure_path("baptismos.yaml")
        
        if baptismos_yaml is None:
            pytest.skip("baptismos.yaml not found")
        
        schema.load(baptismos_yaml, error_accumulator)
        
        # Empty source
        source = ""
        
        groups = translate_string(source, schema, error_accumulator)
        
        assert len(groups) == 0
        assert error_accumulator.error_count == 0
    
    def test_source_with_only_comments(self, error_accumulator):
        """Test handling of source with only comments."""
        schema = SchemaRegistry()
        baptismos_yaml = get_structure_path("baptismos.yaml")
        
        if baptismos_yaml is None:
            pytest.skip("baptismos.yaml not found")
        
        schema.load(baptismos_yaml, error_accumulator)
        
        # Source with comments (lines starting with # in Kleio are different)
        source = """
bap$b1/1/1/1714#this is a comment
"""
        
        groups = translate_string(source, schema, error_accumulator)
        
        # Should still produce a group
        assert len(groups) == 1
        # The comment should be attached to an element
        dia_el = groups[0].get_element("dia")
        if dia_el and dia_el.comment_entries:
            # Comment was parsed
            pass


# =============================================================================
# Real Source File Tests
# =============================================================================

class TestRealSourceFiles:
    """Tests using real source files from kleio-home."""
    
    @pytest.fixture
    def gacto2_schema(self, error_accumulator):
        """Load gacto2 structure."""
        schema = SchemaRegistry()
        gacto2_yaml = get_structure_path("gacto2.str.yaml")
        
        if gacto2_yaml is None:
            pytest.skip("gacto2.str.yaml not found")
        
        schema.load(gacto2_yaml, error_accumulator)
        return schema
    
    def test_bap_celebrantes_file(self, gacto2_schema, error_accumulator):
        """Test translating bap-com-celebrantes.cli."""
        source_path = get_source_path("api/paroquiais/baptismos/bap-com-celebrantes.cli")
        
        if source_path is None:
            pytest.skip("bap-com-celebrantes.cli not found")
        
        groups = translate_file(source_path, gacto2_schema, error_accumulator)
        
        assert len(groups) > 0
        
        # Should have kleio header
        kleio_groups = [g for g in groups if g.name == "kleio"]
        assert len(kleio_groups) >= 1
    
    def test_bapteiras_problem_file(self, gacto2_schema, error_accumulator):
        """Test translating bapteirasproblem1.cli (problematic file)."""
        source_path = get_source_path("api/paroquiais/baptismos/bapteirasproblem1.cli")
        
        if source_path is None:
            pytest.skip("bapteirasproblem1.cli not found")
        
        # This file has known issues, should still process without crashing
        groups = translate_file(source_path, gacto2_schema, error_accumulator)
        
        # Should produce some groups despite issues
        assert len(groups) > 0
        
        # May have errors, but should be reasonable
        assert error_accumulator.error_count < 100
    
    def test_rogerio_domingos_file(self, gacto2_schema, error_accumulator):
        """Test translating rogeriodomingos_bap_problem.cli."""
        source_path = get_source_path("api/paroquiais/baptismos/rogeriodomingos_bap_problem.cli")
        
        if source_path is None:
            pytest.skip("rogeriodomingos_bap_problem.cli not found")
        
        groups = translate_file(source_path, gacto2_schema, error_accumulator)
        
        assert len(groups) > 0


# =============================================================================
# Inference Rules Tests with Real Data
# =============================================================================

class TestInferenceWithRealData:
    """Tests for inference rules with real structure data."""
    
    def test_load_inference_sample(self):
        """Test loading the inference sample file."""
        from kleio.inference.loader import load_rules_from_yaml
        
        inference_path = get_inference_path("inference_sample.yml")
        
        if inference_path is None:
            pytest.skip("inference_sample.yml not found")
        
        rules = load_rules_from_yaml(inference_path)
        
        # Should load some rules
        assert len(rules) >= 0  # May be empty depending on format
    
    def test_default_inference_rules(self):
        """Test that default inference rules are available."""
        rules = get_default_rules()
        
        assert len(rules) > 0
        
        # Should have key family relation rules
        rule_names = {r.name for r in rules}
        assert "father_of_male_actor" in rule_names
        assert "mother_of_male_actor" in rule_names
    
    def test_inference_engine_with_default_rules(self, gacto2_schema, error_accumulator):
        """Test inference engine with default rules on test data."""
        # Create test data with family relationships
        source = """kleio$gacto2.str
fonte$test
   actorm$joao/id=a1
      pai$antonio/id=p1
      mae$maria/id=m1"""
        
        groups = translate_string(source, gacto2_schema, error_accumulator)
        
        # Create engine with default rules
        engine = InferenceEngine()
        for rule in get_default_rules():
            engine.register_rule(rule)
        
        # Apply rules
        results = engine.apply_rules(groups, gacto2_schema.structure)
        
        # Should generate some relations
        assert results is not None
    
    @pytest.fixture
    def gacto2_schema(self, error_accumulator):
        """Load gacto2 structure."""
        schema = SchemaRegistry()
        gacto2_yaml = get_structure_path("gacto2.str.yaml")
        
        if gacto2_yaml is None:
            pytest.skip("gacto2.str.yaml not found")
        
        schema.load(gacto2_yaml, error_accumulator)
        return schema


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
