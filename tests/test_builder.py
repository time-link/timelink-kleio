"""Tests for the Kleio group builder.

Tests the GroupBuilder class which assembles ParsedGroup objects from
parser actions, including path management, ID generation, and element validation.
"""
from __future__ import annotations

import pytest
from pathlib import Path

from kleio.parser.builder import GroupBuilder, translate_file, translate_string
from kleio.parser.models import (
    Aspect,
    Entry,
    ParsedElement,
    ParsedGroup,
    NewGroup,
    NewElement,
    EndElement,
    NewEntry,
    NewAspect,
    StoreCore,
)
from kleio.schema.registry import SchemaRegistry
from kleio.errors import ErrorAccumulator


# Path to test fixtures
TEST_DATA_DIR = Path(__file__).parent / "kleio-home" / "structures"
BAPTISMOS_YAML = TEST_DATA_DIR / "baptismos.yaml"
SAMPLE_STR_YAML = TEST_DATA_DIR / "sample-str.yaml"


class TestGroupBuilderBasic:
    """Basic tests for GroupBuilder functionality."""
    
    @pytest.fixture
    def schema(self):
        """Create a schema registry with baptismos.yaml loaded."""
        registry = SchemaRegistry()
        registry.load(BAPTISMOS_YAML)
        return registry
    
    @pytest.fixture
    def errors(self):
        """Create an error accumulator."""
        return ErrorAccumulator()
    
    def test_builder_initialization(self, schema, errors):
        """Test that GroupBuilder initializes correctly."""
        builder = GroupBuilder(schema, errors)
        
        assert builder.schema == schema
        assert builder.errors == errors
        assert builder.state.current_group == ""
        assert builder.state.path == []
        assert builder.completed_groups == []
    
    def test_simple_group_from_actions(self, schema, errors):
        """Test building a simple group from actions."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap$b1/1/1/1714")
        
        # Process actions for: bap$b1/1/1/1714
        actions = [
            NewGroup("bap"),
            StoreCore("b1"),
            EndElement(),
            StoreCore("1"),
            EndElement(),
            StoreCore("1"),
            EndElement(),
            StoreCore("1714"),
        ]
        builder.process_actions(actions)
        builder.close()
        
        # Should have one completed group
        assert len(builder.completed_groups) == 1
        
        group = builder.completed_groups[0]
        assert group.name == "bap"
        assert group.id == "bap-1"
        assert len(group.elements) == 4  # id, dia, mes, ano
        
        # Check elements
        assert group.elements[0].name == "id"
        assert group.elements[0].get_core_text() == "b1"
        assert group.elements[1].name == "dia"
        assert group.elements[1].get_core_text() == "1"
        assert group.elements[2].name == "mes"
        assert group.elements[2].get_core_text() == "1"
        assert group.elements[3].name == "ano"
        assert group.elements[3].get_core_text() == "1714"
    
    def test_group_with_explicit_elements(self, schema, errors):
        """Test building a group with explicit element assignments."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap$b1/date=mydate/typ=test")
        
        # Process actions for: bap$b1/date=mydate/typ=test
        actions = [
            NewGroup("bap"),
            StoreCore("b1"),
            EndElement(),
            NewElement("date"),
            StoreCore("mydate"),
            EndElement(),
            NewElement("typ"),
            StoreCore("test"),
        ]
        builder.process_actions(actions)
        builder.close()
        
        group = builder.completed_groups[0]
        assert group.name == "bap"
        
        # Find the explicit elements
        date_el = group.get_element("date")
        assert date_el is not None
        assert date_el.get_core_text() == "mydate"
        
        typ_el = group.get_element("typ")
        assert typ_el is not None
        assert typ_el.get_core_text() == "test"
    
    def test_callback_on_group_complete(self, schema, errors):
        """Test that callback is invoked when group is completed."""
        completed = []
        
        def on_complete(group):
            completed.append(group)
        
        builder = GroupBuilder(schema, errors, on_group_complete=on_complete)
        builder.set_context(1, "bap$b1/1/1/1714")
        
        # Process first group
        actions = [
            NewGroup("bap"),
            StoreCore("b1"),
            EndElement(),
            StoreCore("1"),
            EndElement(),
            StoreCore("1"),
            EndElement(),
            StoreCore("1714"),
        ]
        builder.process_actions(actions)
        
        # Process second group (flushes first)
        actions2 = [
            NewGroup("bap"),
            StoreCore("b2"),
            EndElement(),
            StoreCore("2"),
            EndElement(),
            StoreCore("2"),
            EndElement(),
            StoreCore("1715"),
        ]
        builder.process_actions(actions2)
        
        # Should have one completed group from callback
        assert len(completed) == 1
        assert completed[0].id == "bap-1"
        
        builder.close()
        
        # Now should have two
        assert len(completed) == 2
        assert completed[1].id == "bap-2"


class TestPathManagement:
    """Tests for path management (hierarchical group linking)."""
    
    @pytest.fixture
    def schema(self):
        """Create a schema registry with baptismos.yaml loaded."""
        registry = SchemaRegistry()
        registry.load(BAPTISMOS_YAML)
        return registry
    
    @pytest.fixture
    def errors(self):
        """Create an error accumulator."""
        return ErrorAccumulator()
    
    def test_sibling_groups_same_path(self, schema, errors):
        """Test that sibling groups share the same path."""
        builder = GroupBuilder(schema, errors)
        
        # First bap group
        builder.set_context(1, "bap$b1/1/1/1714")
        actions1 = [
            NewGroup("bap"),
            StoreCore("b1"),
            EndElement(),
            StoreCore("1"),
            EndElement(),
            StoreCore("1"),
            EndElement(),
            StoreCore("1714"),
        ]
        builder.process_actions(actions1)
        
        # Second bap group (sibling)
        builder.set_context(2, "bap$b2/2/2/1715")
        actions2 = [
            NewGroup("bap"),
            StoreCore("b2"),
            EndElement(),
            StoreCore("2"),
            EndElement(),
            StoreCore("2"),
            EndElement(),
            StoreCore("1715"),
        ]
        builder.process_actions(actions2)
        builder.close()
        
        # Both groups should have empty path (bap is not contained by anything in this schema)
        assert len(builder.completed_groups) == 2
        assert builder.completed_groups[0].path == []
        assert builder.completed_groups[1].path == []
    
    def test_nested_group_path(self, schema, errors):
        """Test path management with nested groups."""
        builder = GroupBuilder(schema, errors)
        
        # bap group
        builder.set_context(1, "bap$b1/1/1/1714")
        actions1 = [
            NewGroup("bap"),
            StoreCore("b1"),
            EndElement(),
            StoreCore("1"),
            EndElement(),
            StoreCore("1"),
            EndElement(),
            StoreCore("1714"),
        ]
        builder.process_actions(actions1)
        
        # b group (contained by bap per schema)
        builder.set_context(2, "b$child1/2/2/1714/extra=x")
        actions2 = [
            NewGroup("b"),
            StoreCore("child1"),
            EndElement(),
            StoreCore("2"),
            EndElement(),
            StoreCore("2"),
            EndElement(),
            StoreCore("1714"),
            EndElement(),
            StoreCore("x"),
        ]
        builder.process_actions(actions2)
        builder.close()
        
        # Check paths
        assert len(builder.completed_groups) == 2
        
        # First group (bap) has empty path
        bap_group = builder.completed_groups[0]
        assert bap_group.path == []
        
        # Second group (b) has bap in its path
        b_group = builder.completed_groups[1]
        assert len(b_group.path) == 1
        assert b_group.path[0][0] == "bap"
        assert b_group.path[0][1] == "bap-1"
    
    def test_path_cut_when_ancestor_found(self, schema, errors):
        """Test that path is cut when an ancestor is found.
        
        Uses 'b' group as a child of 'bap' to test path cutting.
        """
        builder = GroupBuilder(schema, errors)
        
        # bap group
        builder.set_context(1, "bap$b1/1/1/1714")
        builder.process_actions([
            NewGroup("bap"),
            StoreCore("b1"), EndElement(),
            StoreCore("1"), EndElement(),
            StoreCore("1"), EndElement(),
            StoreCore("1714"),
        ])
        
        # b group (child of bap)
        builder.set_context(2, "b$child1/2/2/1714/extra=x")
        builder.process_actions([
            NewGroup("b"),
            StoreCore("child1"), EndElement(),
            StoreCore("2"), EndElement(),
            StoreCore("2"), EndElement(),
            StoreCore("1714"), EndElement(),
            StoreCore("x"),
        ])
        
        # Another bap group (should cut path back to empty)
        builder.set_context(3, "bap$b2/2/2/1715")
        builder.process_actions([
            NewGroup("bap"),
            StoreCore("b2"), EndElement(),
            StoreCore("2"), EndElement(),
            StoreCore("2"), EndElement(),
            StoreCore("1715"),
        ])
        
        builder.close()
        
        # Third group (bap-2) should have empty path
        bap2 = builder.completed_groups[2]
        assert bap2.path == []
        assert bap2.id == "bap-2"


class TestIDGeneration:
    """Tests for ID generation."""
    
    @pytest.fixture
    def schema(self):
        """Create a schema registry with baptismos.yaml loaded."""
        registry = SchemaRegistry()
        registry.load(BAPTISMOS_YAML)
        return registry
    
    @pytest.fixture
    def errors(self):
        """Create an error accumulator."""
        return ErrorAccumulator()
    
    def test_id_generation_with_prefix(self, schema, errors):
        """Test ID generation using idprefix."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap$b1/1/1/1714")
        
        actions = [
            NewGroup("bap"),
            StoreCore("b1"), EndElement(),
            StoreCore("1"), EndElement(),
            StoreCore("1"), EndElement(),
            StoreCore("1714"),
        ]
        builder.process_actions(actions)
        builder.close()
        
        group = builder.completed_groups[0]
        assert group.id == "bap-1"  # idprefix is "bap"
    
    def test_id_generation_counter_increments(self, schema, errors):
        """Test that ID counter increments correctly."""
        builder = GroupBuilder(schema, errors)
        
        # First group
        builder.set_context(1, "bap$b1/1/1/1714")
        builder.process_actions([
            NewGroup("bap"),
            StoreCore("b1"), EndElement(),
            StoreCore("1"), EndElement(),
            StoreCore("1"), EndElement(),
            StoreCore("1714"),
        ])
        
        # Second group
        builder.set_context(2, "bap$b2/2/2/1715")
        builder.process_actions([
            NewGroup("bap"),
            StoreCore("b2"), EndElement(),
            StoreCore("2"), EndElement(),
            StoreCore("2"), EndElement(),
            StoreCore("1715"),
        ])
        
        builder.close()
        
        assert builder.completed_groups[0].id == "bap-1"
        assert builder.completed_groups[1].id == "bap-2"
    
    def test_subgroup_counter_reset(self, schema, errors):
        """Test that subgroup counters are reset when parent is encountered.
        
        Note: This test uses 'b' group as a simulated "subgroup" to test
        counter reset logic. In the baptismos schema, both 'bap' and 'b' 
        have idprefix='bap', so they share counter state.
        """
        builder = GroupBuilder(schema, errors)
        
        # bap-1
        builder.set_context(1, "bap$b1/1/1/1714")
        builder.process_actions([
            NewGroup("bap"),
            StoreCore("b1"), EndElement(),
            StoreCore("1"), EndElement(),
            StoreCore("1"), EndElement(),
            StoreCore("1714"),
        ])
        
        # b-1 (simulating a "subgroup" - same idprefix as bap)
        builder.set_context(2, "b$1/2/2/1714/extra=value")
        builder.process_actions([
            NewGroup("b"),
            StoreCore("1"), EndElement(),
            StoreCore("2"), EndElement(),
            StoreCore("2"), EndElement(),
            StoreCore("1714"), EndElement(),
            StoreCore("value"),
        ])
        
        # bap-2 (should reset b counter since bap contains b in schema)
        builder.set_context(3, "bap$b2/2/2/1715")
        builder.process_actions([
            NewGroup("bap"),
            StoreCore("b2"), EndElement(),
            StoreCore("2"), EndElement(),
            StoreCore("2"), EndElement(),
            StoreCore("1715"),
        ])
        
        # b-1 again (counter should have been reset by bap-2)
        builder.set_context(4, "b$2/3/3/1715/other=test")
        builder.process_actions([
            NewGroup("b"),
            StoreCore("2"), EndElement(),
            StoreCore("3"), EndElement(),
            StoreCore("3"), EndElement(),
            StoreCore("1715"), EndElement(),
            StoreCore("test"),
        ])
        
        builder.close()
        
        # Find b groups
        b_groups = [g for g in builder.completed_groups if g.name == "b"]
        assert len(b_groups) == 2
        assert b_groups[0].id == "bap-1"  # First b gets idprefix bap-1
        assert b_groups[1].id == "bap-1"  # Counter was reset by bap-2


class TestElementValidation:
    """Tests for element validation."""
    
    @pytest.fixture
    def schema(self):
        """Create a schema registry with baptismos.yaml loaded."""
        registry = SchemaRegistry()
        registry.load(BAPTISMOS_YAML)
        return registry
    
    @pytest.fixture
    def errors(self):
        """Create an error accumulator."""
        return ErrorAccumulator()
    
    def test_unknown_element_warning(self, schema, errors):
        """Test that unknown elements generate warnings."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap$b1/unknown=value")
        
        actions = [
            NewGroup("bap"),
            StoreCore("b1"), EndElement(),
            NewElement("unknown"),
            StoreCore("value"),
        ]
        builder.process_actions(actions)
        builder.close()
        
        # Should have a warning
        assert errors.warning_count >= 1
        assert any("unknown element" in str(w).lower() for w in errors.warnings)
    
    def test_missing_guaranteed_elements_error(self, schema, errors):
        """Test that missing guaranteed elements generate errors."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap$b1")  # Missing dia, mes, ano
        
        actions = [
            NewGroup("bap"),
            StoreCore("b1"),
        ]
        builder.process_actions(actions)
        builder.close()
        
        # Should have an error about missing elements
        assert errors.error_count >= 1
        assert any("missing element" in str(e).lower() for e in errors.errors)
    
    def test_valid_element_no_warning(self, schema, errors):
        """Test that valid elements don't generate warnings."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap$b1/loc=church")
        
        actions = [
            NewGroup("bap"),
            StoreCore("b1"), EndElement(),
            NewElement("loc"),
            StoreCore("church"),
        ]
        builder.process_actions(actions)
        builder.close()
        
        # Should not have warnings about loc
        assert not any("loc" in str(w) and "unknown" in str(w).lower() 
                      for w in errors.warnings)


class TestAspectHandling:
    """Tests for aspect handling (core/original/comment)."""
    
    @pytest.fixture
    def schema(self):
        """Create a schema registry with baptismos.yaml loaded."""
        registry = SchemaRegistry()
        registry.load(BAPTISMOS_YAML)
        return registry
    
    @pytest.fixture
    def errors(self):
        """Create an error accumulator."""
        return ErrorAccumulator()
    
    def test_original_aspect(self, schema, errors):
        """Test parsing original aspect values."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap$b1/Maria%/Marie")
        
        actions = [
            NewGroup("bap"),
            StoreCore("b1"), EndElement(),
            StoreCore("Maria"),
            NewAspect(Aspect.ORIGINAL),
            StoreCore("Marie"),
        ]
        builder.process_actions(actions)
        builder.close()
        
        group = builder.completed_groups[0]
        # Second positional element (dia) should have original aspect
        dia_el = group.elements[1]
        assert dia_el.name == "dia"
        assert dia_el.get_core_text() == "Maria"
        assert dia_el.get_original_text() == "Marie"
    
    def test_comment_aspect(self, schema, errors):
        """Test parsing comment aspect values."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap$b1/Maria#baptism record")
        
        actions = [
            NewGroup("bap"),
            StoreCore("b1"), EndElement(),
            StoreCore("Maria"),
            NewAspect(Aspect.COMMENT),
            StoreCore("baptism record"),
        ]
        builder.process_actions(actions)
        builder.close()
        
        group = builder.completed_groups[0]
        dia_el = group.elements[1]
        assert dia_el.name == "dia"
        assert dia_el.get_core_text() == "Maria"
        assert dia_el.get_comment_text() == "baptism record"
    
    def test_multiple_entries(self, schema, errors):
        """Test parsing multiple entries with semicolon."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap$b1/John;Paul")
        
        actions = [
            NewGroup("bap"),
            StoreCore("b1"), EndElement(),
            StoreCore("John"),
            NewEntry(),
            StoreCore("Paul"),
        ]
        builder.process_actions(actions)
        builder.close()
        
        group = builder.completed_groups[0]
        dia_el = group.elements[1]
        assert dia_el.name == "dia"
        assert len(dia_el.core_entries) == 2
        assert dia_el.core_entries[0].text == "John"
        assert dia_el.core_entries[1].text == "Paul"


class TestTranslateString:
    """Tests for the translate_string function."""
    
    @pytest.fixture
    def schema(self):
        """Create a schema registry with baptismos.yaml loaded."""
        registry = SchemaRegistry()
        registry.load(BAPTISMOS_YAML)
        return registry
    
    @pytest.fixture
    def errors(self):
        """Create an error accumulator."""
        return ErrorAccumulator()
    
    def test_translate_simple_string(self, schema, errors):
        """Test translating a simple Kleio string."""
        source = """bap$b1/1/1/1714
   b$child/2/2/1714/extra=x"""
        
        groups = translate_string(source, schema, errors)
        
        assert len(groups) == 2
        
        # First group: bap
        bap = groups[0]
        assert bap.name == "bap"
        assert bap.id == "bap-1"
        
        # Second group: b (child of bap per schema)
        b = groups[1]
        assert b.name == "b"
        assert b.path[0][0] == "bap"
    
    def test_translate_with_callback(self, schema, errors):
        """Test translate_string with callback."""
        source = "bap$b1/1/1/1714"
        
        completed = []
        def on_group(group):
            completed.append(group.name)
        
        groups = translate_string(source, schema, errors, on_group=on_group)
        
        assert len(completed) == 1
        assert completed[0] == "bap"
        assert len(groups) == 1
    
    def test_translate_empty_string(self, schema, errors):
        """Test translating an empty string."""
        source = ""
        
        groups = translate_string(source, schema, errors)
        
        assert len(groups) == 0
    
    def test_translate_with_blank_lines(self, schema, errors):
        """Test translating string with blank lines."""
        source = """
bap$b1/1/1/1714

   b$child/2/2/1714/extra=x

"""
        
        groups = translate_string(source, schema, errors)
        
        assert len(groups) == 2


class TestTranslateFile:
    """Tests for the translate_file function."""
    
    @pytest.fixture
    def schema(self):
        """Create a schema registry with baptismos.yaml loaded."""
        registry = SchemaRegistry()
        registry.load(BAPTISMOS_YAML)
        return registry
    
    @pytest.fixture
    def errors(self):
        """Create an error accumulator."""
        return ErrorAccumulator()
    
    def test_translate_nonexistent_file(self, schema, errors):
        """Test that FileNotFoundError is raised for missing file."""
        with pytest.raises(FileNotFoundError):
            translate_file("/nonexistent/file.cli", schema, errors)


class TestPositionalElements:
    """Tests for positional element resolution."""
    
    @pytest.fixture
    def schema(self):
        """Create a schema registry with baptismos.yaml loaded."""
        registry = SchemaRegistry()
        registry.load(BAPTISMOS_YAML)
        return registry
    
    @pytest.fixture
    def errors(self):
        """Create an error accumulator."""
        return ErrorAccumulator()
    
    def test_positional_elements_from_locus(self, schema, errors):
        """Test that positional elements are resolved from position list."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap$b1/1/1/1714/loc=church")
        
        # bap position: [id, dia, mes, ano, fol, loc, celebrante]
        actions = [
            NewGroup("bap"),
            StoreCore("b1"), EndElement(),  # id
            StoreCore("1"), EndElement(),   # dia
            StoreCore("1"), EndElement(),   # mes
            StoreCore("1714"), EndElement(), # ano
            NewElement("loc"),
            StoreCore("church"),
        ]
        builder.process_actions(actions)
        builder.close()
        
        group = builder.completed_groups[0]
        
        # Check positional elements
        assert group.elements[0].name == "id"
        assert group.elements[1].name == "dia"
        assert group.elements[2].name == "mes"
        assert group.elements[3].name == "ano"
        
        # Check explicit element
        loc_el = group.get_element("loc")
        assert loc_el is not None
        assert loc_el.get_core_text() == "church"


class TestEdgeCases:
    """Tests for edge cases."""
    
    @pytest.fixture
    def schema(self):
        """Create a schema registry with baptismos.yaml loaded."""
        registry = SchemaRegistry()
        registry.load(BAPTISMOS_YAML)
        return registry
    
    @pytest.fixture
    def errors(self):
        """Create an error accumulator."""
        return ErrorAccumulator()
    
    def test_close_without_any_groups(self, schema, errors):
        """Test closing builder without processing any groups."""
        builder = GroupBuilder(schema, errors)
        builder.close()
        
        assert len(builder.completed_groups) == 0
    
    def test_multiple_close_calls(self, schema, errors):
        """Test that multiple close calls don't cause issues."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap$b1/1/1/1714")
        
        builder.process_actions([
            NewGroup("bap"),
            StoreCore("b1"), EndElement(),
            StoreCore("1"), EndElement(),
            StoreCore("1"), EndElement(),
            StoreCore("1714"),
        ])
        
        builder.close()
        builder.close()  # Second close should be safe
        
        assert len(builder.completed_groups) == 1
    
    def test_group_without_elements(self, schema, errors):
        """Test group with no elements."""
        builder = GroupBuilder(schema, errors)
        builder.set_context(1, "bap")
        
        actions = [
            NewGroup("bap"),
        ]
        builder.process_actions(actions)
        builder.close()
        
        # Should still create a group (though with errors for missing guaranteed)
        assert len(builder.completed_groups) == 1
        assert builder.completed_groups[0].name == "bap"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
