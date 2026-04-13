"""Tests for XML and JSON exporters."""
from __future__ import annotations

import json
import tempfile
from pathlib import Path
from xml.etree import ElementTree as ET

import pytest

from kleio.export import XmlExporter, JsonExporter, Exporter
from kleio.parser.models import ParsedGroup, ParsedElement, Entry
from kleio.schema.registry import SchemaRegistry
from kleio.inference.models import InferenceResults, GeneratedRelation, GeneratedAttribute


class TestXmlExporter:
    """Tests for XmlExporter."""
    
    @pytest.fixture
    def temp_dir(self):
        """Create a temporary directory for test output."""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir)
            
    @pytest.fixture
    def schema(self):
        """Create a mock schema registry."""
        # For testing, we use a minimal schema
        return SchemaRegistry()
        
    @pytest.fixture
    def simple_group(self):
        """Create a simple parsed group for testing."""
        return ParsedGroup(
            name="fonte",
            id="fonte-1",
            elements=[
                ParsedElement(
                    name="id",
                    core_entries=[Entry(values=["test-source"])]
                ),
                ParsedElement(
                    name="tipo",
                    core_entries=[Entry(values=["baptismos"])]
                )
            ],
            path=[],
            line_number=2,
            line_text="fonte$test-source/tipo=baptismos",
            level=1
        )
        
    @pytest.fixture
    def nested_groups(self):
        """Create nested parsed groups for testing hierarchy."""
        # Parent group
        parent = ParsedGroup(
            name="fonte",
            id="fonte-1",
            elements=[
                ParsedElement(
                    name="id",
                    core_entries=[Entry(values=["test-source"])]
                )
            ],
            path=[],
            line_number=2,
            line_text="fonte$test-source",
            level=1
        )
        
        # Child group (act)
        child = ParsedGroup(
            name="bap",
            id="bap-1",
            elements=[
                ParsedElement(
                    name="id",
                    core_entries=[Entry(values=["b1"])]
                ),
                ParsedElement(
                    name="dia",
                    core_entries=[Entry(values=["1"])]
                )
            ],
            path=[("fonte", "fonte-1")],
            line_number=4,
            line_text="bap$b1/dia=1",
            level=2
        )
        
        # Grandchild group (person)
        grandchild = ParsedGroup(
            name="n",
            id="n-1",
            elements=[
                ParsedElement(
                    name="nome",
                    core_entries=[Entry(values=["maria"])]
                ),
                ParsedElement(
                    name="sexo",
                    core_entries=[Entry(values=["f"])]
                )
            ],
            path=[("fonte", "fonte-1"), ("bap", "bap-1")],
            line_number=6,
            line_text="n$maria/sexo=f",
            level=3
        )
        
        return [parent, child, grandchild]
        
    @pytest.fixture
    def group_with_aspects(self):
        """Create a group with all three aspects (core, original, comment)."""
        return ParsedGroup(
            name="n",
            id="n-1",
            elements=[
                ParsedElement(
                    name="nome",
                    core_entries=[Entry(values=["John"])],
                    original_entries=[Entry(values=["Johann"])],
                    comment_entries=[Entry(values=["written as Johann in original"])]
                )
            ],
            path=[],
            line_number=10,
            line_text="n$John%/Johann/#written as Johann in original",
            level=1
        )
        
    def test_exporter_is_abstract_base(self):
        """Test that Exporter is an abstract base class."""
        assert hasattr(Exporter, 'init')
        assert hasattr(Exporter, 'export_group')
        assert hasattr(Exporter, 'close')
        
    def test_xml_exporter_init(self, temp_dir, schema):
        """Test XmlExporter initialization."""
        exporter = XmlExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema,
            structure_file="gacto2.str",
            translator="test-translator",
            obs="test observation"
        )
        
        assert exporter._source_file == "/path/to/test.cli"
        assert exporter._structure_file == "gacto2.str"
        assert exporter._translator_name == "test-translator"
        assert exporter._obs == "test observation"
        assert exporter._output_path == temp_dir / "test.xml"
        assert exporter._files_json_path == temp_dir / "test.files.json"
        
    def test_xml_exporter_simple_group(self, temp_dir, schema, simple_group):
        """Test exporting a simple group."""
        exporter = XmlExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema
        )
        
        exporter.export_group(simple_group)
        output_files = exporter.close()
        
        # Check output files were created
        assert len(output_files) == 2
        assert str(temp_dir / "test.xml") in output_files
        assert str(temp_dir / "test.files.json") in output_files
        
        # Parse and verify XML
        tree = ET.parse(temp_dir / "test.xml")
        root = tree.getroot()
        
        # Check root element
        assert root.tag == "KLEIO"
        assert root.get("STRUCTURE") == schema.name
        assert root.get("SOURCE") == "/path/to/test.cli"
        assert "TRANSLATOR" in root.attrib
        assert "WHEN" in root.attrib
        
        # Check group element
        groups = root.findall("GROUP")
        assert len(groups) == 1
        
        group = groups[0]
        assert group.get("ID") == "fonte-1"
        assert group.get("NAME") == "fonte"
        assert group.get("CLASS") == "fonte"  # No schema, so uses group name
        assert group.get("LEVEL") == "1"
        assert group.get("LINE") == "2"
        
        # Check elements
        elements = group.findall("ELEMENT")
        element_names = {e.get("NAME") for e in elements}
        assert "id" in element_names
        assert "tipo" in element_names
        
    def test_xml_exporter_nested_groups(self, temp_dir, schema, nested_groups):
        """Test exporting nested groups."""
        exporter = XmlExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema
        )
        
        for group in nested_groups:
            exporter.export_group(group)
        output_files = exporter.close()
        
        # Parse and verify XML
        tree = ET.parse(temp_dir / "test.xml")
        root = tree.getroot()
        
        # Check all groups are present (at any nesting level)
        all_groups = root.findall(".//GROUP")
        assert len(all_groups) == 3
        
        # Check root level has only the parent group
        root_groups = root.findall("GROUP")
        assert len(root_groups) == 1
        
        # Check parent group
        parent = root_groups[0]
        assert parent.get("NAME") == "fonte"
        assert parent.get("LEVEL") == "1"
        
        # Check child is nested inside parent
        children = parent.findall("GROUP")
        assert len(children) == 1
        child = children[0]
        assert child.get("NAME") == "bap"
        assert child.get("LEVEL") == "2"
        
        # Check grandchild is nested inside child
        grandchildren = child.findall("GROUP")
        assert len(grandchildren) == 1
        grandchild = grandchildren[0]
        assert grandchild.get("NAME") == "n"
        assert grandchild.get("LEVEL") == "3"
        
    def test_xml_exporter_aspects(self, temp_dir, schema, group_with_aspects):
        """Test exporting group with core, original, and comment aspects."""
        exporter = XmlExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema
        )
        
        exporter.export_group(group_with_aspects)
        exporter.close()
        
        # Parse and verify XML
        tree = ET.parse(temp_dir / "test.xml")
        root = tree.getroot()
        
        group = root.find("GROUP")
        elements = group.findall("ELEMENT")
        
        # Find the nome element
        nome_elem = None
        for elem in elements:
            if elem.get("NAME") == "nome":
                nome_elem = elem
                break
                
        assert nome_elem is not None
        
        # Check all three aspects
        core = nome_elem.find("CORE")
        original = nome_elem.find("ORIGINAL")
        comment = nome_elem.find("COMMENT")
        
        assert core is not None
        assert core.text == "John"
        
        assert original is not None
        assert original.text == "Johann"
        
        assert comment is not None
        assert comment.text == "written as Johann in original"
        
    def test_xml_exporter_with_inference_results(self, temp_dir, schema, simple_group):
        """Test exporting with inference results."""
        exporter = XmlExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema
        )
        
        exporter.export_group(simple_group)
        
        # Create inference results
        inference = InferenceResults()
        inference.add_relation(GeneratedRelation(
            rel_type="parentesco",
            value="pai",
            origin_id="per-1",
            dest_id="per-2",
            source_rule="test-rule"
        ))
        inference.add_attribute(GeneratedAttribute(
            entity_id="per-1",
            attr_type="ec",
            attr_value="c",
            source_rule="test-rule"
        ))
        
        output_files = exporter.close(inference)
        
        # Parse and verify XML
        tree = ET.parse(temp_dir / "test.xml")
        root = tree.getroot()
        
        # Check relation
        relations = root.findall("RELATION")
        assert len(relations) == 1
        rel = relations[0]
        assert rel.get("TYPE") == "parentesco"
        assert rel.get("VALUE") == "pai"
        assert rel.get("ORIGIN") == "per-1"
        assert rel.get("DESTINATION") == "per-2"
        
        # Check attribute
        attributes = root.findall("ATTRIBUTE")
        assert len(attributes) == 1
        attr = attributes[0]
        assert attr.get("ID") == "per-1"
        assert attr.get("TYPE") == "ec"
        assert attr.get("VALUE") == "c"
        
    def test_xml_exporter_files_json(self, temp_dir, schema, simple_group):
        """Test that .files.json metadata is created correctly."""
        exporter = XmlExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema,
            structure_file="gacto2.str"
        )
        
        exporter.export_group(simple_group)
        exporter.close()
        
        # Read and verify .files.json
        with open(temp_dir / "test.files.json") as f:
            files_info = json.load(f)
            
        assert files_info['stru'] == "gacto2.str"
        assert files_info['source'] == "/path/to/test.cli"
        assert 'xml' in files_info
        assert 'timestamp' in files_info
        assert files_info['groups_processed'] == 1
        
    def test_xml_exporter_escaping(self, temp_dir, schema):
        """Test that special XML characters are properly escaped."""
        group = ParsedGroup(
            name="n",
            id="n-1",
            elements=[
                ParsedElement(
                    name="nome",
                    core_entries=[Entry(values=["John & Mary <test>"])]
                )
            ],
            path=[],
            line_number=1,
            line_text="n$John & Mary <test>",
            level=1
        )
        
        exporter = XmlExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema
        )
        
        exporter.export_group(group)
        exporter.close()
        
        # Parse and verify XML - this should not raise
        tree = ET.parse(temp_dir / "test.xml")
        root = tree.getroot()
        
        # Verify the content was preserved
        group = root.find("GROUP")
        elements = group.findall("ELEMENT")
        nome_elem = [e for e in elements if e.get("NAME") == "nome"][0]
        core = nome_elem.find("CORE")
        assert core.text == "John & Mary <test>"


class TestJsonExporter:
    """Tests for JsonExporter."""
    
    @pytest.fixture
    def temp_dir(self):
        """Create a temporary directory for test output."""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir)
            
    @pytest.fixture
    def schema(self):
        """Create a mock schema registry."""
        return SchemaRegistry()
        
    @pytest.fixture
    def simple_group(self):
        """Create a simple parsed group for testing."""
        return ParsedGroup(
            name="fonte",
            id="fonte-1",
            elements=[
                ParsedElement(
                    name="id",
                    core_entries=[Entry(values=["test-source"])]
                ),
                ParsedElement(
                    name="tipo",
                    core_entries=[Entry(values=["baptismos"])]
                )
            ],
            path=[],
            line_number=2,
            line_text="fonte$test-source/tipo=baptismos",
            level=1
        )
        
    @pytest.fixture
    def nested_groups(self):
        """Create nested parsed groups for testing hierarchy."""
        parent = ParsedGroup(
            name="fonte",
            id="fonte-1",
            elements=[
                ParsedElement(
                    name="id",
                    core_entries=[Entry(values=["test-source"])]
                )
            ],
            path=[],
            line_number=2,
            line_text="fonte$test-source",
            level=1
        )
        
        child = ParsedGroup(
            name="bap",
            id="bap-1",
            elements=[
                ParsedElement(
                    name="id",
                    core_entries=[Entry(values=["b1"])]
                )
            ],
            path=[("fonte", "fonte-1")],
            line_number=4,
            line_text="bap$b1",
            level=2
        )
        
        return [parent, child]
        
    def test_json_exporter_init(self, temp_dir, schema):
        """Test JsonExporter initialization."""
        exporter = JsonExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema,
            structure_file="gacto2.str",
            translator="test-translator",
            obs="test observation"
        )
        
        assert exporter._source_file == "/path/to/test.cli"
        assert exporter._structure_file == "gacto2.str"
        assert exporter._translator_name == "test-translator"
        assert exporter._obs == "test observation"
        assert exporter._output_path == temp_dir / "test.json"
        
    def test_json_exporter_simple_group(self, temp_dir, schema, simple_group):
        """Test exporting a simple group to JSON."""
        exporter = JsonExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema
        )
        
        exporter.export_group(simple_group)
        output_files = exporter.close()
        
        # Check output files were created
        assert len(output_files) == 2
        assert str(temp_dir / "test.json") in output_files
        
        # Read and verify JSON
        with open(temp_dir / "test.json") as f:
            data = json.load(f)
            
        # Check structure
        assert data['structure'] == schema.name
        assert data['source'] == "/path/to/test.cli"
        assert 'timestamp' in data
        assert 'groups' in data
        assert 'relations' in data
        assert 'attributes' in data
        
        # Check group
        assert len(data['groups']) == 1
        group = data['groups'][0]
        assert group['id'] == "fonte-1"
        assert group['name'] == "fonte"
        assert group['class'] == "fonte"
        assert group['level'] == 1
        assert group['line'] == 2
        
        # Check elements
        assert len(group['elements']) == 2
        element_names = {e['name'] for e in group['elements']}
        assert "id" in element_names
        assert "tipo" in element_names
        
    def test_json_exporter_nested_groups(self, temp_dir, schema, nested_groups):
        """Test exporting nested groups to JSON."""
        exporter = JsonExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema
        )
        
        for group in nested_groups:
            exporter.export_group(group)
        output_files = exporter.close()
        
        # Read and verify JSON
        with open(temp_dir / "test.json") as f:
            data = json.load(f)
            
        # Check hierarchy
        assert len(data['groups']) == 1
        parent = data['groups'][0]
        assert parent['name'] == "fonte"
        assert parent['level'] == 1
        
        # Check child is nested
        assert len(parent['groups']) == 1
        child = parent['groups'][0]
        assert child['name'] == "bap"
        assert child['level'] == 2
        
    def test_json_exporter_with_inference_results(self, temp_dir, schema, simple_group):
        """Test exporting with inference results to JSON."""
        exporter = JsonExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema
        )
        
        exporter.export_group(simple_group)
        
        # Create inference results
        inference = InferenceResults()
        inference.add_relation(GeneratedRelation(
            rel_type="parentesco",
            value="pai",
            origin_id="per-1",
            dest_id="per-2",
            source_rule="test-rule"
        ))
        inference.add_attribute(GeneratedAttribute(
            entity_id="per-1",
            attr_type="ec",
            attr_value="c",
            source_rule="test-rule"
        ))
        
        exporter.close(inference)
        
        # Read and verify JSON
        with open(temp_dir / "test.json") as f:
            data = json.load(f)
            
        # Check relations
        assert len(data['relations']) == 1
        rel = data['relations'][0]
        assert rel['rel_type'] == "parentesco"
        assert rel['value'] == "pai"
        assert rel['origin_id'] == "per-1"
        assert rel['dest_id'] == "per-2"
        
        # Check attributes
        assert len(data['attributes']) == 1
        attr = data['attributes'][0]
        assert attr['entity_id'] == "per-1"
        assert attr['attr_type'] == "ec"
        assert attr['attr_value'] == "c"
        
    def test_json_exporter_aspects(self, temp_dir, schema):
        """Test exporting group with all aspects to JSON."""
        group = ParsedGroup(
            name="n",
            id="n-1",
            elements=[
                ParsedElement(
                    name="nome",
                    core_entries=[Entry(values=["John"])],
                    original_entries=[Entry(values=["Johann"])],
                    comment_entries=[Entry(values=["comment"])]
                )
            ],
            path=[],
            line_number=1,
            line_text="n$John%/Johann/#comment",
            level=1
        )
        
        exporter = JsonExporter()
        exporter.init(
            source_file="/path/to/test.cli",
            output_dir=temp_dir,
            schema=schema
        )
        
        exporter.export_group(group)
        exporter.close()
        
        # Read and verify JSON
        with open(temp_dir / "test.json") as f:
            data = json.load(f)
            
        element = data['groups'][0]['elements'][0]
        assert element['core'] == "John"
        assert element['original'] == "Johann"
        assert element['comment'] == "comment"
