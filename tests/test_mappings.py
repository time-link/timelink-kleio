"""Tests for the database mapping layer and XML element synthesis.

Covers:
- default-mappings.yaml loading and inheritance-aware group→class resolution.
- <CLASS> block emission in the XML exporter.
- Synthesized elements (date, type, sex) on groups.
"""
from __future__ import annotations

import tempfile
from pathlib import Path
from xml.etree import ElementTree as ET

import pytest

from kleio.mappings import MappingStore, get_default_mapping_store, get_default_mappings_path
from kleio.schema.registry import SchemaRegistry
from kleio.errors import ErrorAccumulator
from kleio.export.xml_exporter import XmlExporter
from kleio.parser.builder import translate_string

GACTO2 = Path("tests/kleio-home/structures/gacto2.str.yaml")


# ---------------------------------------------------------------------------
# MappingStore: loading and resolution
# ---------------------------------------------------------------------------

class TestMappingStore:
    """Tests for the mapping YAML loading and class resolution."""

    def test_default_mappings_load(self):
        """default-mappings.yaml loads with the expected number of entries."""
        path = get_default_mappings_path()
        assert path.exists(), f"default-mappings.yaml not found at {path}"
        store = get_default_mapping_store()
        assert len(store._class_mappings) >= 30, "expected at least 30 mappings"
        assert len(store._class_definitions) >= 30, "expected at least 30 class defs"

    def test_direct_mappings(self):
        """Key builtin mappings are present."""
        store = get_default_mapping_store()
        assert store.get_class_for_group('historical-source') == 'source'
        assert store.get_class_for_group('historical-act') == 'act'
        assert store.get_class_for_group('person') == 'person'
        assert store.get_class_for_group('relation') == 'relation'
        assert store.get_class_for_group('attribute') == 'attribute'
        assert store.get_class_for_group('amz') == 'acta'
        assert store.get_class_for_group('fogo') == 'household'

    def test_class_definition_attributes(self):
        """The 'source' class definition has the expected attributes."""
        store = get_default_mapping_store()
        source_def = store.get_class_definition('source')
        assert source_def is not None
        assert source_def['super'] == 'entity'
        assert source_def['table'] == 'sources'
        attrs = source_def['attributes']
        assert len(attrs) == 9
        # Spot-check the date attribute.
        date_attr = next(a for a in attrs if a['name'] == 'date')
        assert date_attr['column'] == 'the_date'
        assert date_attr['baseclass'] == 'date'

    def test_resolve_class_inheritance(self):
        """Inheritance-aware resolution: unmapped groups resolve via their
        source chain (e.g. bap → pt-acto → historical-act → act)."""
        store = get_default_mapping_store()
        if not GACTO2.exists():
            pytest.skip("gacto2.str.yaml not found")
        schema = SchemaRegistry()
        schema.load(GACTO2, ErrorAccumulator())

        # bap is not directly mapped but extends historical-act → act.
        assert store.resolve_class_for_group('bap', schema) == 'act'
        assert store.resolve_class_for_group('cas', schema) == 'act'
        assert store.resolve_class_for_group('rol', schema) == 'act'
        assert store.resolve_class_for_group('n', schema) == 'person'
        assert store.resolve_class_for_group('na', schema) == 'person'
        assert store.resolve_class_for_group('pai', schema) == 'person'
        assert store.resolve_class_for_group('lista', schema) == 'act'
        assert store.resolve_class_for_group('fonte', schema) == 'source'

    def test_resolve_class_no_schema(self):
        """Without a schema, only direct mappings resolve."""
        store = get_default_mapping_store()
        assert store.resolve_class_for_group('person') == 'person'
        assert store.resolve_class_for_group('bap') is None  # not directly mapped


# ---------------------------------------------------------------------------
# XML exporter: <CLASS> blocks, class resolution, element synthesis
# ---------------------------------------------------------------------------

class TestXmlExporterMappings:
    """Tests for mapping-aware XML export."""

    @pytest.fixture
    def temp_dir(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            yield Path(tmpdir)

    @pytest.fixture
    def schema(self):
        if not GACTO2.exists():
            pytest.skip("gacto2.str.yaml not found")
        s = SchemaRegistry()
        s.load(GACTO2, ErrorAccumulator())
        return s

    @pytest.fixture
    def store(self):
        return get_default_mapping_store()

    def _export(self, schema, store, src, temp_dir, name="t"):
        """Translate and export to XML; return the XML root element."""
        errors = ErrorAccumulator()
        groups = translate_string(src, schema, errors)
        exporter = XmlExporter()
        exporter.init(source_file=f"{name}.cli", output_dir=temp_dir,
                      schema=schema, structure_file="gacto2.str",
                      mapping_store=store)
        for g in groups:
            exporter.export_group(g)
        exporter.close()
        return ET.parse(temp_dir / f"{name}.xml").getroot()

    def test_group_class_mapped(self, schema, store, temp_dir):
        """Groups get the mapped database class, not the schema base class."""
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1714\n"
            "         n$joao/m/id=c1\n"
        )
        root = self._export(schema, store, src, temp_dir)
        groups = {g.get('NAME'): g for g in root.iter('GROUP')}
        assert groups['fonte'].get('CLASS') == 'source'
        assert groups['bap'].get('CLASS') == 'act'
        assert groups['n'].get('CLASS') == 'person'

    def test_class_blocks_emitted(self, schema, store, temp_dir):
        """<CLASS> blocks are emitted before <GROUP> elements."""
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1714\n"
            "         n$joao/m/id=c1\n"
        )
        root = self._export(schema, store, src, temp_dir)
        class_blocks = root.findall('CLASS')
        assert len(class_blocks) >= 3  # source, act, person

        class_names = [c.get('NAME') for c in class_blocks]
        assert 'source' in class_names
        assert 'act' in class_names
        assert 'person' in class_names
        # entity is suppressed (builtin).
        assert 'entity' not in class_names

        # Check the source class block has correct attributes.
        source_cls = next(c for c in class_blocks if c.get('NAME') == 'source')
        assert source_cls.get('SUPER') == 'entity'
        assert source_cls.get('TABLE') == 'sources'
        assert source_cls.get('GROUP') == 'fonte'
        attrs = source_cls.findall('ATTRIBUTE')
        assert len(attrs) == 9

    def test_date_synthesized_from_dmy(self, schema, store, temp_dir):
        """An act group with dia/mes/ano gets a synthesized date element."""
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1/4/2/1714\n"
            "         n$joao/m/id=c1\n"
        )
        root = self._export(schema, store, src, temp_dir)
        bap = next(g for g in root.iter('GROUP') if g.get('NAME') == 'bap')
        date_el = None
        for el in bap.findall('ELEMENT'):
            if el.get('NAME') == 'date':
                core = el.find('CORE')
                if core is None:
                    core = el.find('core')
                date_el = core.text if core is not None else ""
        assert date_el == '17140204'  # YYYYMMDD

    def test_type_synthesized(self, schema, store, temp_dir):
        """An act group gets a synthesized type element (the group name)."""
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1714\n"
            "         n$joao/m/id=c1\n"
        )
        root = self._export(schema, store, src, temp_dir)
        bap = next(g for g in root.iter('GROUP') if g.get('NAME') == 'bap')
        type_el = None
        for el in bap.findall('ELEMENT'):
            if el.get('NAME') == 'type':
                core = el.find('CORE')
                if core is None:
                    core = el.find('core')
                type_el = core.text if core is not None else ""
        assert type_el == 'bap'  # the group name

    def test_sex_synthesized_male(self, schema, store, temp_dir):
        """A male person (descended from actorm/male) gets sex=m."""
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1714\n"
            "         n$joao/m/id=c1\n"
        )
        root = self._export(schema, store, src, temp_dir)
        person = next(g for g in root.iter('GROUP') if g.get('NAME') == 'n')
        sex_el = None
        for el in person.findall('ELEMENT'):
            if el.get('NAME') == 'sex':
                core = el.find('CORE')
                if core is None:
                    core = el.find('core')
                sex_el = core.text if core is not None else ""
        assert sex_el == 'm'

    def test_sex_synthesized_female(self, schema, store, temp_dir):
        """A female person (descended from actorf/female) gets sex=f."""
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1714\n"
            "         na$maria/id=c1\n"
        )
        root = self._export(schema, store, src, temp_dir)
        person = next(g for g in root.iter('GROUP') if g.get('NAME') == 'na')
        sex_el = None
        for el in person.findall('ELEMENT'):
            if el.get('NAME') == 'sex':
                core = el.find('CORE')
                if core is None:
                    core = el.find('core')
                sex_el = core.text if core is not None else ""
        assert sex_el == 'f'

    def test_relation_class_block_with_function_in_act(self, schema, store, temp_dir):
        """When function-in-act relations are generated, the relation
        <CLASS> block is also emitted."""
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1714\n"
            "         n$joao/m/id=c1\n"
        )
        root = self._export(schema, store, src, temp_dir)
        class_names = [c.get('NAME') for c in root.findall('CLASS')]
        assert 'relation' in class_names
