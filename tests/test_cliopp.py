"""Tests for the .ids pretty-printer (clioPP.pl port) and rename-on-success.

Covers:
- The kleio ``prefix=`` element being applied to every id during parsing
  and stripped back off when pretty-printing the .ids file.
- The .ids formatting rules (indentation, blank lines, positional then
  named elements, aspects, the trailing /id=..., the kleio /translations).
- The rename-on-success logic (.ids -> .cli with .org/.old backups).
- The .ids landing in the same directory as the source .cli.
- Round-trip id stability (the core purpose: re-translating the promoted
  .cli yields identical ids).
"""
from __future__ import annotations

from pathlib import Path

import pytest

from kleio.schema.registry import SchemaRegistry
from kleio.parser.builder import translate_string, translate_file
from kleio.errors import ErrorAccumulator
from kleio.export.cliopp import ClioPrettyPrinter
from kleio.export.rename import promote_ids_on_success


GACTO2 = Path("tests/kleio-home/structures/gacto2.str.yaml")
INFERENCE_TEST = Path(
    "tests/kleio-home/sources/reference_sources/inference/inference-test.cli"
)


@pytest.fixture
def schema():
    if not GACTO2.exists():
        pytest.skip("gacto2.str.yaml not found")
    s = SchemaRegistry()
    errors = ErrorAccumulator()
    s.load(GACTO2, errors)
    return s


@pytest.fixture
def errors():
    return ErrorAccumulator()


# ---------------------------------------------------------------------------
# Builder: kleio prefix handling
# ---------------------------------------------------------------------------


class TestBuilderPrefix:
    """The kleio ``prefix=`` element namespaces every generated/explicit id."""

    def test_builder_applies_kleio_prefix(self, schema, errors):
        """prefix=lousa -> every id (explicit and auto) becomes lousa-..."""
        src = (
            "kleio$gacto2.str/prefix=lousa\n"
            "   fonte$test\n"
            "      bap$b1714-1/6/1/1714\n"
            "         n$filipa/f/id=b1714-1-per1\n"
        )
        groups = translate_string(src, schema, errors)
        ids = {g.name: g.id for g in groups}
        # Explicit ids get prefixed.
        assert ids["fonte"] == "lousa-test"
        assert ids["bap"] == "lousa-b1714-1"
        assert ids["n"] == "lousa-b1714-1-per1"
        # kleio itself is never prefixed.
        assert ids["kleio"] == "lousa"  # coincidentally equals the prefix value

    def test_builder_no_prefix_default(self, schema, errors):
        """Without prefix=, ids are unchanged (regression guard)."""
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1714-1/6/1/1714\n"
        )
        groups = translate_string(src, schema, errors)
        ids = {g.name: g.id for g in groups}
        assert ids["fonte"] == "test"
        assert ids["bap"] == "b1714-1"

    def test_builder_prefix_no_double_application(self, schema, errors):
        """Hierarchical child ids get the prefix exactly once, not twice."""
        src = (
            "kleio$gacto2.str/prefix=lousa\n"
            "   fonte$test\n"
            "      bap$b1714-1/6/1/1714\n"
            "         n$filipa/f/id=b1714-1-per1\n"
            "            pn$antonio\n"
        )
        groups = translate_string(src, schema, errors)
        pn = next(g for g in groups if g.name == "pn")
        # Single prefix, not lousa-lousa-...
        assert pn.id == "lousa-b1714-1-per1-per1"
        assert not pn.id.startswith("lousa-lousa")


class TestTranslationCount:
    """The translation-count suffix (gactoxml.pl:442-444, 1482-1484).

    The ``translations=N`` element on the ``kleio$`` group is incremented by 1
    unconditionally on every translation pass. When the (incremented) count is
    > 1, it is appended to **auto-generated ids only** (never to explicit ids).
    This ensures groups inserted after the first pass get fresh ids and don't
    collide with previously-assigned ones.
    """

    def test_first_pass_no_suffix(self, schema, errors):
        """First translation (no translations= element): count becomes 1,
        auto-ids get NO suffix (gactoxml.pl:1482 TransCount > 1 guard)."""
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1714\n"
            "         n$joao/m\n"
        )
        groups = translate_string(src, schema, errors)
        n = next(g for g in groups if g.name == "n")
        # Auto-id, no translation-count suffix.
        assert "-2" not in n.id and "-1" not in n.id.split("-")[-1]
        assert n.id == "b1-per1"

    def test_second_pass_appends_suffix(self, schema, errors):
        """translations=1 -> count becomes 2 -> auto-ids get '-2' suffix."""
        src = (
            "kleio$gacto2.str/translations=1\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1714\n"
            "         n$joao/m\n"
        )
        groups = translate_string(src, schema, errors)
        n = next(g for g in groups if g.name == "n")
        assert n.id == "b1-per1-2"

    def test_explicit_id_never_suffixed(self, schema, errors):
        """Explicit ids (identification=sic) do NOT get the translation-count
        suffix, even when the count > 1 (gactoxml.pl:1464-1472 bypass)."""
        src = (
            "kleio$gacto2.str/translations=5\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1714\n"
        )
        groups = translate_string(src, schema, errors)
        bap = next(g for g in groups if g.name == "bap")
        # b1 is explicit (id element, identification=sic); no -6 suffix.
        assert bap.id == "b1"

    def test_pretty_printer_increments_count(self, schema, tmp_path):
        """The .ids output writes back the incremented translations count
        so the next pass sees N+1 (clioPP.pl:123-128)."""
        src = (
            "kleio$gacto2.str/translations=5\n"
            "   fonte$test\n"
        )
        errors = ErrorAccumulator()
        groups = translate_string(src, schema, errors)
        pp = ClioPrettyPrinter("t.cli", tmp_path, schema)
        for g in groups:
            pp.on_group(g)
        pp.close()
        text = (tmp_path / "t.ids").read_text()
        assert "/translations=6" in text

    def test_pretty_printer_first_pass_sets_count_to_1(self, schema, tmp_path):
        """A source with no translations= element gets /translations=1 on the
        first pretty-printed output."""
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
        )
        errors = ErrorAccumulator()
        groups = translate_string(src, schema, errors)
        pp = ClioPrettyPrinter("t.cli", tmp_path, schema)
        for g in groups:
            pp.on_group(g)
        pp.close()
        text = (tmp_path / "t.ids").read_text()
        assert "/translations=1" in text


# ---------------------------------------------------------------------------
# Pretty-printer: formatting rules
# ---------------------------------------------------------------------------


class TestClioPrettyPrinter:
    """The .ids output format (ports clioPP.pl)."""

    def _pp(self, schema, src, tmp_path, name="t"):
        errors = ErrorAccumulator()
        groups = translate_string(src, schema, errors)
        pp = ClioPrettyPrinter(f"{name}.cli", tmp_path, schema)
        for g in groups:
            pp.on_group(g)
        paths = pp.close()
        text = Path(paths[0]).read_text()
        return groups, text

    def test_pretty_print_basic_structure(self, schema, tmp_path):
        """Indentation is 3 spaces per level; blank line before acts/persons
        but not before ls$."""
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1800\n"
            "         n$joao/m/id=c1\n"
            "            pai$pedro/id=f1\n"
            "               ls$morada/vila\n"
        )
        _, text = self._pp(schema, src, tmp_path)
        lines = text.splitlines()
        # fonte is at depth 1, bap at depth 2, n at depth 3, pai at depth 4.
        assert any(ln.startswith("   fonte$") for ln in lines)
        assert any(ln.startswith("      bap$") for ln in lines)
        assert any(ln.startswith("         n$") for ln in lines)
        assert any(ln.startswith("            pai$") for ln in lines)
        # ls$ has no blank line before it (attribute class).
        ls_idx = next(i for i, ln in enumerate(lines) if ln.startswith("               ls$"))
        # The previous line is the pai$ line, not a blank.
        assert lines[ls_idx - 1].strip().startswith("pai$")

    def test_pretty_print_strips_prefix(self, schema, tmp_path):
        """With prefix=lousa, the .ids shows unprefixed ids; the kleio
        header retains /prefix=lousa."""
        src = (
            "kleio$gacto2.str/prefix=lousa\n"
            "   fonte$test\n"
            "      bap$b1714-1/6/1/1714\n"
            "         n$filipa/f/id=b1714-1-per1\n"
        )
        _, text = self._pp(schema, src, tmp_path)
        # The kleio header keeps the prefix element.
        assert "kleio$gacto2.str/prefix=lousa" in text
        # No id in the output contains the lousa- prefix.
        assert "lousa-" not in text

    def test_pretty_print_kleio_translations(self, schema, tmp_path):
        """The kleio$ header includes /translations=N+1 (the incremented count)
        when translations=N was set on the source."""
        src = (
            "kleio$gacto2.str/translations=5\n"
            "   fonte$test\n"
        )
        _, text = self._pp(schema, src, tmp_path)
        assert "kleio$gacto2.str" in text
        # The printer increments the count by 1 (gactoxml.pl:442-444).
        assert "/translations=6" in text

    def test_pretty_print_aspects(self, schema, tmp_path):
        """An element with original (%) and comment (#) aspects renders
        as value%original#comment."""
        # Use ls$ with multiple entries: core/original/comment.
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test\n"
            "      bap$b1/1/1/1800\n"
            "         n$joao/m/id=c1\n"
            "            ls$morada/vila%aldeia#confirmed in 1714 census\n"
        )
        _, text = self._pp(schema, src, tmp_path)
        # The ls$ line should embed the aspects.
        ls_line = next(ln for ln in text.splitlines() if "ls$morada" in ln)
        assert "vila%aldeia" in ls_line
        assert "#confirmed in 1714 census" in ls_line

    def test_ids_lands_next_to_source(self, schema, tmp_path):
        """The .ids file is written inside output_dir (which the caller sets
        to the source's directory)."""
        src = "kleio$gacto2.str\n   fonte$test\n"
        _, text = self._pp(schema, src, tmp_path, name="inference-test")
        ids_path = tmp_path / "inference-test.ids"
        assert ids_path.exists()


# ---------------------------------------------------------------------------
# Rename on success
# ---------------------------------------------------------------------------


class TestPromoteIdsOnSuccess:
    """promote_ids_on_success ports rename_files/4."""

    def test_rename_on_success_first_run(self, tmp_path):
        """First successful translation: .cli -> .org, .ids -> .cli."""
        cli = tmp_path / "x.cli"
        ids = tmp_path / "x.ids"
        org = tmp_path / "x.org"
        old = tmp_path / "x.old"
        cli.write_text("original")
        ids.write_text("pretty-printed")

        result = promote_ids_on_success(cli, error_count=0)

        assert org.exists() and org.read_text() == "original"
        assert old.exists() is False
        assert cli.exists() and cli.read_text() == "pretty-printed"
        assert ids.exists() is False
        assert result.get("cli_target_kind") == "org"

    def test_rename_on_success_subsequent_run(self, tmp_path):
        """Subsequent run with .org already present: .cli -> .old
        (previous .old deleted), .ids -> .cli."""
        cli = tmp_path / "x.cli"
        ids = tmp_path / "x.ids"
        org = tmp_path / "x.org"
        old = tmp_path / "x.old"
        cli.write_text("current cli")
        ids.write_text("new pretty-printed")
        org.write_text("original")
        old.write_text("previous old, should be deleted")

        result = promote_ids_on_success(cli, error_count=0)

        # .org preserved (the very first original).
        assert org.read_text() == "original"
        # .cli -> .old (the previous cli is backed up).
        assert old.exists() and old.read_text() == "current cli"
        # .ids -> .cli (the new pretty-printed becomes .cli).
        assert cli.read_text() == "new pretty-printed"
        assert ids.exists() is False
        assert result.get("cli_target_kind") == "old"

    def test_rename_skipped_on_errors(self, tmp_path):
        """With error_count > 0 nothing is renamed; .ids remains."""
        cli = tmp_path / "x.cli"
        ids = tmp_path / "x.ids"
        org = tmp_path / "x.org"
        cli.write_text("original cli")
        ids.write_text("pretty-printed")

        result = promote_ids_on_success(cli, error_count=3)

        assert result == {}
        # Nothing moved.
        assert cli.read_text() == "original cli"
        assert ids.read_text() == "pretty-printed"
        assert org.exists() is False

    def test_rename_no_ids_is_safe(self, tmp_path):
        """If .ids doesn't exist, the function logs and returns; the .cli
        is left untouched (no destructive .cli -> .org rename)."""
        cli = tmp_path / "x.cli"
        cli.write_text("original cli")

        result = promote_ids_on_success(cli, error_count=0)

        assert result == {}
        assert cli.read_text() == "original cli"


# ---------------------------------------------------------------------------
# Round-trip stability (the core purpose)
# ---------------------------------------------------------------------------


class TestRoundTrip:
    """Re-translating the promoted .cli yields identical ids."""

    def _translate(self, schema, src, tmp_path, name):
        errors = ErrorAccumulator()
        groups = translate_string(src, schema, errors)
        pp = ClioPrettyPrinter(f"{name}.cli", tmp_path, schema)
        for g in groups:
            pp.on_group(g)
        pp.close()
        ids_path = tmp_path / f"{name}.ids"
        ids_text = ids_path.read_text()
        return groups, ids_text

    def test_round_trip_id_stability(self, schema, tmp_path):
        """Translate, pretty-print, re-translate the .ids as .cli, and
        compare ids between the two passes.

        Content-group ids must be stable across passes (the whole point of
        the explicit-id promotion: the first pass auto-generates ids, the
        pretty-printer writes them back as explicit /id=..., and the second
        pass reads them as explicit and doesn't change them).

        The ``kleio`` group itself is excluded: its id is intentionally not
        promoted to explicit by the pretty-printer (clioPP.pl:111 suppresses
        the id suffix for the kleio class), so it stays auto-generated and
        gains the translation-count suffix on the second pass. That's
        expected and harmless — the kleio id is not a content id.
        """
        src = (
            "kleio$gacto2.str\n"
            "   fonte$test1\n"
            "      bap$b1/1/1/1800\n"
            "         n$joao/m\n"
            "            pn$antonio\n"
            "            mn$maria\n"
        )
        first_groups, first_ids = self._translate(schema, src, tmp_path, "pass1")
        first_ids_map = {g.name: g.id for g in first_groups if g.name != "kleio"}

        # Re-translate the pretty-printed .ids text as if it were a .cli.
        second_groups, _ = self._translate(schema, first_ids, tmp_path, "pass2")
        second_ids_map = {g.name: g.id for g in second_groups if g.name != "kleio"}

        # Content-group ids should be identical between passes.
        for name, first_id in first_ids_map.items():
            assert second_ids_map.get(name) == first_id, (
                f"id for {name} changed between passes: "
                f"{first_id!r} -> {second_ids_map.get(name)!r}"
            )

    def test_round_trip_with_prefix(self, schema, tmp_path):
        """Round-trip with prefix=lousa: prefixed ids stay stable, and the
        .ids output shows the unprefixed form."""
        src = (
            "kleio$gacto2.str/prefix=lousa\n"
            "   fonte$test1\n"
            "      bap$b1/1/1/1800\n"
            "         n$joao/m/id=b1-per1\n"
        )
        first_groups, first_ids = self._translate(schema, src, tmp_path, "pass1")
        first_ids_map = {g.name: g.id for g in first_groups}

        # Stored ids are prefixed on both passes.
        assert first_ids_map["bap"] == "lousa-b1"
        assert first_ids_map["n"] == "lousa-b1-per1"
        # The .ids shows the unprefixed form.
        assert "lousa-" not in first_ids
        assert "kleio$gacto2.str/prefix=lousa" in first_ids

        # Re-translate.
        second_groups, _ = self._translate(schema, first_ids, tmp_path, "pass2")
        second_ids_map = {g.name: g.id for g in second_groups}
        for name, first_id in first_ids_map.items():
            assert second_ids_map.get(name) == first_id
