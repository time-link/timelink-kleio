"""Kleio "pretty print" — emits a ``.ids`` file with explicit ids.

This module ports ``clioPP.pl`` from the Prolog implementation. The
printer is fed one parsed group at a time (mirroring how ``clioPP/2`` is
called from ``db_store`` in ``gactoxml.pl``) and writes a faithful copy
of the source with all auto-generated ids made explicit.

The resulting ``.ids`` file is what gets promoted to ``.cli`` on a
successful translation (see :mod:`kleio.export.rename`). Promoting the
pretty-printed version is what keeps database imports stable across
re-translations: the explicit ids pin every entity, so a re-import after
editing the transcription updates rows instead of creating duplicates.

The id namespace prefix from the top-level ``kleio$`` group's ``prefix``
element is stripped from every id in the output (mirroring
``remove_id_prefix/2``), while the ``prefix=`` element itself is
preserved on the ``kleio$`` header line.
"""
from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Optional

if TYPE_CHECKING:
    from kleio.parser.models import ParsedGroup
    from kleio.schema.registry import SchemaRegistry


# Group classes that do NOT get a blank line before them and do NOT get
# an explicit /id=... suffix. Sourced from clioPP.pl:101-105,111.
_NO_BLANK_LINE_CLASSES = {
    "attribute", "relation", "kleio", "historical-source", "link", "end",
}
# Subset that also suppresses the trailing /id=... (clioPP.pl:111).
_NO_ID_SUFFIX_CLASSES = {
    "attribute", "property", "link", "relation", "group-element",
    "kleio", "end",
}


class ClioPrettyPrinter:
    """Stream a ``.ids`` pretty-printed copy of the source as groups complete.

    Usage mirrors :class:`kleio.export.report_writer.ReportWriter` and
    :class:`kleio.export.xml_exporter.XmlExporter`::

        pp = ClioPrettyPrinter(source_file, output_dir, schema)
        for group in groups:
            pp.on_group(group)        # called from the builder callback
        paths = pp.close()             # writes the .ids file
    """

    def __init__(
        self,
        source_file: str,
        output_dir: Path,
        schema: SchemaRegistry,
    ) -> None:
        self._schema = schema
        self._source_file = source_file

        base = Path(source_file).stem
        self._output_path = Path(output_dir) / f"{base}.ids"

        # Buffered lines; flushed in close().
        self._lines: list[str] = []

        # Id namespace prefix, captured when the kleio$ group is seen.
        self._id_prefix: str = ""
        self._use_id_prefix: bool = False

        # Translation count from the kleio$ group, if any.
        self._transcount: Optional[int] = None

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def on_group(self, group: ParsedGroup) -> None:
        """Format one group and append it to the buffer.

        Called once per group in translation order, mirroring the Prolog
        ``clioPP(G, NID)`` invocation from ``db_store`` (gactoxml.pl:359).
        """
        # Capture the kleio prefix/translations on first sight of the
        # kleio group, before formatting any ids.
        if group.name == "kleio":
            self._capture_kleio_elements(group)

        self._lines.extend(self._format_group(group))

    def close(self) -> list[str]:
        """Write the buffered lines to the ``.ids`` file.

        Returns:
            The list of file paths written (always the single ``.ids`` path).
        """
        text = "\n".join(self._lines)
        if text and not text.endswith("\n"):
            text += "\n"
        self._output_path.write_text(text, encoding="utf-8")
        return [str(self._output_path)]

    # ------------------------------------------------------------------
    # kleio prefix handling
    # ------------------------------------------------------------------

    def _capture_kleio_elements(self, group: ParsedGroup) -> None:
        """Read the ``prefix`` and ``translations`` elements of the kleio group
        and increment the translation count.

        Mirrors group_export(kleio,_) in gactoxml.pl:415,432-433,442-444: the
        parsed translations value is incremented by 1 unconditionally, so the
        .ids output carries the new count for the next pass. The same
        incremented value drives the translation-count suffix on auto-generated
        ids in the builder (see GroupBuilder._make_id).
        """
        prefix_value = group.get_element_value("prefix").strip()
        if prefix_value:
            self._id_prefix = prefix_value
            self._use_id_prefix = True

        tc_text = group.get_element_value("translations").strip()
        if tc_text:
            try:
                self._transcount = int(tc_text) + 1
            except ValueError:
                # Prolog falls back to 0 then +1 (=1) with a warning.
                self._transcount = 1
        else:
            # No translations= element: first translation of this file.
            self._transcount = 1

    def _strip_id_prefix(self, group_id: str) -> str:
        """Strip the kleio namespace prefix from an id.

        Mirrors remove_id_prefix/2 (gactoxml.pl:1506-1512). If a prefix is
        in use and ``group_id`` starts with ``<prefix>-``, the prefix is
        removed; otherwise the id is returned unchanged.
        """
        if not self._use_id_prefix or not group_id:
            return group_id
        prefix_with_dash = f"{self._id_prefix}-"
        if group_id.startswith(prefix_with_dash):
            return group_id[len(prefix_with_dash):]
        return group_id

    # ------------------------------------------------------------------
    # Group formatting
    # ------------------------------------------------------------------

    def _format_group(self, group: ParsedGroup) -> list[str]:
        """Format one group as one (or more) lines per clioPP2/2.

        Returns a list of lines (usually one; the leading blank line, if
        any, is a separate empty string entry).
        """
        base_class = self._schema.base_class(group.name) if self._schema.structure else ""
        depth = len(group.path)
        indent = "   " * depth

        out: list[str] = []

        # Blank line before most groups (clioPP.pl:101-105).
        if base_class not in _NO_BLANK_LINE_CLASSES:
            out.append("")

        # Build the header: <indent>name$<positional>/<named>...
        parts: list[str] = [f"{indent}{group.name}$"]

        id_emitted_positionally = False
        position_names = self._position_element_names(group.name)

        # Positional (locus) elements first, separated by '/'.
        locus_values = self._format_positional_elements(group, position_names)
        for i, (text, is_id) in enumerate(locus_values):
            if i == 0:
                parts.append(text)
            else:
                parts.append("/" + text)
            if is_id:
                id_emitted_positionally = True

        # Named elements next: /name=value (clioPP.pl:162-188).
        named_parts, named_id_emitted = self._format_named_elements(
            group, position_names
        )
        if named_parts and not locus_values:
            # No positional element preceded; the first named element still
            # needs the leading '/' separator from the '$'.
            pass
        for np_text in named_parts:
            parts.append("/" + np_text)
        if named_id_emitted:
            id_emitted_positionally = True

        # Trailing /id=<id> when no positional element was the id-element
        # and the group class is not in the suppress list.
        if (not id_emitted_positionally
                and base_class not in _NO_ID_SUFFIX_CLASSES):
            display_id = self._strip_id_prefix(group.id)
            if display_id:
                parts.append(f"/id={display_id}")

        # kleio$ special case: append /translations=N (clioPP.pl:123-128).
        if group.name == "kleio" and self._transcount is not None:
            parts.append(f"/translations={self._transcount}")

        out.append("".join(parts))
        return out

    # ------------------------------------------------------------------
    # Element formatting helpers
    # ------------------------------------------------------------------

    def _position_element_names(self, group_name: str) -> list[str]:
        """The schema's ``position`` (locus) element names for a group."""
        group_def = self._schema.get_group(group_name) if self._schema.structure else None
        if group_def is None:
            return []
        return list(group_def.position)

    def _format_positional_elements(
        self,
        group: ParsedGroup,
        position_names: list[str],
    ) -> list[tuple[str, bool]]:
        """Format the positional (locus) elements.

        Returns a list of (formatted_text, is_id_element) tuples, one per
        positional element that has a non-empty core value. The id-element
        flag is set when the element's schema definition has
        ``identification == 'sic'`` (clioPP.pl:192-195 clioPP_check_id_element).
        """
        result: list[tuple[str, bool]] = []
        for el_name in position_names:
            element = self._get_element(group, el_name)
            if element is None:
                continue
            core = element.get_core_text()
            if not core:
                # Prolog skips empty-core locus elements entirely.
                continue
            is_id = self._is_id_element(el_name)
            display = self._strip_id_prefix(core) if is_id else core
            text = self._format_with_aspects(element, display_value=display)
            result.append((text, is_id))
        return result

    def _format_named_elements(
        self,
        group: ParsedGroup,
        position_names: list[str],
    ) -> tuple[list[str], bool]:
        """Format the named (non-positional) elements as /name=value.

        Order follows the schema's full element list
        (position + guaranteed + also), de-duplicated (clioPP.pl:164-166).
        ``translations`` is excluded (it's emitted separately on the
        kleio header); positional elements are excluded (already emitted).

        Returns (list_of_formatted_parts, any_id_element_emitted).
        """
        position_set = set(position_names)
        position_set.add("translations")  # clioPP.pl:167

        group_def = self._schema.get_group(group.name) if self._schema.structure else None
        if group_def is None:
            order = [el.name for el in group.elements]
        else:
            # Schema element order (position + guaranteed + also), de-duped.
            order: list[str] = []
            seen: set[str] = set()
            for n in group_def.position + group_def.guaranteed + group_def.also:
                if n not in seen:
                    seen.add(n)
                    order.append(n)
            # Append any elements present on the group but not in the schema
            # (preserves user-defined extras in source order).
            for el in group.elements:
                if el.name and el.name not in seen:
                    seen.add(el.name)
                    order.append(el.name)

        parts: list[str] = []
        any_id = False
        emitted: set[str] = set()
        for el_name in order:
            if el_name in position_set:
                continue
            element = self._get_element(group, el_name)
            if element is None:
                continue
            core = element.get_core_text()
            if not core:
                # Prolog still emits the name= prefix only if there are
                # original/comment aspects; to keep this port simple we
                # skip empty-core elements unless they have an aspect.
                if not element.get_original_text() and not element.get_comment_text():
                    continue
            is_id = self._is_id_element(el_name)
            if is_id:
                any_id = True
            display = self._strip_id_prefix(core) if is_id else core
            text = self._format_with_aspects(element, name=el_name, display_value=display)
            parts.append(text)

        return parts, any_id

    def _format_with_aspects(
        self,
        element: ParsedElement,
        name: Optional[str] = None,
        display_value: Optional[str] = None,
    ) -> str:
        """Format an element's aspects (core/original/comment).

        Mirrors clioPP_locus/clioPP_elements aspect emission
        (clioPP.pl:149-156, 178-185). ``%``-prefixed original, ``#``-prefixed
        comment. Multi-entry values are joined by ``;`` (the entry
        separator) — this is the same known limitation as the Prolog
        original (clioPP.pl:74-79).

        If ``name`` is given, the result is prefixed with ``name=``.
        ``display_value`` overrides the core text (used for id-prefix
        stripping); when None the element's own core text is used.
        """
        if display_value is None:
            display_value = element.get_core_text()

        original = element.get_original_text()
        comment = element.get_comment_text()

        parts: list[str] = []
        if display_value:
            parts.append(display_value)
        if original:
            parts.append("%" + original)
        if comment:
            parts.append("#" + comment)
        body = "".join(parts)

        if name:
            return f"{name}={body}"
        return body

    # ------------------------------------------------------------------
    # Small schema helpers
    # ------------------------------------------------------------------

    def _get_element(self, group: ParsedGroup, name: str) -> Optional[ParsedElement]:
        for el in group.elements:
            if el.name == name:
                return el
        return None

    def _is_id_element(self, element_name: str) -> bool:
        element_def = self._schema.get_element(element_name)
        if element_def is None:
            return False
        return element_def.identification == "sic"
