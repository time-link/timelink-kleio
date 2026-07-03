"""Translation report writers (``.rpt`` and ``.err`` files).

Mirrors the report output of the Prolog translator (``topLevel.pl`` /
``errors.pl``): a detailed ``.rpt`` processing log and a one-line ``.err``
summary of error/warning counts. Both files share the same header (version
banner + timestamp) and the same ``N  errors.`` / ``N  warnings.`` footer.

``.rpt`` content (comparable against Prolog; the version banner, timestamp and
path-bearing footer lines are stripped by ``exclude_while_comparing.grep``):

* a header,
* per-source group-start markers of the form ``<line>: <group>$<id>``,
* inline ``WARNING:`` / ``ERROR:`` lines,
* (only when ``echo=yes``) every input source line,
* an end-of-file marker and the error/warning counts,
* the ``Groups in this file:[...]`` list + ``Translation finished.``.

The ``echo`` flag matches the Prolog ``echo`` thread-local: when ``yes``, every
input line is echoed to the report; when ``no`` (the default and what the
semantic test harness uses), only group markers and diagnostic lines are
written.
"""
from __future__ import annotations

from datetime import datetime
from pathlib import Path
from typing import Optional

from kleio.errors import ErrorAccumulator
from kleio.parser.models import ParsedGroup


class ReportWriter:
    """Writes ``.rpt`` and ``.err`` translation report files.

    A single instance accumulates report content during a translation and writes
    both files on :meth:`close`. It is driven by callbacks from
    :func:`kleio.parser.builder.translate_file` (per-line echo, per-group
    markers) and reads accumulated diagnostics from an
    :class:`~kleio.errors.ErrorAccumulator`.
    """

    def __init__(
        self,
        source_file: str | Path,
        output_dir: str | Path,
        errors: ErrorAccumulator,
        *,
        echo: bool = False,
        schema=None,
        structure_file: str = "",
        prefix: str = "",
        autorel: str = "",
        translation_count: int = 1,
        obs: str = "",
        translator_name: str = "kleio-python",
    ) -> None:
        """Configure the report writer.

        Args:
            source_file: Path to the ``.cli``/``.kleio`` source being translated.
            output_dir: Directory in which to write the ``.rpt``/``.err`` files.
            errors: The error accumulator holding translation diagnostics.
            echo: If ``True``, echo every input source line to the ``.rpt``.
            schema: Optional SchemaRegistry, used to echo only act-inheriting
                groups in the report (see on_group). When None, all groups with
                an id are echoed.
            structure_file: Structure (schema) file name, for the report header.
            prefix: Source id prefix (if any), for the report header.
            autorel: Autorel prefix (if any), for the report header.
            translation_count: Translation counter, for the report header.
            obs: Observation string, for the report header.
            translator_name: Translator identifier used in the version banner.
        """
        self._source_path = Path(source_file)
        self._output_dir = Path(output_dir)
        self._errors = errors
        self._echo = echo
        self._schema = schema
        self._structure_file = structure_file
        self._prefix = prefix
        self._autorel = autorel
        self._translation_count = translation_count
        self._obs = obs
        self._translator_name = translator_name

        base = self._source_path.stem
        self._rpt_path = self._output_dir / f"{base}.rpt"
        self._err_path = self._output_dir / f"{base}.err"

        # Accumulated report body lines (everything between header and footer).
        self._lines: list[str] = []
        # Distinct group names encountered, in first-seen order, for the
        # "Groups in this file:[...]" footer line.
        self._group_names: list[str] = []
        self._group_names_seen: set[str] = set()
        # The source document name (top-level kleio group), set when the first
        # source "fonte$" line is processed. Defaults to "kleio".
        self._document_name: str = "kleio"
        self._started = False

    # ------------------------------------------------------------------
    # Callbacks invoked during translation (translate_file wires these up).
    # ------------------------------------------------------------------
    def on_line(self, line_number: int, line: str) -> None:
        """Echo a source line to the report (only meaningful when echo=True).

        Mirrors Prolog ``echo_line/2``: a no-op unless echo is enabled.
        """
        if self._echo and line.strip():
            self._lines.append(f"{line_number}: {line}")

    def on_group(self, group: ParsedGroup) -> None:
        """Record a completed group.

        Tracks every group name for the footer's ``Groups in this file:[...]``
        list, and emits the ``<line>: <group>$<id>`` marker for act groups only.

        With echo off, the Prolog report (gactoxml.pl historical_act_export,
        line 673) prints a ``<line>: <group>$<id>`` marker ONLY for groups that
        inherit from ``historical-act`` (i.e. base class ``event``: bap, obito,
        cas, rol, lista, escritura, devassa, ...). Person/object/attribute/
        relation/link groups are not echoed. Without this filter the report
        would list every nested group (n, ls, referido, atr, ...) and not match
        the reference.
        """
        # Track every group name for the footer's "Groups in this file:" list.
        if group.name and group.name not in self._group_names_seen:
            self._group_names.append(group.name)
            self._group_names_seen.add(group.name)

        if not group.id or self._schema is None:
            return

        # Source groups: emit a "** Processing source <name>$<id>" marker,
        # matching Prolog's historical_source_export (gactoxml.pl:639).
        supers = self._schema.super_groups(group.name)
        if "historical-source" in supers:
            self._lines.append(f"** Processing source {group.name}${group.id}")
            return

        # Act groups: emit the "<line>: <group>$<id>" marker, matching Prolog's
        # historical_act_export (gactoxml.pl:673). 'event' is the Python
        # schema's root for Prolog's 'historical-act'.
        base = self._schema.base_class(group.name)
        if base == "event":
            self._lines.append(f"{group.line_number}: {group.name}${group.id}")

    # ------------------------------------------------------------------
    # Writing.
    # ------------------------------------------------------------------
    def _version_banner(self) -> str:
        # The version/build tokens are intentionally left as placeholders.
        # ``exclude_while_comparing.grep`` strips this whole line, so the exact
        # values do not affect the semantic diff; keeping the placeholder keeps
        # the report visually consistent with the Prolog banner.
        return f"KleioTranslator - server version @@VERSION@@ - build @@BUILD@@ @@DATE@@"

    def _timestamp(self) -> str:
        # Prolog format: "3-7-2026 7-12" (day-month-year hour-minute).
        return datetime.now().strftime("%-d-%-m-%Y %-H-%M")

    def _counts_line(self) -> list[str]:
        # NOTE the two spaces between the number and the word, matching Prolog.
        return [
            f"{self._errors.error_count}  errors. ",
            f"{self._errors.warning_count}  warnings.",
        ]

    def _format_warnings(self) -> list[str]:
        """Format accumulated warnings/errors as inline report lines.

        Prolog emits ``WARNING: <file> line <n> <message>`` lines in the body
        of the report; these survive the diff filter and must match.
        """
        out: list[str] = []
        fname = self._source_path.name
        for w in self._errors.warnings:
            ln = f" line {w.line_number}" if w.line_number else ""
            out.append(f"WARNING: {fname}{ln} {w.message}")
        for e in self._errors.errors:
            ln = f" line {e.line_number}" if e.line_number else ""
            out.append(f"ERROR: {fname}{ln} {e.message}")
        return out

    def close(self) -> list[str]:
        """Write the ``.rpt`` and ``.err`` files.

        Returns:
            The list of file paths written (``.rpt`` then ``.err``).
        """
        written: list[str] = []
        banner = self._version_banner()
        ts = self._timestamp()
        counts = self._counts_line()
        warnings = self._format_warnings()

        # ---- .err : banner + timestamp + counts ----
        err_body = [banner, ts, *counts]
        self._err_path.write_text("\n".join(err_body) + "\n", encoding="utf-8")
        written.append(str(self._err_path))

        # ---- .rpt : full processing log ----
        rpt: list[str] = []
        rpt.append(banner)
        rpt.append(ts)
        rpt.append("")
        rpt.append(f"Processing data file {self._source_path.name}")
        rpt.append("-" * 43)
        rpt.append(f"{self._translator_name} translation module (XML).")
        rpt.append(f"** New document: {self._document_name}")
        rpt.append("=" * 25)
        rpt.append("kleio translation started")
        rpt.append("=" * 25)
        rpt.append(f"Structure: {Path(self._structure_file).name if self._structure_file else ''}")
        rpt.append(f"Prefix: {self._prefix}")
        rpt.append(f"Autorel: {self._autorel}")
        rpt.append(f"Translation count: {self._translation_count}")
        rpt.append(f"Obs: {self._obs}")

        # Body: echoed source lines (if echo), group markers, warnings/errors.
        # Insert diagnostics inline at the end of the body, before the
        # end-of-file marker, matching the Prolog layout.
        rpt.extend(self._lines)
        rpt.extend(warnings)

        rpt.append("*** End of File")
        rpt.append("")
        # Path-bearing footer lines are stripped by the diff filter; we emit
        # them for human readability but their exact values are irrelevant to
        # the semantic comparison.
        rpt.append(f"Structure file: {self._structure_file}")
        rpt.append(f"Kleio file: {self._source_path}")
        rpt.extend(counts)
        rpt.append(f"Groups in this file:[{','.join(self._group_names)}]")
        rpt.append("Translation finished.")

        self._rpt_path.write_text("\n".join(rpt) + "\n", encoding="utf-8")
        written.append(str(self._rpt_path))

        return written
