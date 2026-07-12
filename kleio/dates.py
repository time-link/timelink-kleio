"""Kleio date parsing and sortable value computation.

This module ports the Prolog ``match_date/3`` family
(``gactoxml.pl:1220-1446``) to Python. It converts a Kleio date string into:

- ``original``: the date text as written in the source file.
- ``type``: ``single``, ``relative``, or ``range`` (or ``error``).
- ``value``: a sortable string encoding both the date and its type.
- ``date``: a structured dict describing the parsed components.

Sortable value scheme (from issue #1):

==================  =========================================
Fractional part     Meaning
==================  =========================================
``.0``              single date -> ``round(value)``
``+.3``             after ``round(value)``
``-.3``             before ``round(value)``
``+.1``             open-ended range starting at ``round(value)``
``-.1``             open-start range ending at ``round(value)``
other               range from ``round(value)`` to remainder
==================  =========================================
"""

from __future__ import annotations

import json
import math
import re
from dataclasses import dataclass, field
from typing import Optional, Union

__all__ = ["ParsedDate", "parse_date", "date_extra_info_json"]


# --- helpers ---------------------------------------------------------------


def _is_int(token: str) -> bool:
    """True if *token* is a non-empty integer string (no sign)."""
    return token.isdigit() and len(token) > 0


def _try_int(token: str) -> Optional[int]:
    try:
        return int(token)
    except (ValueError, TypeError):
        return None


def _format_value(num: float) -> str:
    """Format a numeric sortable value as the Prolog does.

    The Prolog uses ``atom_number/2`` which, for a float like ``15800000.3``,
    produces ``"15800000.3"``; for an integer-valued float it produces an
    integer atom like ``"15800000"``. We mirror that.
    """
    if num == int(num):
        return str(int(num))
    # Trim trailing zeros from the fractional part.
    s = repr(num)
    # repr may give e.g. '16610413.3' or scientific notation for tiny
    # fractions; fall back to a fixed-point representation.
    if "e" in s or "E" in s:
        s = f"{num:.10f}".rstrip("0").rstrip(".")
    return s


def _numeric_value(date_dict: Optional[dict]) -> float:
    """Extract the numeric sortable value from a parsed date's ``date`` dict.

    Handles both single dates (``{"value": N}``) and relative dates
    (``{"value": {"value": N}}``), mirroring the Prolog's
    ``option(value(V), Extra)`` which recurses through the nested dicts.
    """
    if date_dict is None:
        return 0
    val = date_dict.get("value", 0)
    if isinstance(val, dict):
        val = val.get("value", 0)
    return val


# --- data model ------------------------------------------------------------


@dataclass
class ParsedDate:
    """Result of parsing one Kleio date expression.

    Attributes:
        original: the date text as written in the source.
        type: ``single``, ``relative``, ``range``, or ``error``.
        value: the sortable string (see module docstring).
        date: structured dict mirroring the Prolog ``DateInfo`` dict,
            with ``subtype`` and ``value``/``from``/``to`` sub-dicts.
            ``None`` when there is no parseable date (the ``{}`` case).
    """

    original: str
    type: str  # 'single' | 'relative' | 'range' | 'error'
    value: str  # sortable string; '0' on failure
    date: Optional[dict] = field(default=None)

    def to_json_dict(self) -> Optional[dict]:
        """Return the dict to be serialized as the ``date_extra_info`` JSON.

        Returns ``None`` when there is no date info (emitting ``{}`` in the
        XML, matching the Prolog ``empty{}`` case at gactoxml.pl:773).
        """
        if self.date is None:
            return None
        return {
            "date": self.date,
            "original": self.original,
            "type": self.type,
            "value": self.value,
        }


# --- single date -----------------------------------------------------------


def _single_subtype(year: int, month: int, day: int) -> str:
    """Determine precision subtype from Y/M/D components."""
    if year == 0 and month == 0 and day == 0:
        return "missing"
    if month == 0 and day == 0:
        return "y"
    if day == 0:
        return "ym"
    return "ymd"


def _match_single(token: str) -> Optional[ParsedDate]:
    """Parse a single date (no ``>``/``<`` prefix, no ``:`` range).

    Accepts the following Kleio formats (mirroring ``match_single_date/3``):

    - ``YYYY``                -> subtype ``y``
    - ``YYYYMM`` / ``YYYY-MM`` -> subtype ``ym``
    - ``YYYYMMDD``            -> subtype ``ymd``
    - ``YYYY-M-D`` / ``D-M-YYYY`` / ``M-Y`` etc.

    The Prolog uses the number of digits and the presence of ``-`` to
    disambiguate. We reproduce the same heuristics.
    """
    token = token.strip()

    # --- dash-separated forms ---
    if "-" in token:
        parts = token.split("-")
        nums = [_try_int(p) for p in parts]
        if any(n is None for n in nums):
            return None

        # Y-M-D or D-M-Y: three components.
        # The Prolog ALWAYS assigns subtype=ymd for the 3-component dash
        # form (gactoxml.pl:1262-1303), regardless of zero components.
        # The all-zero case (0-0-0 / 0000-00-00) gets subtype=missing.
        if len(nums) == 3:
            a, b, c = nums
            if a is not None and b is not None and c is not None:
                # All-zero -> missing (gactoxml.pl:1305-1308).
                if a == 0 and b == 0 and c == 0:
                    return ParsedDate(
                        original=token,
                        type="single",
                        value="0",
                        date={"subtype": "missing", "value": 0},
                    )
                # Y-M-D: a is year (a > 31 and a < 9999)
                if a > 31 and a < 9999 and 0 <= b < 13 and 0 <= c < 32:
                    val = a * 10000 + b * 100 + c
                    return ParsedDate(
                        original=token,
                        type="single",
                        value=_format_value(val),
                        date={"subtype": "ymd", "value": val},
                    )
                # D-M-Y: c is year (c > 0)
                if c > 0 and c < 9999 and 0 < b < 13 and 0 <= a < 32:
                    val = c * 10000 + b * 100 + a
                    return ParsedDate(
                        original=token,
                        type="single",
                        value=_format_value(val),
                        date={"subtype": "ymd", "value": val},
                    )
            return None

        # Y-M or M-Y: two components.
        # The Prolog always assigns subtype=ym for the 2-component dash
        # form (gactoxml.pl:1273-1280, 1311-1317).
        if len(nums) == 2:
            a, b = nums
            if a is not None and b is not None:
                # M-Y: b is the year (b >= 0, b < 9999), a is month (a < 13)
                # Y-M: a is the year (a >= 0, a < 9999), b is month
                # Prolog matches M-Y first when M < 13 and Y < 9999;
                # we try the interpretation that yields a valid date.
                if 0 <= a < 13 and 0 <= b < 9999 and a > 0:
                    # M-Y form
                    val = b * 10000 + a * 100
                    return ParsedDate(
                        original=token,
                        type="single",
                        value=_format_value(val),
                        date={"subtype": "ym", "value": val},
                    )
                if 0 <= a < 9999 and 0 <= b < 13:
                    # Y-M form
                    val = a * 10000 + b * 100
                    return ParsedDate(
                        original=token,
                        type="single",
                        value=_format_value(val),
                        date={"subtype": "ym", "value": val},
                    )
            return None

        return None

    # --- bare numeric forms (no dash) ---
    # Mirrors the Prolog clause ordering (gactoxml.pl:1320-1367) which uses
    # log10(value) guards, NOT digit count. The clauses are tried in order:
    #   0              -> missing
    #   log10(v) > 7   -> YYYYMMDD family (refined by trailing-digit check)
    #   log10(v) > 6   -> YYYYMMD -> *10
    #   log10(v) > 5   -> YYYYMM  -> *100
    #   log10(v) > 3   -> YYYY    -> *10000
    if _is_int(token):
        n = int(token)

        # 0 -> missing. Prolog formats value as '00000000' (gactoxml.pl:1320).
        if n == 0:
            return ParsedDate(
                original=token,
                type="single",
                value="00000000",
                date={"subtype": "missing", "value": 0},
            )

        log10 = math.log10(n) if n > 0 else 0

        # log10(v) > 7  -> v >= 10,000,000: YYYYMMDD family.
        # Three sub-clauses, tried in order (gactoxml.pl:1322-1344):
        #   ends with '0000' (chars 4..7) -> subtype y
        #   ends with '00'   (chars 6..7) -> subtype ym
        #   otherwise                         -> subtype ymd
        if log10 > 7:
            s = str(n)
            if len(s) >= 8 and s[4:8] == "0000":
                sub = "y"
            elif len(s) >= 8 and s[6:8] == "00":
                sub = "ym"
            else:
                sub = "ymd"
            return ParsedDate(
                original=token,
                type="single",
                value=_format_value(n),
                date={"subtype": sub, "value": n},
            )

        # log10(v) > 6  -> v >= 1,000,000: YYYYMMD -> *10 (gactoxml.pl:1346).
        if log10 > 6:
            val = n * 10
            return ParsedDate(
                original=token,
                type="single",
                value=_format_value(val),
                date={"subtype": "ymd", "value": val},
            )

        # log10(v) > 5  -> v >= 100,000: YYYYMM -> *100 (gactoxml.pl:1355).
        if log10 > 5:
            val = n * 100
            return ParsedDate(
                original=token,
                type="single",
                value=_format_value(val),
                date={"subtype": "ym", "value": val},
            )

        # log10(v) > 3  -> v >= 1,000: YYYY -> *10000 (gactoxml.pl:1362).
        if log10 > 3:
            val = n * 10000
            return ParsedDate(
                original=token,
                type="single",
                value=_format_value(val),
                date={"subtype": "y", "value": val},
            )

    return None


# --- relative date (>date / <date) ----------------------------------------


def _match_relative(token: str) -> Optional[ParsedDate]:
    """Parse a relative date: ``>DATE`` (after) or ``<DATE`` (before).

    Adds 0.3 for "after", subtracts 0.3 for "before" (mirroring
    ``match_single_relative_date/3`` at gactoxml.pl:1246-1260).
    """
    token = token.strip()
    if not token:
        return None

    prefix = token[0]
    if prefix not in (">", "<"):
        return None

    inner = token[1:].strip()
    single = _match_single(inner)
    if single is None or single.date is None:
        return None

    base_val = single.date.get("value", 0)
    if prefix == ">":
        adjusted = base_val + 0.3
        rel = {"subtype": "after", "value": single.date}
    else:  # '<'
        adjusted = base_val - 0.3
        rel = {"subtype": "before", "value": single.date}

    return ParsedDate(
        original=token,
        type="relative",
        value=_format_value(adjusted),
        date=rel,
    )


# --- range dates (from:to, open ranges) ------------------------------------


def _match_range(token: str) -> Optional[ParsedDate]:
    """Parse a range date: ``FROM:TO``, ``FROM:``, ``:TO``.

    The ``from`` and ``to`` parts may themselves be single or relative dates.

    Mirrors ``match_range/3`` (gactoxml.pl:1370-1433):

    - ``from:to``  -> ``subtype=from_to``, value ``"V1.V2"``
    - ``from:``    -> ``subtype=from_only``, value ``V1 + 0.1``
    - ``:to``      -> ``subtype=to_only``,   value ``V1 - 0.1``
    """
    token = token.strip()
    if ":" not in token:
        return None

    # Split only on the first ':' to allow relative dates (which have no ':').
    left, _, right = token.partition(":")

    # Parse each side; each may be single or relative.
    def _parse_side(side: str) -> Optional[ParsedDate]:
        side = side.strip()
        if not side:
            return None
        return _match_single(side) or _match_relative(side)

    left_parsed = _parse_side(left) if left.strip() else None
    right_parsed = _parse_side(right) if right.strip() else None

    # from:to (both sides present)
    if left_parsed and right_parsed:
        v1 = _numeric_value(left_parsed.date)
        v2 = _numeric_value(right_parsed.date)
        return ParsedDate(
            original=token,
            type="range",
            value=f"{_format_value(v1)}.{_format_value(v2)}",
            date={
                "subtype": "from_to",
                "from": left_parsed.date,
                "to": right_parsed.date,
            },
        )

    # from: (open-ended)
    if left_parsed and not right.strip():
        v1 = _numeric_value(left_parsed.date)
        adjusted = v1 + 0.1
        return ParsedDate(
            original=token,
            type="range",
            value=_format_value(adjusted),
            date={
                "subtype": "from_only",
                "from": left_parsed.date,
            },
        )

    # :to (open-start)
    if not left.strip() and right_parsed:
        v2 = _numeric_value(right_parsed.date)
        adjusted = v2 - 0.1
        return ParsedDate(
            original=token,
            type="range",
            value=_format_value(adjusted),
            date={
                "subtype": "to_only",
                "to": right_parsed.date,
            },
        )

    return None


# --- top-level entry point -------------------------------------------------


def parse_date(text: str) -> ParsedDate:
    """Parse a Kleio date string.

    Tries single -> relative -> range in order (mirroring the Prolog clause
    ordering). On failure returns a ``ParsedDate`` with ``type="error"`` and
    ``value="0"``.

    Args:
        text: the raw date text (may be empty).

    Returns:
        A :class:`ParsedDate`. ``date`` is ``None`` when the text is empty
        or unparseable (yielding the ``{}`` JSON case).
    """
    if text is None:
        return ParsedDate(original="", type="error", value="0", date=None)

    original = text.strip()
    if not original:
        # Empty date text: the Prolog emits empty{} -> "{}".
        return ParsedDate(original="", type="error", value="0", date=None)

    # Try each parser in Prolog clause order.
    result = (
        _match_single(original)
        or _match_relative(original)
        or _match_range(original)
    )
    if result is not None:
        # Preserve the original text exactly as written (e.g. "1620"
        # vs "16200000").
        result.original = original
        return result

    # Failed parse.
    return ParsedDate(original=original, type="error", value="0", date=None)


def date_extra_info_json(text: str) -> str:
    """Return the JSON string for the ``date_extra_info`` element.

    Returns ``"{}"`` when there is no parseable date (matching the Prolog
    ``empty{}`` case), otherwise a pretty-printed JSON object with keys
    ``date``, ``original``, ``type``, ``value``.

    The Prolog uses ``dict_json_string/2`` which pretty-prints with 2-space
    indentation. We match that so the semantic comparison is clean.
    """
    parsed = parse_date(text)
    info = parsed.to_json_dict()
    if info is None:
        return "{}"
    return json.dumps(info, indent=2, ensure_ascii=False)
