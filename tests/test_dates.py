"""Tests for kleio.dates — the match_date port.

Validates against the sortable-value scheme and JSON format documented in
GitHub issue #1 and emitted by the Prolog reference translator.
"""

import json

import pytest

from kleio.dates import ParsedDate, parse_date, date_extra_info_json


# ---------------------------------------------------------------------------
# Single dates
# ---------------------------------------------------------------------------


class TestSingleDates:
    """Single-date parsing (match_single_date/3)."""

    def test_year_only(self):
        p = parse_date("1620")
        assert p.type == "single"
        assert p.value == "16200000"
        assert p.date == {"subtype": "y", "value": 16200000}

    def test_year_only_preserves_original(self):
        p = parse_date("1620")
        assert p.original == "1620"

    def test_8digit_year_month(self):
        # YYYYMM00 -> subtype y
        p = parse_date("16110000")
        assert p.type == "single"
        assert p.value == "16110000"
        assert p.date["subtype"] == "y"

    def test_ymd_dash_separated(self):
        p = parse_date("1705-10-01")
        assert p.type == "single"
        assert p.value == "17051001"
        assert p.date == {"subtype": "ymd", "value": 17051001}

    def test_dmy_dash_separated(self):
        # D-M-Y form (day first)
        p = parse_date("01-10-1705")
        assert p.type == "single"
        assert p.value == "17051001"
        assert p.date["subtype"] == "ymd"

    def test_year_month_dash(self):
        # Y-M (2-component dash form)
        p = parse_date("1705-10")
        assert p.type == "single"
        assert p.value == "17051000"
        assert p.date["subtype"] == "ym"

    def test_zero_is_missing(self):
        p = parse_date("0")
        assert p.type == "single"
        # Prolog formats the value as '00000000' (gactoxml.pl:1320).
        assert p.value == "00000000"
        assert p.date == {"subtype": "missing", "value": 0}

    def test_8digit_full_date(self):
        p = parse_date("17051001")
        assert p.type == "single"
        assert p.value == "17051001"
        assert p.date["subtype"] == "ymd"

    def test_6digit_is_year_month(self):
        # 6-digit form encodes YYYYMM -> subtype ym (gactoxml.pl:1355-1360).
        # The value is expanded to 8 digits (YYYYMM00), but the subtype
        # reflects the original precision, not the expanded zeros.
        p = parse_date("163900")
        assert p.type == "single"
        assert p.value == "16390000"
        assert p.date["subtype"] == "ym"

    def test_4digit_is_year_only(self):
        p = parse_date("1580")
        assert p.type == "single"
        assert p.value == "15800000"
        assert p.date["subtype"] == "y"

    def test_large_numeric_date_uses_log10_guard(self):
        # The Prolog uses log10(value) > 7, NOT digit count, so a 9-digit
        # number like 117370413 matches the YYYYMMDD clause (gactoxml.pl:1338).
        p = parse_date("117370413")
        assert p.type == "single"
        assert p.value == "117370413"
        assert p.date["subtype"] == "ymd"

    def test_all_zeros_dash_form_is_missing(self):
        # 0000-00-00 (D-M-Y all zeros) -> subtype=missing
        p = parse_date("0000-00-00")
        assert p.type == "single"
        assert p.date["subtype"] == "missing"
        assert p.date["value"] == 0

    def test_day_zero_dash_form_is_ymd(self):
        # 1654-00-00 (Y-M-D with zero M,D) -> still subtype=ymd
        # The Prolog dash-form clause always assigns ymd.
        p = parse_date("1654-00-00")
        assert p.type == "single"
        assert p.date["subtype"] == "ymd"
        assert p.value == "16540000"

    def test_month_day_dash_form_is_ymd(self):
        # 1551-10-00 -> subtype=ymd (Prolog dash-form always ymd)
        p = parse_date("1551-10-00")
        assert p.type == "single"
        assert p.date["subtype"] == "ymd"
        assert p.value == "15511000"


# ---------------------------------------------------------------------------
# Relative dates (> / <)
# ---------------------------------------------------------------------------


class TestRelativeDates:
    """Relative-date parsing (match_single_relative_date/3)."""

    def test_after_date(self):
        p = parse_date(">1580")
        assert p.type == "relative"
        assert p.value == "15800000.3"
        assert p.date["subtype"] == "after"
        assert p.date["value"] == {"subtype": "y", "value": 15800000}

    def test_before_date(self):
        p = parse_date("<1640")
        assert p.type == "relative"
        # 16400000 - 0.3 = 16399999.7
        assert p.value == "16399999.7"
        assert p.date["subtype"] == "before"

    def test_after_full_date(self):
        p = parse_date(">17051001")
        assert p.type == "relative"
        assert p.value == "17051001.3"

    def test_relative_preserves_original(self):
        p = parse_date(">1580")
        assert p.original == ">1580"


# ---------------------------------------------------------------------------
# Range dates (from:to, open ranges)
# ---------------------------------------------------------------------------


class TestRangeDates:
    """Range-date parsing (match_range/3)."""

    def test_full_range(self):
        p = parse_date("1705-10-01:1710-10-01")
        assert p.type == "range"
        assert p.value == "17051001.17101001"
        assert p.date["subtype"] == "from_to"
        assert p.date["from"]["value"] == 17051001
        assert p.date["to"]["value"] == 17101001

    def test_open_ended_range(self):
        # from: (from_only)
        p = parse_date("1709-06-28:")
        assert p.type == "range"
        assert p.value == "17090628.1"
        assert p.date["subtype"] == "from_only"
        assert p.date["from"]["value"] == 17090628

    def test_open_start_range(self):
        # :to (to_only)
        p = parse_date(":1640")
        assert p.type == "range"
        assert p.value == "16399999.9"
        assert p.date["subtype"] == "to_only"
        assert p.date["to"]["value"] == 16400000

    def test_range_from_relative_edge(self):
        # >2025-01-27: (from_only with a relative 'from' edge)
        # From dates.cli line 9.
        p = parse_date(">2025-01-27:")
        assert p.type == "range"
        assert p.value == "20250127.1"
        assert p.date["subtype"] == "from_only"

    def test_range_until_relative_edge(self):
        # :<2025-02-03 (to_only with a relative 'to' edge)
        # From dates.cli line 10.
        p = parse_date(":<2025-02-03")
        assert p.type == "range"
        assert p.value == "20250202.9"
        assert p.date["subtype"] == "to_only"

    def test_range_both_relative_edges(self):
        # >20250128:<20250204 (from_to with both edges relative)
        # From dates.cli line 11.
        p = parse_date(">20250128:<20250204")
        assert p.type == "range"
        assert p.value == "20250128.20250204"
        assert p.date["subtype"] == "from_to"


# ---------------------------------------------------------------------------
# Sortable-value type recovery (issue #1)
# ---------------------------------------------------------------------------


class TestTypeRecoveryFromValue:
    """The date type can be inferred from value - round(value)."""

    @pytest.mark.parametrize(
        "value, expected",
        [
            ("15800000", 0.0),      # single
            ("15800000.3", 0.3),    # after
            ("16399999.7", -0.3),   # before (approx)
            ("15800000.1", 0.1),    # open-ended
            ("16399999.9", -0.1),   # open-start (approx)
        ],
    )
    def test_fractional_part_matches_type(self, value, expected):
        num = float(value)
        frac = round(num - round(num), 2)
        assert abs(frac - expected) < 0.01


# ---------------------------------------------------------------------------
# JSON serialization (date_extra_info)
# ---------------------------------------------------------------------------


class TestDateExtraInfoJson:
    """The date_extra_info element value."""

    def test_single_date_json(self):
        j = date_extra_info_json("1620")
        d = json.loads(j)
        assert d["original"] == "1620"
        assert d["type"] == "single"
        assert d["value"] == "16200000"
        assert d["date"]["subtype"] == "y"

    def test_relative_date_json(self):
        j = date_extra_info_json(">1580")
        d = json.loads(j)
        assert d["type"] == "relative"
        assert d["value"] == "15800000.3"
        assert d["date"]["subtype"] == "after"

    def test_range_date_json(self):
        j = date_extra_info_json("1705-10-01:1710-10-01")
        d = json.loads(j)
        assert d["type"] == "range"
        assert d["value"] == "17051001.17101001"
        assert d["date"]["subtype"] == "from_to"

    def test_empty_date_json(self):
        j = date_extra_info_json("")
        assert j == "{}"

    def test_malformed_date_json(self):
        j = date_extra_info_json("not-a-date")
        assert j == "{}"


# ---------------------------------------------------------------------------
# Error handling
# ---------------------------------------------------------------------------


class TestErrorHandling:
    """Malformed dates should not crash."""

    def test_garbage_returns_error(self):
        p = parse_date("not-a-date")
        assert p.type == "error"
        assert p.value == "0"
        assert p.date is None

    def test_none_input(self):
        p = parse_date(None)
        assert p.type == "error"
        assert p.value == "0"

    def test_empty_string(self):
        p = parse_date("")
        assert p.type == "error"
        assert p.value == "0"
        assert p.date is None


# ---------------------------------------------------------------------------
# Round-trip property: parse -> to_json_dict -> JSON
# ---------------------------------------------------------------------------


class TestRoundTrip:
    """Parsed dates should serialize cleanly to JSON and back."""

    @pytest.mark.parametrize(
        "text",
        ["1620", "1705-10-01", ">1580", "<1640", "1705-10-01:1710-10-01", "1709-06-28:"],
    )
    def test_json_roundtrip(self, text):
        p = parse_date(text)
        j = date_extra_info_json(text)
        d = json.loads(j)
        assert d["original"] == p.original
        assert d["type"] == p.type
        assert d["value"] == p.value
