"""Tests for the structure-file resolver (kleio.schema.resolver).

Mirrors the precedence of Prolog's get_stru_for_file / match_stru_to_file
predicates (src/apiTranslations.pl), ported to a YAML-only world.

Convention follows tests/test_api.py: a temp home_dir with sources/ and
structures/ created by hand, plus one real-fixture test against the on-disk
tests/kleio-home tree (skipped when absent).
"""
from __future__ import annotations

import tempfile
from pathlib import Path

import pytest

from kleio.config import KleioConfig
from kleio.schema.resolver import resolve_structure_for_source


TESTS_DIR = Path(__file__).parent
KLEIO_HOME = TESTS_DIR / "kleio-home"


# =============================================================================
# Fixtures
# =============================================================================

@pytest.fixture
def home_dir():
    """Yield a temp kleio-home with sources/ and structures/ created."""
    with tempfile.TemporaryDirectory() as tmpdir:
        home = Path(tmpdir)
        (home / "sources").mkdir(parents=True, exist_ok=True)
        (home / "structures").mkdir(parents=True, exist_ok=True)
        yield home


def _config(home: Path) -> KleioConfig:
    return KleioConfig(home_dir=home, admin_token="t", debug=True)


def _write_source(home: Path, rel_path: str, first_line: str = "") -> Path:
    """Create a source file under home/sources/<rel_path> with first_line."""
    src = home / "sources" / rel_path
    src.parent.mkdir(parents=True, exist_ok=True)
    src.write_text(first_line + "\n", encoding="utf-8")
    return src


def _touch(path: Path) -> Path:
    """Create an empty file (and parent dirs)."""
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("[]\n", encoding="utf-8")  # minimal valid YAML structure
    return path


class _TokenInfoStub:
    """Minimal stand-in for kleio.api.auth.TokenInfo.

    The resolver only reads the ``structures`` attribute (via getattr), so a
    lightweight stub avoids importing FastAPI-dependent auth code into a unit
    test of pure path logic.
    """

    def __init__(self, *, sources: str = "", structures: str = ""):
        self.sources = sources
        self.structures = structures


# =============================================================================
# Precedence 1: explicit override
# =============================================================================

class TestExplicitOverride:
    def test_override_returns_resolved_path_without_existence_check(self, home_dir):
        """Override is returned as-is; the route validates existence (HTTP 404)."""
        cfg = _config(home_dir)
        src = _write_source(home_dir, "d/x.cli")
        # Note: structures/x.yaml is NOT created; override skips existence check.
        result = resolve_structure_for_source(src, cfg, override="x.yaml")
        assert result == (home_dir / "structures" / "x.yaml").resolve()


# =============================================================================
# Precedence 2: kleio$spec directive on first line
# =============================================================================

class TestKleioDirective:
    def test_yaml_directive_resolves_relative_to_source_dir(self, home_dir):
        cfg = _config(home_dir)
        src = _write_source(
            home_dir, "d/x.cli", first_line="kleio$foo.yaml/prefix=lousa/translations=1"
        )
        _touch(src.parent / "foo.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == (src.parent / "foo.yaml").resolve()

    def test_str_directive_falls_through(self, home_dir):
        """A .str directive is skipped (loader can't read it); falls to matching."""
        cfg = _config(home_dir)
        src = _write_source(
            home_dir, "bapt/bapt1714.cli", first_line="kleio$gacto2.str/prefix=lousa"
        )
        # Make the lastdir rule (structures/<lastdir>.yaml) win to prove fall-through.
        _touch(home_dir / "structures" / "bapt.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == (home_dir / "structures" / "bapt.yaml").resolve()

    def test_directive_with_no_spec_falls_through(self, home_dir):
        """`kleio$/...` has an empty spec and should not resolve."""
        cfg = _config(home_dir)
        src = _write_source(home_dir, "d/x.cli", first_line="kleio$/translations=1")
        _touch(home_dir / "structures" / "sources-structure.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == (home_dir / "structures" / "sources-structure.yaml").resolve()

    def test_non_kleio_first_line_falls_through(self, home_dir):
        cfg = _config(home_dir)
        src = _write_source(home_dir, "d/x.cli", first_line="fonte$x/loc=coimbra")
        _touch(home_dir / "structures" / "sources-structure.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == (home_dir / "structures" / "sources-structure.yaml").resolve()


# =============================================================================
# Precedence 3: path/name matching
# =============================================================================

class TestPathMatching:
    def test_colocated_structure_yaml_wins(self, home_dir):
        """Clause 1: <stem>-structure.yaml co-located with the source."""
        cfg = _config(home_dir)
        src = _write_source(home_dir, "d/x.cli")
        colocated = _touch(src.parent / "x-structure.yaml")
        # Also place a default that should NOT win.
        _touch(home_dir / "structures" / "sources-structure.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == colocated.resolve()

    def test_mirrored_structures_by_basename(self, home_dir):
        """Clause 2: swap sources -> structures, keep subpath, <stem>.yaml."""
        cfg = _config(home_dir)
        src = _write_source(home_dir, "a/b/x.cli")
        mirror = _touch(home_dir / "structures" / "a" / "b" / "x.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == mirror.resolve()

    def test_mirrored_structures_str_yaml_alt(self, home_dir):
        """Clause 2 also tries <stem>.str.yaml."""
        cfg = _config(home_dir)
        src = _write_source(home_dir, "a/b/x.cli")
        mirror = _touch(home_dir / "structures" / "a" / "b" / "x.str.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == mirror.resolve()

    def test_lastdir_under_structures(self, home_dir):
        """Clauses 3/4: structures/<lastdir>.yaml directly under structures/.

        This is the Prolog test_case fact at apiTranslations.pl:769-770,
        ported: .../sources/.../baptismos/bapt1714.cli -> structures/baptismos.yaml.
        """
        cfg = _config(home_dir)
        src = _write_source(
            home_dir,
            "api/paroquiais/baptismos/bapt1714.cli",
            first_line="kleio$gacto2.str/prefix=lousa/translations=1",
        )
        lastdir_stru = _touch(home_dir / "structures" / "baptismos.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == lastdir_stru.resolve()

    def test_upward_walk_finds_sources_structure_in_source_dir(self, home_dir):
        """Clause 5: walk up the sources tree for sources-structure.yaml."""
        cfg = _config(home_dir)
        src = _write_source(home_dir, "proj/sub/deep/x.cli")
        ancestor = _touch(home_dir / "sources" / "proj" / "sources-structure.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == ancestor.resolve()

    def test_upward_walk_finds_at_home_root(self, home_dir):
        """Clause 5 walks all the way up to home_dir/sources/."""
        cfg = _config(home_dir)
        src = _write_source(home_dir, "proj/sub/deep/x.cli")
        ancestor = _touch(home_dir / "sources" / "sources-structure.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == ancestor.resolve()


# =============================================================================
# Precedence 4: default fallback
# =============================================================================

class TestDefaultFallback:
    def test_default_sources_structure_yaml(self, home_dir):
        cfg = _config(home_dir)
        src = _write_source(home_dir, "d/x.cli")
        default = _touch(home_dir / "structures" / "sources-structure.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == default.resolve()

    def test_default_gacto2_str_yaml_when_no_sources_structure(self, home_dir):
        cfg = _config(home_dir)
        src = _write_source(home_dir, "d/x.cli")
        default = _touch(home_dir / "structures" / "gacto2.str.yaml")
        result = resolve_structure_for_source(src, cfg)
        assert result == default.resolve()

    def test_default_honors_per_token_structures_dir(self, home_dir):
        """Default fallback uses the token's structures subdir when set.

        Mirrors resolve_structure_path in auth.py and the Prolog
        kleio_resolve_structure_file (option(structures(...))).
        """
        cfg = _config(home_dir)

        # Token-scoped structures dir: <home>/users/alice/structures/
        user_stru_dir = home_dir / "users" / "alice" / "structures"
        user_stru_dir.mkdir(parents=True, exist_ok=True)
        user_default = _touch(user_stru_dir / "sources-structure.yaml")

        # Also place a global default that must NOT win.
        _touch(home_dir / "structures" / "sources-structure.yaml")

        src = _write_source(home_dir, "d/x.cli")
        token = _TokenInfoStub(structures="users/alice/structures")
        result = resolve_structure_for_source(src, cfg, token_info=token)
        assert result == user_default.resolve()

    def test_default_ignores_token_structures_when_empty(self, home_dir):
        """An empty token.structures falls back to the global structures_dir."""
        cfg = _config(home_dir)
        src = _write_source(home_dir, "d/x.cli")
        global_default = _touch(home_dir / "structures" / "sources-structure.yaml")
        token = _TokenInfoStub(structures="")
        result = resolve_structure_for_source(src, cfg, token_info=token)
        assert result == global_default.resolve()


# =============================================================================
# Empty / no-match
# =============================================================================

class TestNoMatch:
    def test_returns_none_when_nothing_matches(self, home_dir):
        cfg = _config(home_dir)
        src = _write_source(home_dir, "d/x.cli")
        result = resolve_structure_for_source(src, cfg)
        assert result is None


# =============================================================================
# Real fixture test (mirrors Prolog get_stru test exactly)
# =============================================================================

class TestRealFixtures:
    def test_real_bapt1714_resolves_to_baptismos_yaml(self):
        """Port of the Prolog test_case/2 fact at apiTranslations.pl:769-770.

        tests/kleio-home/sources/api/paroquiais/baptismos/bapt1714.cli
            -> tests/kleio-home/structures/baptismos.yaml
        """
        src = KLEIO_HOME / "sources" / "api" / "paroquiais" / "baptismos" / "bapt1714.cli"
        expected = KLEIO_HOME / "structures" / "baptismos.yaml"
        if not src.exists() or not expected.exists():
            pytest.skip("bapt1714.cli or baptismos.yaml fixture not present")

        cfg = KleioConfig(home_dir=KLEIO_HOME)
        result = resolve_structure_for_source(src, cfg)
        assert result is not None
        assert result.resolve() == expected.resolve()
