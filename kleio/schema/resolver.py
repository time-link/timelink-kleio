"""Structure file resolution for Kleio source files.

This module ports the per-source-file structure-resolution logic from the
Prolog ``get_stru_for_file`` / ``match_stru_to_file`` predicates
(``src/apiTranslations.pl``) so that a translation request without an explicit
``structure`` parameter picks the same structure file the Prolog server would,
based on file-path conventions.

The resolution precedence (highest to lowest):

1. **Explicit override** -- the ``structure`` request parameter.
2. **``kleio$spec`` directive** on the first line of the source file.
3. **Path/name-based matching** -- co-located ``-structure.yaml``, mirrored
   ``structures/`` paths, last-directory under ``structures/``, and an upward
   walk for ``sources-structure.yaml`` / ``gacto2.str.yaml``.
4. **Default fallback** -- ``structures/sources-structure.yaml`` then
   ``structures/gacto2.str.yaml``.

Divergences from the Prolog implementation (YAML-only port):

- The Python loader reads YAML exclusively; bare ``.str`` candidates from the
  Prolog rules are replaced by their modern ``.yaml`` / ``.str.yaml``
  equivalents. ``.str`` is never returned (the loader cannot parse it).
- ``sources.str`` (legacy) is replaced by ``sources-structure.yaml``.
- Path components are computed relative to ``home_dir`` instead of the
  Prolog representation that splits absolute paths (with a leading empty atom).
- The ``KLEIO_DEFAULT_STRU`` environment variable and ``kleio_stru_dir``
  search from ``kleioFiles.pl`` are not honored; the default fallback uses
  ``config.structures_dir`` (current Python behavior).
- Directive handling is simplified: the Prolog alias engine
  (``create_str_path`` with ``system`` / ``structures`` / ``sources`` / ``home``
  / ``~`` prefixes) is replaced by source-directory-relative resolution for
  YAML specs and a skip for ``.str`` specs.
"""
from __future__ import annotations

from pathlib import Path
from typing import TYPE_CHECKING, Optional

if TYPE_CHECKING:
    from kleio.api.auth import TokenInfo
    from kleio.config import KleioConfig


# Candidate filenames for the upward-walk and default-fallback rules.
# In Prolog these are sources-structure.yaml / sources.str / gacto2.str;
# here sources.str is dropped (legacy) and gacto2.str becomes gacto2.str.yaml
# (the YAML conversion the Python loader can read).
_ANCESTOR_CANDIDATES: tuple[str, ...] = (
    "sources-structure.yaml",
    "gacto2.str.yaml",
)

# Default fallback candidates checked directly under structures_dir.
_DEFAULT_CANDIDATES: tuple[str, ...] = _ANCESTOR_CANDIDATES


def resolve_structure_for_source(
    source_path: Path,
    config: "KleioConfig",
    token_info: "Optional[TokenInfo]" = None,
    override: "Optional[str]" = None,
) -> Optional[Path]:
    """Resolve the structure file for a given source file.

    Mirrors the precedence of Prolog's ``get_stru_for_file``: an explicit
    override wins, then the ``kleio$`` directive in the source, then
    path/name conventions, then a default fallback.

    Args:
        source_path: Absolute path to the ``.cli`` source file.
        config: Server configuration (provides ``home_dir``,
            ``sources_dir``, ``structures_dir``).
        token_info: Optional token info; if it carries a ``structures``
            subdir, that directory is used as the structures base instead of
            ``config.structures_dir``. Currently only consulted for the
            default-fallback branch.
        override: Optional explicit structure reference (the ``structure``
            request parameter). When given, it is resolved against the
            structures base and returned without an existence check -- the
            caller is responsible for raising on a missing file, preserving
            the existing 404 behavior of the route.

    Returns:
        The resolved (canonical) structure path, or ``None`` if no candidate
        exists.
    """
    # Canonicalize the source path so all derived paths are comparable
    # regardless of symlinks (e.g. /var -> /private/var on macOS).
    source_path = Path(source_path).resolve()

    # 1. Explicit override. The route already validates existence and raises
    #    HTTP 404 when body.structure is set, so when an override reaches us
    #    we just resolve the base and return it.
    if override:
        return (_structures_base(config, token_info) / override).resolve()

    # 2. kleio$spec directive on the first line of the source.
    directive_path = _resolve_directive(source_path)
    if directive_path is not None:
        return directive_path

    # 3. Path/name-based matching.
    matched = _match_stru_to_file(source_path, config)
    if matched is not None:
        return matched.resolve()

    # 4. Default fallback under structures_dir.
    base = _structures_base(config, token_info)
    for candidate in _DEFAULT_CANDIDATES:
        candidate_path = base / candidate
        if candidate_path.exists():
            return candidate_path.resolve()

    return None


# -----------------------------------------------------------------------------
# Directive parsing (Prolog get_stru_for_file clause 1)
# -----------------------------------------------------------------------------

def _resolve_directive(source_path: Path) -> Optional[Path]:
    """Parse the ``kleio$spec`` directive on the first line, if present.

    A directive looks like ``kleio$gacto2.str/prefix=lousa/translations=1``:
    the part after ``kleio$`` and before the first ``/`` is the structure
    reference. YAML specs (``.yaml`` / ``.yml``) are resolved relative to the
    source file's directory; ``.str`` specs are skipped (the loader cannot
    read them); bare names get ``.yaml`` appended.

    Returns the resolved existing path, or ``None`` to fall through to the
    path/name matching rules.
    """
    try:
        with open(source_path, "r", encoding="utf-8", errors="replace") as f:
            first_line = f.readline()
    except OSError:
        return None

    spec = _extract_kleio_spec(first_line)
    if spec is None:
        return None

    source_dir = source_path.parent

    if spec.endswith((".yaml", ".yml")):
        # Prolog's yaml_file-relative branch: anchor on the source directory.
        candidate = (source_dir / spec).resolve()
        return candidate if candidate.exists() else None

    if spec.endswith(".str"):
        # Legacy Prolog structure file -- the Python loader cannot parse it.
        # Fall through to path/name matching (matches the observed behavior
        # for bapt1714.cli, whose directive is kleio$gacto2.str/...).
        return None

    # Bare name: try <sourcedir>/<spec>.yaml.
    candidate = (source_dir / f"{spec}.yaml").resolve()
    return candidate if candidate.exists() else None


def _extract_kleio_spec(line: str) -> Optional[str]:
    """Extract the structure spec from a ``kleio$spec/...`` first line.

    The Prolog parser splits the line on ``/`` and takes the first token,
    then splits that token on ``$`` and keeps the part after ``kleio``.
    Returns ``""`` when the directive is present but has no spec
    (``kleio$/...``); returns ``None`` when the line is not a kleio directive.
    """
    first_token = line.split("/", 1)[0].strip()
    if "$" not in first_token:
        return None
    head, _, tail = first_token.partition("$")
    if head != "kleio":
        return None
    return tail


# -----------------------------------------------------------------------------
# Path/name matching (Prolog match_stru_to_file, clauses 1-6)
# -----------------------------------------------------------------------------

def _match_stru_to_file(source_path: Path, config: "KleioConfig") -> Optional[Path]:
    """Try the path/name-based structure-file conventions in Prolog order."""
    stem = source_path.stem
    source_dir = source_path.parent

    # Clause 1: co-located <stem>-structure.yaml in the source's own directory.
    candidate = source_dir / f"{stem}-structure.yaml"
    if candidate.exists():
        return candidate

    # Compute home-relative components for the clauses below.
    dirs = _relative_dirs(source_dir, config)

    # Clauses 3 & 4 need the last path component (the directory containing the
    # source file). When the source lives at home_dir/<lastdir>/file.cli we
    # try structures/<lastdir>.yaml (and .str.yaml).
    last_dir = source_dir.name

    # Clauses 2-4 only make sense if the source is inside a sources/ tree
    # (the Prolog rules key off the 'sources' atom). If it isn't, skip
    # straight to the upward walk.
    if "sources" in dirs:
        # Clause 2: swap the first 'sources' -> 'structures' and keep the
        # rest of the path, looking for <stem>.yaml / <stem>.str.yaml.
        mirror_dirs = _swap_first(dirs, "sources", "structures")
        mirror_dir = _join_under_home(mirror_dirs, config)
        for name in (f"{stem}.yaml", f"{stem}.str.yaml"):
            candidate = mirror_dir / name
            if candidate.exists():
                return candidate

        # Clauses 3 & 4: structures/<lastdir>.yaml / .str.yaml directly under
        # the structures root (intermediate subpath discarded, matching the
        # Prolog 'append(_, [structures|PathToStructures], ...)' behavior when
        # 'sources' sits near the top of the path).
        structures_root = _structures_root(config)
        for name in (f"{last_dir}.yaml", f"{last_dir}.str.yaml"):
            candidate = structures_root / name
            if candidate.exists():
                return candidate

    # Clauses 5 & 6: walk up from the source directory toward home_dir, and
    # at each ancestor try sources-structure.yaml / gacto2.str.yaml. Prolog
    # walks both the sources tree and the structures tree; here the structures
    # tree is covered by the default fallback in resolve_structure_for_source,
    # so we only need the sources-side walk.
    matched = _walk_up_for_ancestors(source_dir, config)
    if matched is not None:
        return matched

    return None


def _walk_up_for_ancestors(source_dir: Path, config: "KleioConfig") -> Optional[Path]:
    """Walk up from ``source_dir`` toward ``home_dir`` trying ancestor files.

    At each level (deepest first, including ``source_dir`` itself and stopping
    at ``home_dir`` inclusive) tries ``sources-structure.yaml`` then
    ``gacto2.str.yaml``.
    """
    home = config.home_dir.resolve()
    try:
        source_dir.relative_to(home)
    except ValueError:
        # Source is outside home_dir: nothing to walk.
        return None

    current = source_dir.resolve()
    while True:
        for name in _ANCESTOR_CANDIDATES:
            candidate = current / name
            if candidate.exists():
                return candidate
        if current == home:
            break
        current = current.parent
    return None


# -----------------------------------------------------------------------------
# Path helpers
# -----------------------------------------------------------------------------

def _relative_dirs(source_dir: Path, config: "KleioConfig") -> list[str]:
    """Return the home_dir-relative path components of ``source_dir``.

    For ``<home>/sources/api/paroquiais/baptismos`` returns
    ``['sources', 'api', 'paroquiais', 'baptismos']``. If the source is
    outside ``home_dir``, returns an empty list.

    Unlike the Prolog port, no leading empty string is produced: we operate
    on the relative path, which is cleaner and behavior-equivalent for the
    swap/last-component operations.
    """
    home = config.home_dir.resolve()
    try:
        rel = source_dir.resolve().relative_to(home)
    except ValueError:
        return []
    return [p for p in rel.parts if p]


def _swap_first(dirs: list[str], old: str, new: str) -> list[str]:
    """Return a copy of ``dirs`` with the first ``old`` replaced by ``new``.

    Mirrors SWI-Prolog ``select/4`` semantics: only the first occurrence is
    replaced. Returns ``dirs`` unchanged when ``old`` is not present.
    """
    out = list(dirs)
    for i, d in enumerate(out):
        if d == old:
            out[i] = new
            break
    return out


def _join_under_home(dirs: list[str], config: "KleioConfig") -> Path:
    """Join ``home_dir`` with the given relative components into a Path."""
    base = config.home_dir
    for d in dirs:
        base = base / d
    return base


def _structures_root(config: "KleioConfig") -> Path:
    """The structures root used by clauses 3 & 4 (always structures_dir).

    Per-token ``structures`` subdirs are not consulted for these clauses
    because the path-mirroring rules are defined relative to the global
    structures tree, not a user-specific one.
    """
    return config.structures_dir


def _structures_base(
    config: "KleioConfig",
    token_info: "Optional[TokenInfo]",
) -> Path:
    """Structures base for the default fallback and explicit overrides.

    Honors a per-token ``structures`` subdir when present (mirrors
    ``resolve_structure_path`` in ``kleio/api/auth.py``), else falls back to
    ``config.structures_dir``.
    """
    if token_info is not None and getattr(token_info, "structures", None):
        return config.home_dir / token_info.structures
    return config.structures_dir
