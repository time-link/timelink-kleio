"""Promote a ``.ids`` pretty-printed file to ``.cli`` on successful translation.

Ports the ``rename_files/4`` predicate from ``gactoxml.pl`` (lines 264-317).
The end state, after a successful (no-error) translation, is:

* ``.cli`` — the last pretty-printed translation with explicit ids.
* ``.old`` — the previous ``.cli`` that was translated (kept as a backup).
* ``.org`` — the original ``.cli`` as first translated (kept forever as a
  safety net).

This stability is what lets the database be re-imported safely after edits:
the explicit ids in the promoted ``.cli`` pin every entity, so a re-import
updates rows instead of creating duplicates.
"""
from __future__ import annotations

import logging
import shutil
from pathlib import Path

logger = logging.getLogger(__name__)


def promote_ids_on_success(
    source_path: Path,
    error_count: int,
) -> dict[str, str]:
    """Promote ``.ids`` to ``.cli`` when the translation had no errors.

    Mirrors ``rename_files/4`` in gactoxml.pl:264-317:

    1. If a ``.old`` exists, delete it (it's the previous-previous version).
    2. If a ``.org`` exists (i.e. this is not the first successful
       translation): rename ``.cli`` → ``.old``.
       Otherwise (first successful translation): rename ``.cli`` → ``.org``.
    3. Rename ``.ids`` → ``.cli``.

    Args:
        source_path: Path to the source ``.cli`` file that was translated.
            The ``.ids``/``.org``/``.old`` paths are derived from it.
        error_count: Number of errors from the translation. When > 0 the
            rename is skipped entirely (matching gactoxml.pl:221).

    Returns:
        A dict describing the operations performed, with keys describing
        each step (e.g. ``{"cli_to": ".../x.old", "ids_to": ".../x.cli"}``).
        Empty when nothing was done.
    """
    source_path = Path(source_path)
    cli_path = source_path
    ids_path = source_path.with_suffix(".ids")
    org_path = source_path.with_suffix(".org")
    old_path = source_path.with_suffix(".old")

    result: dict[str, str] = {}

    if error_count > 0:
        logger.info(
            "Skipping .ids promotion for %s: translation had %d error(s)",
            cli_path, error_count,
        )
        return result

    if not ids_path.exists():
        logger.warning(
            "Cannot promote .ids for %s: .ids file not found at %s",
            cli_path, ids_path,
        )
        return result

    # Step 1: delete the previous .old if any.
    if old_path.exists():
        try:
            old_path.unlink()
            result["deleted_old"] = str(old_path)
        except OSError as e:
            logger.warning("Could not delete previous .old %s: %s", old_path, e)

    # Step 2: rename .cli -> .org (first run) or .cli -> .old (subsequent).
    if cli_path.exists():
        if org_path.exists():
            target = old_path
            target_label = "old"
        else:
            target = org_path
            target_label = "org"
        _rename_safe(cli_path, target)
        result["cli_to"] = str(target)
        result["cli_target_kind"] = target_label

    # Step 3: rename .ids -> .cli.
    _rename_safe(ids_path, cli_path)
    result["ids_to"] = str(cli_path)

    logger.info("Promoted %s -> %s; result: %s", ids_path, cli_path, result)
    return result


def _rename_safe(src: Path, dst: Path) -> None:
    """Rename ``src`` to ``dst``, falling back to copy+delete.

    Mirrors rename_with_shell/2 in gactoxml.pl:319-325, which falls back to
    a shell ``cp -p`` + ``rm`` to work around a VirtualBox-on-Windows bug
    with cross-device renames. We use ``pathlib.Path.rename`` first and
    fall back to ``shutil.copy2`` + ``unlink`` on ``OSError`` (which
    includes cross-device errors).
    """
    try:
        src.rename(dst)
    except OSError as e:
        logger.debug(
            "rename %s -> %s failed (%s); falling back to copy+delete",
            src, dst, e,
        )
        shutil.copy2(src, dst)
        src.unlink()
