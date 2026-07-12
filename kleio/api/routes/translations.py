"""Translation API endpoints.

This module provides REST endpoints for translating Kleio source files.
"""
from __future__ import annotations

import asyncio
import logging
from datetime import datetime
from pathlib import Path
from typing import Optional

from fastapi import APIRouter, BackgroundTasks, Depends, HTTPException, Request
from pydantic import BaseModel

from kleio.api.auth import (
    TokenInfo,
    require_auth,
    require_permission,
    resolve_source_path,
    resolve_structure_path,
)
from kleio.config import KleioConfig

logger = logging.getLogger(__name__)

router = APIRouter()


# Background task tracking
_translation_tasks: dict[str, dict] = {}
_task_counter = 0


class TranslateRequest(BaseModel):
    """Request to translate a source file."""
    path: str
    structure: str = ""
    echo: str = "no"  # yes/no for verbose output


class TranslationJob(BaseModel):
    """Status of a translation job."""
    job_id: str
    status: str  # "queued", "processing", "completed", "error"
    source: str
    structure: str = ""
    message: str = ""
    errors: int = 0
    warnings: int = 0
    created: str = ""
    updated: str = ""


class TranslationResult(BaseModel):
    """Result of a completed translation."""
    status: str
    source: str
    xml_file: str = ""
    errors: int = 0
    warnings: int = 0
    message: str = ""


def _generate_job_id() -> str:
    """Generate a unique job ID."""
    global _task_counter
    _task_counter += 1
    return f"job-{datetime.now().strftime('%Y%m%d%H%M%S')}-{_task_counter}"


async def _do_translation(
    job_id: str,
    source_path: Path,
    structure_path: Path | None,
    output_dir: Path,
    config: KleioConfig,
    token_info: TokenInfo | None,
    echo: bool = False,
) -> None:
    """Execute translation in background.

    Args:
        job_id: The job ID for tracking.
        source_path: Absolute path to the source file.
        structure_path: Optional path to structure file (already resolved and
            token-scoped by the caller when ``body.structure`` was supplied).
        output_dir: Directory for output files.
        config: Server configuration.
        token_info: The caller's token info. Only consulted by the structure
            resolver's default-fallback branch (per-token ``structures`` dir);
            path-mirroring clauses use the global structures root, matching the
            Prolog behavior. May be ``None`` for unauthenticated/internal calls.
        echo: If True, echo every source line into the .rpt report file.
    """
    from kleio.schema.registry import SchemaRegistry
    from kleio.parser.builder import translate_file
    from kleio.export.xml_exporter import XmlExporter
    from kleio.export.report_writer import ReportWriter
    from kleio.export.cliopp import ClioPrettyPrinter
    from kleio.export.rename import promote_ids_on_success
    from kleio.inference.engine import InferenceEngine
    from kleio.inference.rules import get_default_rules
    from kleio.mappings import get_default_mapping_store
    from kleio.errors import ErrorAccumulator

    task_info = _translation_tasks[job_id]
    task_info["status"] = "processing"
    task_info["updated"] = datetime.now().isoformat()

    try:
        # Initialize error accumulator
        errors = ErrorAccumulator(max_errors=config.max_errors)

        # Load schema
        schema = SchemaRegistry()
        if structure_path and structure_path.exists():
            schema.load(structure_path, errors)
        else:
            # No explicit structure was supplied: resolve one from the source
            # file using the Prolog get_stru_for_file precedence (directive
            # in the source, then path/name conventions, then default). The
            # default fallback honors a per-token structures dir; the
            # path-mirroring clauses use the global structures root (matching
            # Prolog, which hardcodes the sources->structures swap).
            from kleio.schema.resolver import resolve_structure_for_source
            resolved = resolve_structure_for_source(
                source_path, config, token_info=token_info, override=None
            )
            if resolved is None or not resolved.exists():
                raise ValueError("No structure file available")
            schema.load(resolved, errors)
            structure_path = resolved

        # Create report writer (.rpt/.err). It collects group markers and,
        # when echo=True, an echo of every source line. The schema is passed so
        # that, with echo off, only act-inheriting groups are echoed (matching
        # the Prolog report's historical_act_export behaviour).
        report = ReportWriter(
            source_file=str(source_path),
            output_dir=output_dir,
            errors=errors,
            echo=echo,
            schema=schema,
            structure_file=str(structure_path) if structure_path else "",
        )

        # Create exporter with the default mapping store (ports mappings.pl).
        # The mapping store resolves group→database-class (e.g. historical-source
        # → source) and provides class definitions for <CLASS> block emission.
        mapping_store = get_default_mapping_store()

        exporter = XmlExporter()
        exporter.init(
            source_file=str(source_path),
            output_dir=output_dir,
            schema=schema,
            structure_file=str(structure_path) if structure_path else "",
            mapping_store=mapping_store,
        )

        # Create the .ids pretty-printer (ports clioPP.pl). It receives each
        # completed group and writes a copy of the source with explicit ids
        # to <source>.ids, in the same directory as the .xml/.rpt outputs.
        cliopp = ClioPrettyPrinter(
            source_file=str(source_path),
            output_dir=output_dir,
            schema=schema,
        )

        # Translate file. The XML exporter, report writer and .ids printer
        # all receive group-completion callbacks; the report writer also
        # receives per-line callbacks for the optional echo.
        def _on_group(group):
            exporter.export_group(group)
            report.on_group(group)
            cliopp.on_group(group)

        groups = translate_file(
            source_path,
            schema,
            errors,
            on_group=_on_group,
            on_line=report.on_line,
        )

        # Apply inference rules if available
        inference_results = None
        try:
            engine = InferenceEngine()
            rules_path = config.home_dir / "inferences" / "default_rules.yaml"
            if rules_path.exists():
                engine.load_rules_from_yaml(rules_path)
            else:
                # Use default rules
                for rule in get_default_rules():
                    engine.register_rule(rule)

            if engine.get_rules():
                inference_results = engine.apply_rules(groups, schema.structure)
        except Exception as e:
            logger.warning(f"Inference failed: {e}")

        # Close exporter (writes .xml + .files.json)
        output_files = exporter.close(inference_results)

        # Close report writer (writes .rpt + .err)
        output_files.extend(report.close())

        # Close the .ids pretty-printer (writes <source>.ids)
        output_files.extend(cliopp.close())

        # On a successful translation (no errors), promote the .ids to .cli
        # and preserve the originals as .org/.old (ports rename_files/4 in
        # gactoxml.pl:264-317). This is what keeps database imports stable
        # across re-translations: the explicit ids pin every entity.
        rename_result = promote_ids_on_success(source_path, errors.error_count)
        if rename_result:
            task_info["rename"] = rename_result

        # Update task info
        task_info["status"] = "completed"
        task_info["errors"] = errors.error_count
        task_info["warnings"] = errors.warning_count
        task_info["output_files"] = output_files
        task_info["message"] = f"Translated {len(groups)} groups"

    except Exception as e:
        logger.error(f"Translation failed: {e}")
        task_info["status"] = "error"
        task_info["message"] = str(e)

    finally:
        task_info["updated"] = datetime.now().isoformat()


@router.post("", response_model=dict)
async def start_translation(
    request: Request,
    body: TranslateRequest,
    background_tasks: BackgroundTasks,
    token_info: TokenInfo = Depends(require_permission("translations"))
) -> dict:
    """Start translation of a source file.
    
    Triggers asynchronous translation and returns a job ID for tracking.
    
    Requires 'translations' permission.
    """
    config: KleioConfig = request.app.state.config
    
    # Resolve source path
    source_path = resolve_source_path(body.path, token_info, config)
    
    if not source_path.exists():
        raise HTTPException(404, f"Source file not found: {body.path}")
    
    if not source_path.is_file():
        raise HTTPException(400, f"Path is not a file: {body.path}")
    
    # Resolve structure path if specified
    structure_path = None
    if body.structure:
        structure_path = resolve_structure_path(body.structure, token_info, config)
        if not structure_path.exists():
            raise HTTPException(404, f"Structure file not found: {body.structure}")
    
    # Create output directory (same as source directory)
    output_dir = source_path.parent
    
    # Generate job ID
    job_id = _generate_job_id()
    
    # Track the task
    _translation_tasks[job_id] = {
        "job_id": job_id,
        "status": "queued",
        "source": body.path,
        "structure": body.structure,
        "created": datetime.now().isoformat(),
        "updated": datetime.now().isoformat(),
    }
    
    # Queue translation as background task. The echo flag controls whether the
    # .rpt report echoes every source line (echo=yes) or only group markers and
    # diagnostics (echo=no, the default).
    echo_flag = str(body.echo).strip().lower() in ("yes", "true", "1")
    background_tasks.add_task(
        _do_translation,
        job_id,
        source_path,
        structure_path,
        output_dir,
        config,
        token_info,
        echo_flag,
    )
    
    return {
        "status": "OK",
        "job": {
            "method": "translations",
            "object": body.path,
            "job_id": job_id
        }
    }


@router.get("/{path:path}")
async def get_translation_status(
    path: str,
    request: Request,
    token_info: TokenInfo = Depends(require_auth)
) -> dict:
    """Get translation status for a file.
    
    Returns the current translation status and any output files.
    """
    config: KleioConfig = request.app.state.config
    
    # Resolve source path
    source_path = resolve_source_path(path, token_info, config)
    
    # Check for existing output files
    xml_path = source_path.with_suffix(".xml")
    files_json_path = source_path.with_suffix(".files.json")
    
    result = {
        "source": path,
        "status": "not_translated",
        "xml_exists": xml_path.exists(),
        "xml_file": str(xml_path.relative_to(config.sources_dir)) if xml_path.exists() else None,
    }
    
    if xml_path.exists():
        result["status"] = "completed"
        result["xml_file"] = str(xml_path)
        result["last_modified"] = datetime.fromtimestamp(
            xml_path.stat().st_mtime
        ).isoformat()
    
    # Check for ongoing job
    for job_id, task in _translation_tasks.items():
        if task.get("source") == path:
            result["job"] = {
                "job_id": job_id,
                "status": task["status"],
                "message": task.get("message", "")
            }
            break
    
    return result


@router.delete("/{path:path}")
async def delete_translation(
    path: str,
    request: Request,
    recurse: str = "no",
    token_info: TokenInfo = Depends(require_permission("delete"))
) -> dict:
    """Delete translation outputs.
    
    Removes XML, IDS, and other generated files.
    
    Requires 'delete' permission.
    """
    config: KleioConfig = request.app.state.config
    
    # Resolve source path
    source_path = resolve_source_path(path, token_info, config)
    
    deleted = []
    
    if source_path.is_file():
        # Delete single file outputs
        for ext in [".xml", ".ids", ".rpt", ".err", ".files.json"]:
            output_path = source_path.with_suffix(ext)
            if output_path.exists():
                output_path.unlink()
                deleted.append(str(output_path))
    
    elif source_path.is_dir() and recurse == "yes":
        # Delete all translation outputs in directory tree
        for pattern in ["*.xml", "*.ids", "*.rpt", "*.err", "*.files.json"]:
            for output_path in source_path.rglob(pattern):
                output_path.unlink()
                deleted.append(str(output_path))
    
    return {
        "status": "OK",
        "deleted": deleted
    }


@router.get("")
async def list_translations(
    request: Request,
    token_info: TokenInfo = Depends(require_auth)
) -> dict:
    """List all current translation jobs."""
    jobs = []
    for job_id, task in _translation_tasks.items():
        jobs.append({
            "job_id": job_id,
            "status": task["status"],
            "source": task.get("source", ""),
            "message": task.get("message", ""),
            "created": task.get("created", ""),
            "updated": task.get("updated", "")
        })
    
    return {"jobs": jobs}
