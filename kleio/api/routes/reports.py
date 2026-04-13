"""Report endpoints.

This module provides REST endpoints for retrieving translation reports.
"""
from __future__ import annotations

from pathlib import Path

from fastapi import APIRouter, Depends, HTTPException, Request
from fastapi.responses import FileResponse

from kleio.api.auth import (
    TokenInfo,
    require_auth,
    require_permission,
    resolve_source_path,
)
from kleio.config import KleioConfig

router = APIRouter()


@router.get("/{path:path}.rpt")
async def get_report(
    path: str,
    request: Request,
    token_info: TokenInfo = Depends(require_permission("sources"))
):
    """Get translation report file.
    
    Returns the .rpt report file from translation.
    
    Requires 'sources' permission.
    """
    config: KleioConfig = request.app.state.config
    source_path = resolve_source_path(path, token_info, config)
    rpt_path = source_path.with_suffix(".rpt")
    
    if not rpt_path.exists():
        raise HTTPException(404, f"Report not found: {path}.rpt")
    
    return FileResponse(
        rpt_path,
        media_type="text/plain",
        filename=rpt_path.name
    )


@router.get("/{path:path}.err")
async def get_error_report(
    path: str,
    request: Request,
    token_info: TokenInfo = Depends(require_permission("sources"))
):
    """Get error report file.
    
    Returns the .err error file from translation.
    
    Requires 'sources' permission.
    """
    config: KleioConfig = request.app.state.config
    source_path = resolve_source_path(path, token_info, config)
    err_path = source_path.with_suffix(".err")
    
    if not err_path.exists():
        raise HTTPException(404, f"Error report not found: {path}.err")
    
    return FileResponse(
        err_path,
        media_type="text/plain",
        filename=err_path.name
    )


@router.get("/{path:path}")
async def get_translation_reports(
    path: str,
    request: Request,
    token_info: TokenInfo = Depends(require_permission("sources"))
) -> dict:
    """Get all reports for a translation.
    
    Returns information about available report files.
    
    Requires 'sources' permission.
    """
    config: KleioConfig = request.app.state.config
    source_path = resolve_source_path(path, token_info, config)
    
    reports = {}
    
    rpt_path = source_path.with_suffix(".rpt")
    if rpt_path.exists():
        reports["rpt"] = {
            "path": str(rpt_path),
            "size": rpt_path.stat().st_size,
            "exists": True
        }
    
    err_path = source_path.with_suffix(".err")
    if err_path.exists():
        reports["err"] = {
            "path": str(err_path),
            "size": err_path.stat().st_size,
            "exists": True
        }
    
    return {
        "source": path,
        "reports": reports
    }
