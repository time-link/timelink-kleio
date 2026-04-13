"""Export file retrieval endpoints.

This module provides REST endpoints for retrieving translation outputs.
"""
from __future__ import annotations

from pathlib import Path
from typing import Optional

from fastapi import APIRouter, Depends, HTTPException, Request
from fastapi.responses import FileResponse, PlainTextResponse

from kleio.api.auth import (
    TokenInfo,
    require_auth,
    require_permission,
    resolve_source_path,
)
from kleio.config import KleioConfig

router = APIRouter()


def _get_export_path(
    path: str,
    token_info: TokenInfo,
    config: KleioConfig,
    extension: str
) -> Path:
    """Get path to an export file.
    
    Args:
        path: Relative path to the source file.
        token_info: Token info with user's directory info.
        config: Server configuration.
        extension: File extension for the export file.
        
    Returns:
        Path to the export file.
    """
    source_path = resolve_source_path(path, token_info, config)
    return source_path.with_suffix(extension)


@router.get("/{path:path}.xml")
async def get_xml_export(
    path: str,
    request: Request,
    token_info: TokenInfo = Depends(require_permission("sources"))
):
    """Get XML export file.
    
    Returns the XML output from translation.
    
    Requires 'sources' permission.
    """
    config: KleioConfig = request.app.state.config
    xml_path = _get_export_path(path, token_info, config, ".xml")
    
    if not xml_path.exists():
        raise HTTPException(404, f"XML export not found: {path}.xml")
    
    return FileResponse(
        xml_path,
        media_type="application/xml",
        filename=xml_path.name
    )


@router.get("/{path:path}.ids")
async def get_ids_export(
    path: str,
    request: Request,
    token_info: TokenInfo = Depends(require_permission("sources"))
):
    """Get IDs export file.
    
    Returns the ID mapping file from translation.
    
    Requires 'sources' permission.
    """
    config: KleioConfig = request.app.state.config
    ids_path = _get_export_path(path, token_info, config, ".ids")
    
    if not ids_path.exists():
        raise HTTPException(404, f"IDS export not found: {path}.ids")
    
    return FileResponse(
        ids_path,
        media_type="text/plain",
        filename=ids_path.name
    )


@router.get("/{path:path}.json")
async def get_json_export(
    path: str,
    request: Request,
    token_info: TokenInfo = Depends(require_permission("sources"))
):
    """Get JSON export file.
    
    Returns the JSON metadata file from translation.
    
    Requires 'sources' permission.
    """
    config: KleioConfig = request.app.state.config
    json_path = _get_export_path(path, token_info, config, ".files.json")
    
    if not json_path.exists():
        raise HTTPException(404, f"JSON export not found: {path}.files.json")
    
    return FileResponse(
        json_path,
        media_type="application/json",
        filename=json_path.name
    )


@router.get("/{path:path}")
async def get_export(
    path: str,
    request: Request,
    format: str = "xml",
    token_info: TokenInfo = Depends(require_permission("sources"))
):
    """Get export file in specified format.
    
    Query params:
        format: Output format (xml, json, ids)
    
    Requires 'sources' permission.
    """
    config: KleioConfig = request.app.state.config
    
    if format == "xml":
        export_path = _get_export_path(path, token_info, config, ".xml")
        media_type = "application/xml"
    elif format == "json":
        export_path = _get_export_path(path, token_info, config, ".files.json")
        media_type = "application/json"
    elif format == "ids":
        export_path = _get_export_path(path, token_info, config, ".ids")
        media_type = "text/plain"
    else:
        raise HTTPException(400, f"Invalid format: {format}")
    
    if not export_path.exists():
        raise HTTPException(404, f"Export not found: {path} ({format})")
    
    return FileResponse(
        export_path,
        media_type=media_type,
        filename=export_path.name
    )
