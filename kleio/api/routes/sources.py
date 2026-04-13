"""Source file management endpoints.

This module provides REST endpoints for managing Kleio source files.
"""
from __future__ import annotations

import shutil
from datetime import datetime
from pathlib import Path
from typing import Optional

from fastapi import APIRouter, Depends, File, HTTPException, Request, UploadFile
from fastapi.responses import FileResponse
from pydantic import BaseModel

from kleio.api.auth import (
    TokenInfo,
    require_auth,
    require_permission,
    resolve_source_path,
)
from kleio.config import KleioConfig

router = APIRouter()


class SourceInfo(BaseModel):
    """Information about a source file."""
    path: str
    name: str
    is_file: bool
    is_directory: bool
    size: int = 0
    modified: str = ""
    mime_type: str = ""


class SourceList(BaseModel):
    """List of source files."""
    path: str
    files: list[SourceInfo]


def _get_mime_type(path: Path) -> str:
    """Get MIME type for a file."""
    ext = path.suffix.lower()
    mime_types = {
        ".cli": "text/x-kleio-cli",
        ".kleio": "text/x-kleio",
        ".str": "text/x-kleio-str",
        ".yaml": "text/yaml",
        ".yml": "text/yaml",
        ".xml": "application/xml",
        ".txt": "text/plain",
        ".json": "application/json",
    }
    return mime_types.get(ext, "application/octet-stream")


def _list_directory(
    base_path: Path,
    rel_path: str,
    recurse: bool = False
) -> list[SourceInfo]:
    """List contents of a directory."""
    result = []
    
    if not base_path.exists():
        return result
    
    if base_path.is_file():
        stat = base_path.stat()
        return [SourceInfo(
            path=rel_path,
            name=base_path.name,
            is_file=True,
            is_directory=False,
            size=stat.st_size,
            modified=datetime.fromtimestamp(stat.st_mtime).isoformat(),
            mime_type=_get_mime_type(base_path)
        )]
    
    # List directory contents
    try:
        for item in sorted(base_path.iterdir()):
            item_rel = f"{rel_path}/{item.name}" if rel_path else item.name
            
            if item.is_file():
                stat = item.stat()
                result.append(SourceInfo(
                    path=item_rel,
                    name=item.name,
                    is_file=True,
                    is_directory=False,
                    size=stat.st_size,
                    modified=datetime.fromtimestamp(stat.st_mtime).isoformat(),
                    mime_type=_get_mime_type(item)
                ))
            elif item.is_dir():
                result.append(SourceInfo(
                    path=item_rel,
                    name=item.name,
                    is_file=False,
                    is_directory=True
                ))
                
                if recurse:
                    result.extend(_list_directory(item, item_rel, recurse))
    except PermissionError:
        pass
    
    return result


@router.get("/{path:path}")
async def get_source(
    path: str,
    request: Request,
    recurse: str = "no",
    token_info: TokenInfo = Depends(require_permission("sources"))
):
    """Get a source file or list directory contents.
    
    If path points to a file, returns the file content.
    If path points to a directory, returns list of contents.
    
    Requires 'sources' permission.
    """
    config: KleioConfig = request.app.state.config
    source_path = resolve_source_path(path, token_info, config)
    
    if not source_path.exists():
        raise HTTPException(404, f"Source not found: {path}")
    
    if source_path.is_file():
        # Return file content
        return FileResponse(
            source_path,
            media_type=_get_mime_type(source_path),
            filename=source_path.name
        )
    
    # List directory contents
    files = _list_directory(source_path, path, recurse == "yes")
    
    return {
        "path": path,
        "files": [f.model_dump() for f in files]
    }


@router.get("")
async def list_sources(
    request: Request,
    path: str = "",
    recurse: str = "no",
    token_info: TokenInfo = Depends(require_permission("sources"))
) -> dict:
    """List source files.
    
    Returns a list of files and directories in the sources directory.
    
    Requires 'sources' permission.
    """
    config: KleioConfig = request.app.state.config
    
    if path:
        base_path = resolve_source_path(path, token_info, config)
    else:
        # Use user's source directory or default
        if token_info.sources:
            base_path = config.home_dir / token_info.sources
        else:
            base_path = config.sources_dir
    
    if not base_path.exists():
        return {"path": path, "files": []}
    
    files = _list_directory(base_path, path, recurse == "yes")
    
    return {
        "path": path,
        "files": [f.model_dump() for f in files]
    }


@router.put("/{path:path}")
async def upload_source(
    path: str,
    request: Request,
    file: UploadFile = File(...),
    token_info: TokenInfo = Depends(require_permission("upload"))
) -> dict:
    """Upload a source file.
    
    Creates or replaces a file at the specified path.
    
    Requires 'upload' permission.
    """
    config: KleioConfig = request.app.state.config
    source_path = resolve_source_path(path, token_info, config)
    
    # Validate file extension
    allowed_extensions = {".cli", ".kleio", ".str", ".yaml", ".yml", ".txt"}
    if source_path.suffix.lower() not in allowed_extensions:
        raise HTTPException(
            400,
            f"Invalid file extension. Allowed: {allowed_extensions}"
        )
    
    # Create parent directories if needed
    source_path.parent.mkdir(parents=True, exist_ok=True)
    
    # Write file
    content = await file.read()
    with open(source_path, "wb") as f:
        f.write(content)
    
    return {
        "status": "OK",
        "path": path,
        "size": len(content),
        "message": f"File uploaded: {path}"
    }


@router.delete("/{path:path}")
async def delete_source(
    path: str,
    request: Request,
    token_info: TokenInfo = Depends(require_permission("delete"))
) -> dict:
    """Delete a source file.
    
    Removes the file at the specified path.
    
    Requires 'delete' permission.
    """
    config: KleioConfig = request.app.state.config
    source_path = resolve_source_path(path, token_info, config)
    
    if not source_path.exists():
        raise HTTPException(404, f"Source not found: {path}")
    
    if source_path.is_file():
        source_path.unlink()
    else:
        raise HTTPException(400, f"Path is a directory, use DELETE /directories/{path}")
    
    return {
        "status": "OK",
        "path": path,
        "message": f"File deleted: {path}"
    }


@router.post("/{path:path}/copy")
async def copy_source(
    path: str,
    request: Request,
    origin: str,
    token_info: TokenInfo = Depends(require_permission("upload"))
) -> dict:
    """Copy a source file.
    
    Copies a file from origin to the specified path.
    
    Requires 'upload' permission.
    """
    config: KleioConfig = request.app.state.config
    
    # Resolve destination path
    dest_path = resolve_source_path(path, token_info, config)
    
    # Resolve origin path
    origin_path = resolve_source_path(origin, token_info, config)
    
    if not origin_path.exists():
        raise HTTPException(404, f"Origin file not found: {origin}")
    
    if not origin_path.is_file():
        raise HTTPException(400, f"Origin is not a file: {origin}")
    
    if dest_path.exists():
        raise HTTPException(400, f"Destination already exists: {path}")
    
    # Create parent directories if needed
    dest_path.parent.mkdir(parents=True, exist_ok=True)
    
    # Copy file
    shutil.copy2(origin_path, dest_path)
    
    return {
        "status": "OK",
        "origin": origin,
        "destination": path,
        "message": f"File copied from {origin} to {path}"
    }


@router.post("/{path:path}/move")
async def move_source(
    path: str,
    request: Request,
    origin: str,
    token_info: TokenInfo = Depends(require_permission("upload"))
) -> dict:
    """Move a source file.
    
    Moves a file from origin to the specified path.
    
    Requires 'upload' permission.
    """
    config: KleioConfig = request.app.state.config
    
    # Resolve destination path
    dest_path = resolve_source_path(path, token_info, config)
    
    # Resolve origin path
    origin_path = resolve_source_path(origin, token_info, config)
    
    if not origin_path.exists():
        raise HTTPException(404, f"Origin file not found: {origin}")
    
    if not origin_path.is_file():
        raise HTTPException(400, f"Origin is not a file: {origin}")
    
    if dest_path.exists():
        raise HTTPException(400, f"Destination already exists: {path}")
    
    # Create parent directories if needed
    dest_path.parent.mkdir(parents=True, exist_ok=True)
    
    # Move file
    shutil.move(str(origin_path), str(dest_path))
    
    return {
        "status": "OK",
        "origin": origin,
        "destination": path,
        "message": f"File moved from {origin} to {path}"
    }
