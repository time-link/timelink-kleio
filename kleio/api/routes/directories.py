"""Directory management endpoints.

This module provides REST endpoints for managing directories.
"""
from __future__ import annotations

import shutil
from pathlib import Path

from fastapi import APIRouter, Depends, HTTPException, Request

from kleio.api.auth import (
    TokenInfo,
    require_auth,
    require_permission,
    resolve_source_path,
    resolve_structure_path,
)
from kleio.config import KleioConfig

router = APIRouter()


@router.get("/{path:path}")
async def list_directory(
    path: str,
    request: Request,
    structure: str = "no",
    recurse: str = "no",
    token_info: TokenInfo = Depends(require_auth)
) -> dict:
    """List directory contents.
    
    Returns a list of files and subdirectories.
    """
    config: KleioConfig = request.app.state.config
    
    if structure == "yes":
        dir_path = resolve_structure_path(path, token_info, config)
    else:
        dir_path = resolve_source_path(path, token_info, config)
    
    if not dir_path.exists():
        raise HTTPException(404, f"Directory not found: {path}")
    
    if not dir_path.is_dir():
        raise HTTPException(400, f"Path is not a directory: {path}")
    
    # List directory contents
    contents = []
    for item in sorted(dir_path.iterdir()):
        item_rel = f"{path}/{item.name}"
        contents.append({
            "name": item.name,
            "path": item_rel,
            "is_file": item.is_file(),
            "is_directory": item.is_dir(),
        })
    
    return {
        "path": path,
        "structure": structure,
        "contents": contents
    }


@router.post("/{path:path}")
async def create_directory(
    path: str,
    request: Request,
    structure: str = "no",
    token_info: TokenInfo = Depends(require_permission("mkdir"))
) -> dict:
    """Create a directory.
    
    Requires 'mkdir' permission.
    """
    config: KleioConfig = request.app.state.config
    
    if structure == "yes":
        dir_path = resolve_structure_path(path, token_info, config)
    else:
        dir_path = resolve_source_path(path, token_info, config)
    
    if dir_path.exists():
        raise HTTPException(400, f"Directory already exists: {path}")
    
    dir_path.mkdir(parents=True, exist_ok=False)
    
    return {
        "status": "OK",
        "path": path,
        "message": f"Directory created: {path}"
    }


@router.delete("/{path:path}")
async def delete_directory(
    path: str,
    request: Request,
    structure: str = "no",
    force: str = "no",
    token_info: TokenInfo = Depends(require_permission("rmdir"))
) -> dict:
    """Delete a directory.
    
    Query params:
        structure: "yes" to operate on structure directory
        force: "yes" to delete non-empty directory
    
    Requires 'rmdir' permission.
    """
    config: KleioConfig = request.app.state.config
    
    if structure == "yes":
        dir_path = resolve_structure_path(path, token_info, config)
    else:
        dir_path = resolve_source_path(path, token_info, config)
    
    if not dir_path.exists():
        raise HTTPException(404, f"Directory not found: {path}")
    
    if not dir_path.is_dir():
        raise HTTPException(400, f"Path is not a directory: {path}")
    
    try:
        if force == "yes":
            shutil.rmtree(dir_path)
        else:
            dir_path.rmdir()  # Will fail if not empty
    except OSError as e:
        if "not empty" in str(e).lower():
            raise HTTPException(
                400,
                f"Directory not empty: {path}. Use force=yes to delete."
            )
        raise
    
    return {
        "status": "OK",
        "path": path,
        "message": f"Directory deleted: {path}"
    }


@router.get("")
async def list_root_directories(
    request: Request,
    structure: str = "no",
    token_info: TokenInfo = Depends(require_auth)
) -> dict:
    """List root directories accessible to the user.
    
    Returns the user's base source or structure directory contents.
    """
    config: KleioConfig = request.app.state.config
    
    if structure == "yes":
        if token_info.structures:
            base_path = config.home_dir / token_info.structures
        else:
            base_path = config.structures_dir
    else:
        if token_info.sources:
            base_path = config.home_dir / token_info.sources
        else:
            base_path = config.sources_dir
    
    contents = []
    if base_path.exists():
        for item in sorted(base_path.iterdir()):
            contents.append({
                "name": item.name,
                "path": item.name,
                "is_file": item.is_file(),
                "is_directory": item.is_dir(),
            })
    
    return {
        "base_path": str(base_path.relative_to(config.home_dir)) if base_path.exists() else "",
        "contents": contents
    }
