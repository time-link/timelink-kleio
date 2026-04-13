"""Git version control endpoints.

This module provides REST endpoints for Git operations.
"""
from __future__ import annotations

import logging
from pathlib import Path
from typing import Optional

from fastapi import APIRouter, Depends, HTTPException, Request
from pydantic import BaseModel

from kleio.api.auth import (
    TokenInfo,
    require_auth,
    require_permission,
    resolve_source_path,
)
from kleio.config import KleioConfig

logger = logging.getLogger(__name__)

router = APIRouter()

# Try to import git module
try:
    import git
    GIT_AVAILABLE = True
except ImportError:
    GIT_AVAILABLE = False


class GitStatus(BaseModel):
    """Git repository status."""
    path: str
    is_repo: bool
    branch: str = ""
    clean: bool = True
    ahead: int = 0
    behind: int = 0
    modified: list[str] = []
    staged: list[str] = []
    untracked: list[str] = []


class GitCommit(BaseModel):
    """Git commit request."""
    path: str
    message: str
    add_all: bool = True


class GitPull(BaseModel):
    """Git pull result."""
    path: str
    success: bool
    message: str
    changes: list[str] = []


class GitPush(BaseModel):
    """Git push result."""
    path: str
    success: bool
    message: str


def _get_repo(path: Path) -> Optional["git.Repo"]:
    """Get git repository for a path."""
    if not GIT_AVAILABLE:
        return None
    
    try:
        return git.Repo(path, search_parent_directories=True)
    except git.InvalidGitRepositoryError:
        return None
    except git.NoSuchPathError:
        return None


@router.get("/{path:path}/status")
async def get_git_status(
    path: str,
    request: Request,
    token_info: TokenInfo = Depends(require_auth)
) -> GitStatus:
    """Get Git status for a path.
    
    Returns information about the Git repository containing the path.
    """
    if not GIT_AVAILABLE:
        raise HTTPException(501, "Git support not available (install gitpython)")
    
    config: KleioConfig = request.app.state.config
    source_path = resolve_source_path(path, token_info, config)
    
    repo = _get_repo(source_path)
    
    if repo is None:
        return GitStatus(
            path=path,
            is_repo=False
        )
    
    # Get status
    try:
        branch = repo.active_branch.name
    except TypeError:
        branch = "HEAD (detached)"
    
    # Check for changes
    modified = []
    staged = []
    untracked = []
    
    for item in repo.index.diff(None):
        modified.append(item.a_path)
    
    for item in repo.index.diff("HEAD"):
        staged.append(item.a_path)
    
    for item in repo.untracked_files:
        untracked.append(item)
    
    # Check ahead/behind
    ahead = 0
    behind = 0
    try:
        tracking = repo.active_branch.tracking_branch()
        if tracking:
            ahead = sum(1 for _ in repo.iter_commits(f"{tracking}..{repo.active_branch}"))
            behind = sum(1 for _ in repo.iter_commits(f"{repo.active_branch}..{tracking}"))
    except Exception:
        pass
    
    return GitStatus(
        path=path,
        is_repo=True,
        branch=branch,
        clean=len(modified) == 0 and len(staged) == 0 and len(untracked) == 0,
        ahead=ahead,
        behind=behind,
        modified=modified,
        staged=staged,
        untracked=untracked
    )


@router.post("/{path:path}/pull")
async def git_pull(
    path: str,
    request: Request,
    token_info: TokenInfo = Depends(require_permission("sources"))
) -> GitPull:
    """Pull changes from remote.
    
    Requires 'sources' permission.
    """
    if not GIT_AVAILABLE:
        raise HTTPException(501, "Git support not available")
    
    config: KleioConfig = request.app.state.config
    source_path = resolve_source_path(path, token_info, config)
    
    repo = _get_repo(source_path)
    
    if repo is None:
        raise HTTPException(400, f"Not a Git repository: {path}")
    
    try:
        origin = repo.remotes.origin
        result = origin.pull()
        
        changes = []
        for item in result:
            if item.flags:
                changes.append(str(item.ref))
        
        return GitPull(
            path=path,
            success=True,
            message=f"Pulled {len(changes)} changes",
            changes=changes
        )
    except Exception as e:
        logger.error(f"Git pull failed: {e}")
        return GitPull(
            path=path,
            success=False,
            message=str(e)
        )


@router.post("/{path:path}/push")
async def git_push(
    path: str,
    request: Request,
    token_info: TokenInfo = Depends(require_permission("upload"))
) -> GitPush:
    """Push changes to remote.
    
    Requires 'upload' permission.
    """
    if not GIT_AVAILABLE:
        raise HTTPException(501, "Git support not available")
    
    config: KleioConfig = request.app.state.config
    source_path = resolve_source_path(path, token_info, config)
    
    repo = _get_repo(source_path)
    
    if repo is None:
        raise HTTPException(400, f"Not a Git repository: {path}")
    
    try:
        origin = repo.remotes.origin
        origin.push()
        
        return GitPush(
            path=path,
            success=True,
            message="Push successful"
        )
    except Exception as e:
        logger.error(f"Git push failed: {e}")
        return GitPush(
            path=path,
            success=False,
            message=str(e)
        )


@router.post("/{path:path}/commit")
async def git_commit(
    path: str,
    request: Request,
    body: GitCommit,
    token_info: TokenInfo = Depends(require_permission("upload"))
) -> dict:
    """Commit changes.
    
    Requires 'upload' permission.
    """
    if not GIT_AVAILABLE:
        raise HTTPException(501, "Git support not available")
    
    config: KleioConfig = request.app.state.config
    source_path = resolve_source_path(path, token_info, config)
    
    repo = _get_repo(source_path)
    
    if repo is None:
        raise HTTPException(400, f"Not a Git repository: {path}")
    
    try:
        if body.add_all:
            # Add all changes
            repo.git.add(A=True)
        
        # Commit
        repo.index.commit(body.message)
        
        return {
            "status": "OK",
            "path": path,
            "message": f"Committed: {body.message}"
        }
    except Exception as e:
        logger.error(f"Git commit failed: {e}")
        raise HTTPException(500, f"Commit failed: {e}")


@router.get("")
async def git_info(
    request: Request,
    token_info: TokenInfo = Depends(require_auth)
) -> dict:
    """Get Git availability and version info."""
    if GIT_AVAILABLE:
        import git
        return {
            "available": True,
            "version": git.__version__,
            "message": "Git support available"
        }
    else:
        return {
            "available": False,
            "version": None,
            "message": "Git support not available (install gitpython)"
        }
