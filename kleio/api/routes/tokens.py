"""Token management endpoints.

This module provides REST endpoints for managing authentication tokens.
"""
from __future__ import annotations

from typing import Optional

from fastapi import APIRouter, Depends, HTTPException, Request
from pydantic import BaseModel

from kleio.api.auth import (
    TokenInfo,
    TokenManager,
    require_auth,
    require_permission,
    get_token_manager,
)
from kleio.config import KleioConfig

router = APIRouter()


class TokenGenerateRequest(BaseModel):
    """Request to generate a new token."""
    user: str
    info: dict = {}


class TokenGenerateResponse(BaseModel):
    """Response from token generation."""
    token: str


class UserInfo(BaseModel):
    """User information."""
    user: str
    tokens: int
    api: list[str] = []
    sources: str = ""
    structures: str = ""


@router.post("/generate")
async def generate_token(
    request: Request,
    body: TokenGenerateRequest,
    token_info: TokenInfo = Depends(require_permission("generate_token"))
) -> dict:
    """Generate a new authentication token.
    
    Creates a new token for the specified user with the given permissions.
    
    Requires 'generate_token' permission.
    """
    manager: TokenManager = request.app.state.token_manager
    
    # Generate the token
    token = manager.generate_token(body.user, body.info)
    
    return {"result": token}


@router.post("/invalidate")
async def invalidate_token(
    request: Request,
    token: str,
    token_info: TokenInfo = Depends(require_permission("invalidate_token"))
) -> dict:
    """Invalidate a token.
    
    Removes the specified token from the database.
    
    Requires 'invalidate_token' permission.
    """
    manager: TokenManager = request.app.state.token_manager
    
    success = manager.invalidate_token(token)
    
    if success:
        return {"result": "OK", "message": "Token invalidated"}
    else:
        return {"result": "OK", "message": "Token not found or already invalidated"}


@router.post("/users/invalidate")
async def invalidate_user(
    request: Request,
    user: str,
    token_info: TokenInfo = Depends(require_permission("invalidate_user"))
) -> dict:
    """Invalidate all tokens for a user.
    
    Removes all tokens associated with the specified user.
    
    Requires 'invalidate_user' permission.
    """
    manager: TokenManager = request.app.state.token_manager
    
    count = manager.invalidate_user(user)
    
    return {
        "result": "OK",
        "message": f"Invalidated {count} token(s) for user '{user}'"
    }


@router.get("/users")
async def list_users(
    request: Request,
    token_info: TokenInfo = Depends(require_auth)
) -> dict:
    """List all users with tokens.
    
    Returns a list of usernames that have active tokens.
    """
    manager: TokenManager = request.app.state.token_manager
    
    users = manager.list_users()
    
    return {"users": users}


@router.get("/users/{user}")
async def get_user_info(
    user: str,
    request: Request,
    token_info: TokenInfo = Depends(require_auth)
) -> UserInfo:
    """Get information about a user's tokens.
    
    Returns the user's permissions and token count.
    """
    manager: TokenManager = request.app.state.token_manager
    
    tokens = manager.get_user_tokens(user)
    
    # Get info from first token if available
    api = []
    sources = ""
    structures = ""
    
    for token in tokens:
        info = manager.validate_token(token)
        if info:
            api = info.api
            sources = info.sources
            structures = info.structures
            break
    
    return UserInfo(
        user=user,
        tokens=len(tokens),
        api=api,
        sources=sources,
        structures=structures
    )


@router.get("")
async def token_info(
    request: Request,
    token_info: TokenInfo = Depends(require_auth)
) -> dict:
    """Get information about the current token.
    
    Returns the permissions and user info for the authenticated token.
    """
    return {
        "user": token_info.user,
        "api": token_info.api,
        "sources": token_info.sources,
        "structures": token_info.structures
    }
