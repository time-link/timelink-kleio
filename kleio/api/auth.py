"""Token-based authentication for Kleio API.

This module provides token management and authentication dependencies
for the FastAPI REST API.
"""
from __future__ import annotations

import hashlib
import json
import os
import secrets
from datetime import datetime
from pathlib import Path
from typing import Optional

from fastapi import Depends, HTTPException, Request
from fastapi.security import HTTPAuthorizationCredentials, HTTPBearer

security = HTTPBearer(auto_error=False)


class TokenInfo:
    """Information associated with a token."""
    
    def __init__(
        self,
        user: str,
        api: list[str] | None = None,
        sources: str = "",
        structures: str = "",
        comment: str = "",
        **kwargs
    ):
        self.user = user
        self.api = api or []
        self.sources = sources
        self.structures = structures
        self.comment = comment
        self.extra = kwargs
    
    def to_dict(self) -> dict:
        """Convert to dictionary for serialization."""
        return {
            "user": self.user,
            "api": self.api,
            "sources": self.sources,
            "structures": self.structures,
            "comment": self.comment,
            **self.extra
        }
    
    @classmethod
    def from_dict(cls, data: dict) -> "TokenInfo":
        """Create from dictionary."""
        return cls(**data)
    
    def has_permission(self, permission: str) -> bool:
        """Check if token has a specific API permission."""
        return permission in self.api


class TokenManager:
    """Manages authentication tokens.
    
    Tokens are stored in a JSON file for persistence across server restarts.
    The admin token from the KLEIO_ADMIN_TOKEN environment variable has
    full access to all endpoints.
    """
    
    def __init__(self, config):
        """Initialize the token manager.
        
        Args:
            config: KleioConfig instance with token database path.
        """
        self.config = config
        self.admin_token = config.admin_token
        self._tokens: dict[str, TokenInfo] = {}
        self._token_file: Optional[Path] = None
        self._initialized = False
    
    def initialize(self) -> None:
        """Initialize the token database.
        
        Creates the token database file if it doesn't exist.
        """
        if self._initialized:
            return
            
        # Set up token file path
        self._token_file = self.config.tokens_dir / "token_db.json"
        self._token_file.parent.mkdir(parents=True, exist_ok=True)
        
        # Load existing tokens
        self._load_tokens()
        
        # Generate bootstrap token if no admin token in env
        if not self.admin_token and not self._tokens:
            self._generate_bootstrap_token()
        
        self._initialized = True
    
    def _load_tokens(self) -> None:
        """Load tokens from the token database file."""
        if self._token_file is None or not self._token_file.exists():
            return
            
        try:
            with open(self._token_file, "r", encoding="utf-8") as f:
                data = json.load(f)
            
            for token, info in data.get("tokens", {}).items():
                self._tokens[token] = TokenInfo.from_dict(info)
        except (json.JSONDecodeError, IOError):
            # Start with empty token store on error
            self._tokens = {}
    
    def _save_tokens(self) -> None:
        """Save tokens to the token database file."""
        if self._token_file is None:
            return
            
        data = {
            "tokens": {
                token: info.to_dict()
                for token, info in self._tokens.items()
            },
            "updated": datetime.now().isoformat()
        }
        
        with open(self._token_file, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)
    
    def _generate_bootstrap_token(self) -> str:
        """Generate a bootstrap admin token.
        
        Returns:
            The generated bootstrap token.
        """
        token = secrets.token_urlsafe(32)
        self._tokens[token] = TokenInfo(
            user="bootstrap",
            api=["generate_token", "invalidate_token", "invalidate_user"],
            sources="",
            structures="",
            comment="Bootstrap admin token"
        )
        self._save_tokens()
        return token
    
    def generate_token(self, user: str, info: dict | None = None) -> str:
        """Generate a new token for a user.
        
        Args:
            user: The username for the token.
            info: Optional dictionary with token info (api, sources, structures).
            
        Returns:
            The generated token string.
        """
        token = secrets.token_urlsafe(32)
        
        if info is None:
            info = {}
        
        token_info = TokenInfo(
            user=user,
            api=info.get("api", []),
            sources=info.get("sources", ""),
            structures=info.get("structures", ""),
            comment=info.get("comment", "")
        )
        
        self._tokens[token] = token_info
        self._save_tokens()
        
        return token
    
    def validate_token(self, token: str) -> TokenInfo | None:
        """Validate a token and return associated info.
        
        Args:
            token: The token string to validate.
            
        Returns:
            TokenInfo if valid, None otherwise.
        """
        if not token:
            return None
            
        # Check admin token
        if token == self.admin_token:
            return TokenInfo(
                user="admin",
                api=["*"],  # Full access
                sources="",
                structures="",
                comment="Admin token from environment"
            )
        
        # Check regular tokens
        return self._tokens.get(token)
    
    def invalidate_token(self, token: str) -> bool:
        """Invalidate (remove) a token.
        
        Args:
            token: The token to invalidate.
            
        Returns:
            True if token was removed, False if it didn't exist.
        """
        if token in self._tokens:
            del self._tokens[token]
            self._save_tokens()
            return True
        return False
    
    def invalidate_user(self, user: str) -> int:
        """Invalidate all tokens for a user.
        
        Args:
            user: The username whose tokens should be invalidated.
            
        Returns:
            Number of tokens removed.
        """
        tokens_to_remove = [
            token for token, info in self._tokens.items()
            if info.user == user
        ]
        
        for token in tokens_to_remove:
            del self._tokens[token]
        
        if tokens_to_remove:
            self._save_tokens()
        
        return len(tokens_to_remove)
    
    def get_user_tokens(self, user: str) -> list[str]:
        """Get all tokens for a user.
        
        Args:
            user: The username to look up.
            
        Returns:
            List of token strings for the user.
        """
        return [
            token for token, info in self._tokens.items()
            if info.user == user
        ]
    
    def list_users(self) -> list[str]:
        """List all users with tokens.
        
        Returns:
            List of unique usernames.
        """
        return list(set(info.user for info in self._tokens.values()))


def get_token_manager(request: Request) -> TokenManager:
    """Dependency to get the token manager from app state.
    
    Args:
        request: The FastAPI request object.
        
    Returns:
        The TokenManager instance.
    """
    return request.app.state.token_manager


async def require_auth(
    request: Request,
    credentials: HTTPAuthorizationCredentials = Depends(security),
) -> TokenInfo:
    """Dependency that requires valid authentication.
    
    Checks for token in Authorization header or query parameter.
    
    Args:
        request: The FastAPI request object.
        credentials: Optional HTTP Bearer credentials.
        
    Returns:
        TokenInfo for the authenticated user.
        
    Raises:
        HTTPException: If authentication fails.
    """
    token = None
    
    # Check Authorization header
    if credentials:
        token = credentials.credentials
    
    # Check query parameter (for compatibility)
    if not token:
        token = request.query_params.get("token")
    
    if not token:
        raise HTTPException(
            status_code=401,
            detail="Authentication required"
        )
    
    manager = request.app.state.token_manager
    token_info = manager.validate_token(token)
    
    if not token_info:
        raise HTTPException(
            status_code=401,
            detail="Invalid token"
        )
    
    return token_info


async def optional_auth(
    request: Request,
    credentials: HTTPAuthorizationCredentials = Depends(security),
) -> TokenInfo | None:
    """Dependency for optional authentication.
    
    Returns TokenInfo if authenticated, None otherwise.
    
    Args:
        request: The FastAPI request object.
        credentials: Optional HTTP Bearer credentials.
        
    Returns:
        TokenInfo if authenticated, None otherwise.
    """
    token = None
    
    if credentials:
        token = credentials.credentials
    
    if not token:
        token = request.query_params.get("token")
    
    if not token:
        return None
    
    manager = request.app.state.token_manager
    return manager.validate_token(token)


def require_permission(permission: str):
    """Create a dependency that requires a specific permission.
    
    Args:
        permission: The API permission required.
        
    Returns:
        A dependency function.
    """
    async def check_permission(
        token_info: TokenInfo = Depends(require_auth)
    ) -> TokenInfo:
        # Admin has all permissions
        if "*" in token_info.api:
            return token_info
        
        if permission not in token_info.api:
            raise HTTPException(
                status_code=403,
                detail=f"Permission '{permission}' required"
            )
        
        return token_info
    
    return check_permission


def resolve_source_path(
    path: str,
    token_info: TokenInfo,
    config
) -> Path:
    """Resolve a source file path for a user.
    
    Validates that the path is within the user's allowed sources directory.
    
    Args:
        path: Relative path to the source file.
        token_info: Token info with user's sources directory.
        config: KleioConfig instance.
        
    Returns:
        Absolute path to the source file.
        
    Raises:
        HTTPException: If path is outside allowed directory.
    """
    # Get user's base sources directory
    if token_info.sources:
        base_dir = config.home_dir / token_info.sources
    else:
        # Admin or coordinator with full access
        base_dir = config.sources_dir
    
    # Resolve the full path
    full_path = (base_dir / path).resolve()
    
    # Security check: ensure path is within base_dir
    try:
        full_path.relative_to(base_dir.resolve())
    except ValueError:
        raise HTTPException(
            status_code=403,
            detail="Access denied: path outside allowed directory"
        )
    
    return full_path


def resolve_structure_path(
    path: str,
    token_info: TokenInfo,
    config
) -> Path:
    """Resolve a structure file path for a user.
    
    Args:
        path: Relative path to the structure file.
        token_info: Token info with user's structures directory.
        config: KleioConfig instance.
        
    Returns:
        Absolute path to the structure file.
        
    Raises:
        HTTPException: If path is outside allowed directory.
    """
    if token_info.structures:
        base_dir = config.home_dir / token_info.structures
    else:
        base_dir = config.structures_dir
    
    full_path = (base_dir / path).resolve()
    
    try:
        full_path.relative_to(base_dir.resolve())
    except ValueError:
        raise HTTPException(
            status_code=403,
            detail="Access denied: path outside allowed directory"
        )
    
    return full_path
