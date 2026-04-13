"""JSON-RPC 2.0 compatibility endpoint.

This module provides a JSON-RPC 2.0 compatible endpoint for backward
compatibility with the original Prolog server.
"""
from __future__ import annotations

import logging
from typing import Any, Optional

from fastapi import APIRouter, Depends, HTTPException, Request
from pydantic import BaseModel, Field

from kleio.api.auth import (
    TokenInfo,
    TokenManager,
    require_auth,
    resolve_source_path,
    resolve_structure_path,
)
from kleio.config import KleioConfig

logger = logging.getLogger(__name__)

router = APIRouter()


class JsonRpcRequest(BaseModel):
    """JSON-RPC 2.0 request."""
    jsonrpc: str = "2.0"
    method: str
    params: dict = Field(default_factory=dict)
    id: Optional[int | str] = None


class JsonRpcError(BaseModel):
    """JSON-RPC 2.0 error."""
    code: int
    message: str
    data: Optional[Any] = None


class JsonRpcResponse(BaseModel):
    """JSON-RPC 2.0 response."""
    jsonrpc: str = "2.0"
    result: Optional[Any] = None
    error: Optional[JsonRpcError] = None
    id: Optional[int | str] = None


# JSON-RPC error codes
PARSE_ERROR = -32700
INVALID_REQUEST = -32600
METHOD_NOT_FOUND = -32601
INVALID_PARAMS = -32602
INTERNAL_ERROR = -32603
SERVER_ERROR = -32000


def _make_response(
    result: Any = None,
    error: JsonRpcError | None = None,
    request_id: Any = None
) -> JsonRpcResponse:
    """Create a JSON-RPC response."""
    return JsonRpcResponse(
        jsonrpc="2.0",
        result=result,
        error=error,
        id=request_id
    )


def _make_error(code: int, message: str, request_id: Any = None) -> JsonRpcResponse:
    """Create a JSON-RPC error response."""
    return _make_response(
        error=JsonRpcError(code=code, message=message),
        request_id=request_id
    )


async def _handle_method(
    method: str,
    params: dict,
    request: Request
) -> Any:
    """Handle a JSON-RPC method call.
    
    Args:
        method: The method name.
        params: Method parameters.
        request: FastAPI request object.
        
    Returns:
        Method result.
        
    Raises:
        HTTPException: If method fails.
    """
    config: KleioConfig = request.app.state.config
    token_manager: TokenManager = request.app.state.token_manager
    
    # Extract token and validate
    token = params.get("token")
    if not token:
        raise HTTPException(401, "Token required")
    
    token_info = token_manager.validate_token(token)
    if not token_info:
        raise HTTPException(401, "Invalid token")
    
    # Route to appropriate handler
    if method == "translations":
        return await _handle_translations(params, request, token_info, config)
    
    elif method == "tokens_generate":
        return await _handle_tokens_generate(params, request, token_info, token_manager)
    
    elif method == "tokens_invalidate":
        return await _handle_tokens_invalidate(params, token_info, token_manager)
    
    elif method == "users_invalidate":
        return await _handle_users_invalidate(params, token_info, token_manager)
    
    elif method == "sources_get":
        return await _handle_sources_get(params, request, token_info, config)
    
    elif method == "sources_delete":
        return await _handle_sources_delete(params, request, token_info, config)
    
    elif method == "kleioset":
        return await _handle_kleioset(params, request, token_info, config)
    
    elif method == "clean":
        return await _handle_clean(params, request, token_info, config)
    
    elif method == "update":
        return await _handle_update(params, request, token_info, config)
    
    elif method == "mkdir":
        return await _handle_mkdir(params, request, token_info, config)
    
    elif method == "rmdir":
        return await _handle_rmdir(params, request, token_info, config)
    
    else:
        raise HTTPException(404, f"Method not found: {method}")


async def _handle_translations(
    params: dict,
    request: Request,
    token_info: TokenInfo,
    config: KleioConfig
) -> dict:
    """Handle translations method."""
    from kleio.api.routes.translations import start_translation, TranslateRequest
    
    path = params.get("path")
    if not path:
        raise HTTPException(400, "Missing parameter: path")
    
    body = TranslateRequest(
        path=path,
        structure=params.get("structure", ""),
        echo=params.get("echo", "no")
    )
    
    result = await start_translation(request, body, token_info=token_info)
    return result


async def _handle_tokens_generate(
    params: dict,
    request: Request,
    token_info: TokenInfo,
    token_manager: TokenManager
) -> str:
    """Handle tokens_generate method."""
    user = params.get("user")
    if not user:
        raise HTTPException(400, "Missing parameter: user")
    
    info = params.get("info", {})
    token = token_manager.generate_token(user, info)
    return token


async def _handle_tokens_invalidate(
    params: dict,
    token_info: TokenInfo,
    token_manager: TokenManager
) -> str:
    """Handle tokens_invalidate method."""
    token = params.get("token_to_invalidate") or params.get("token")
    if not token:
        raise HTTPException(400, "Missing parameter: token")
    
    success = token_manager.invalidate_token(token)
    return "OK" if success else "Token not found"


async def _handle_users_invalidate(
    params: dict,
    token_info: TokenInfo,
    token_manager: TokenManager
) -> str:
    """Handle users_invalidate method."""
    user = params.get("user")
    if not user:
        raise HTTPException(400, "Missing parameter: user")
    
    count = token_manager.invalidate_user(user)
    return f"Invalidated {count} tokens for {user}"


async def _handle_sources_get(
    params: dict,
    request: Request,
    token_info: TokenInfo,
    config: KleioConfig
) -> Any:
    """Handle sources_get method."""
    path = params.get("path", "")
    recurse = params.get("recurse", "no")
    
    try:
        source_path = resolve_source_path(path, token_info, config)
    except HTTPException:
        # Path might be for a file that doesn't exist yet
        source_path = config.sources_dir / path
    
    if source_path.is_file():
        # Return file info
        return {
            "url": f"/rest/sources/{path}",
            "mime_type": "text/x-kleio-cli"
        }
    else:
        # List directory
        files = []
        if source_path.exists():
            pattern = "**/*" if recurse == "yes" else "*"
            for item in source_path.glob(pattern):
                if item.is_file():
                    rel_path = str(item.relative_to(config.sources_dir))
                    files.append(rel_path)
        
        return files


async def _handle_sources_delete(
    params: dict,
    request: Request,
    token_info: TokenInfo,
    config: KleioConfig
) -> str:
    """Handle sources_delete method."""
    path = params.get("path")
    if not path:
        raise HTTPException(400, "Missing parameter: path")
    
    source_path = resolve_source_path(path, token_info, config)
    
    if source_path.is_file():
        source_path.unlink()
        return f"Deleted: {path}"
    else:
        raise HTTPException(400, f"Path is not a file: {path}")


async def _handle_kleioset(
    params: dict,
    request: Request,
    token_info: TokenInfo,
    config: KleioConfig
) -> dict:
    """Handle kleioset method - get file set information."""
    path = params.get("path", "")
    
    try:
        source_path = resolve_source_path(path, token_info, config)
    except HTTPException:
        source_path = config.sources_dir / path
    
    result = {
        "kleio": [],
        "rpt": [],
        "err": [],
        "xml": [],
        "org": [],
        "old": [],
        "ids": []
    }
    
    if source_path.exists():
        if source_path.is_file():
            # Single file
            base = source_path
            for key, ext in [("kleio", ".cli"), ("rpt", ".rpt"), ("err", ".err"),
                           ("xml", ".xml"), ("ids", ".ids")]:
                check_path = base.with_suffix(ext)
                if check_path.exists():
                    result[key].append(str(check_path.relative_to(config.home_dir)))
        else:
            # Directory
            for key, ext in [("kleio", "*.cli"), ("rpt", "*.rpt"), ("err", "*.err"),
                           ("xml", "*.xml"), ("ids", "*.ids")]:
                for f in source_path.glob(ext):
                    result[key].append(str(f.relative_to(config.home_dir)))
    
    return result


async def _handle_clean(
    params: dict,
    request: Request,
    token_info: TokenInfo,
    config: KleioConfig
) -> list:
    """Handle clean method - remove translation outputs."""
    path = params.get("path", "")
    
    try:
        source_path = resolve_source_path(path, token_info, config)
    except HTTPException:
        source_path = config.sources_dir / path
    
    cleaned = []
    
    for ext in [".xml", ".ids", ".rpt", ".err", ".files.json"]:
        if source_path.is_file():
            output_path = source_path.with_suffix(ext)
        else:
            continue
        
        if output_path.exists():
            output_path.unlink()
            cleaned.append(str(output_path))
    
    return cleaned


async def _handle_update(
    params: dict,
    request: Request,
    token_info: TokenInfo,
    config: KleioConfig
) -> dict:
    """Handle update method - translate modified files."""
    # This would need to check file modification times and translate as needed
    # For now, return empty result
    return {"translations": 0, "sources": []}


async def _handle_mkdir(
    params: dict,
    request: Request,
    token_info: TokenInfo,
    config: KleioConfig
) -> str:
    """Handle mkdir method."""
    path = params.get("path")
    if not path:
        raise HTTPException(400, "Missing parameter: path")
    
    structure = params.get("structure", "no")
    
    if structure == "yes":
        dir_path = resolve_structure_path(path, token_info, config)
    else:
        dir_path = resolve_source_path(path, token_info, config)
    
    if dir_path.exists():
        raise HTTPException(400, f"Directory already exists: {path}")
    
    dir_path.mkdir(parents=True, exist_ok=False)
    return path


async def _handle_rmdir(
    params: dict,
    request: Request,
    token_info: TokenInfo,
    config: KleioConfig
) -> str:
    """Handle rmdir method."""
    import shutil
    
    path = params.get("path")
    if not path:
        raise HTTPException(400, "Missing parameter: path")
    
    structure = params.get("structure", "no")
    contents = params.get("contents", "no")
    
    if structure == "yes":
        dir_path = resolve_structure_path(path, token_info, config)
    else:
        dir_path = resolve_source_path(path, token_info, config)
    
    if not dir_path.exists():
        raise HTTPException(404, f"Directory not found: {path}")
    
    if contents == "yes":
        shutil.rmtree(dir_path)
    else:
        dir_path.rmdir()
    
    return path


@router.post("/")
async def jsonrpc_endpoint(
    request: Request,
    body: JsonRpcRequest | list[JsonRpcRequest]
) -> JsonRpcResponse | list[JsonRpcResponse]:
    """JSON-RPC 2.0 endpoint.
    
    Accepts single requests or batch requests.
    """
    # Handle batch requests
    if isinstance(body, list):
        return [await _handle_single_request(request, req) for req in body]
    
    return await _handle_single_request(request, body)


async def _handle_single_request(
    request: Request,
    body: JsonRpcRequest
) -> JsonRpcResponse:
    """Handle a single JSON-RPC request."""
    # Validate jsonrpc version
    if body.jsonrpc != "2.0":
        return _make_error(INVALID_REQUEST, "Invalid jsonrpc version", body.id)
    
    try:
        result = await _handle_method(body.method, body.params, request)
        return _make_response(result=result, request_id=body.id)
    
    except HTTPException as e:
        # Map HTTP errors to JSON-RPC errors
        if e.status_code == 401:
            return _make_error(INVALID_PARAMS, str(e.detail), body.id)
        elif e.status_code == 403:
            return _make_error(SERVER_ERROR + 6, str(e.detail), body.id)
        elif e.status_code == 404:
            return _make_error(METHOD_NOT_FOUND, str(e.detail), body.id)
        else:
            return _make_error(SERVER_ERROR, str(e.detail), body.id)
    
    except Exception as e:
        logger.exception(f"JSON-RPC error: {e}")
        return _make_error(INTERNAL_ERROR, str(e), body.id)


@router.post("")
async def jsonrpc_endpoint_alt(request: Request, body: JsonRpcRequest):
    """JSON-RPC endpoint (alternate path without trailing slash)."""
    return await jsonrpc_endpoint(request, body)

