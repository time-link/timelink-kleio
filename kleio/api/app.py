"""Kleio Translation Server - FastAPI application.

This module provides the main FastAPI application for the Kleio translation
server, implementing both REST and JSON-RPC 2.0 APIs.
"""
from __future__ import annotations

import logging
from contextlib import asynccontextmanager
from typing import Optional

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from kleio.config import KleioConfig
from kleio.api.auth import TokenManager

# Configure logging
logger = logging.getLogger(__name__)


@asynccontextmanager
async def lifespan(app: FastAPI):
    """Application lifespan manager.
    
    Initializes the token manager on startup.
    """
    # Startup
    config = app.state.config
    token_manager = TokenManager(config)
    token_manager.initialize()
    app.state.token_manager = token_manager
    
    logger.info(f"Kleio server started")
    logger.info(f"Home directory: {config.home_dir}")
    logger.info(f"Sources directory: {config.sources_dir}")
    
    yield
    
    # Shutdown
    logger.info("Kleio server shutting down")


def create_app(config: Optional[KleioConfig] = None) -> FastAPI:
    """Create and configure the FastAPI application.
    
    Args:
        config: Optional KleioConfig instance. If not provided,
                configuration is loaded from environment variables.
    
    Returns:
        Configured FastAPI application instance.
    """
    if config is None:
        config = KleioConfig.from_env()
    
    app = FastAPI(
        title="Kleio Translation Server",
        description=(
            "Translation server for historical documents in Kleio notation. "
            "Provides both REST and JSON-RPC 2.0 APIs."
        ),
        version="0.1.0",
        lifespan=lifespan,
    )
    
    # Configure CORS
    app.add_middleware(
        CORSMiddleware,
        allow_origins=config.cors_origins,
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )
    
    # Store config in app state
    app.state.config = config
    app.state.token_manager = None
    
    # Include routers
    from kleio.api.routes import (
        translations,
        sources,
        exports,
        reports,
        directories,
        versions,
        tokens,
        jsonrpc,
    )
    
    app.include_router(
        translations.router,
        prefix="/rest/translations",
        tags=["translations"]
    )
    app.include_router(
        sources.router,
        prefix="/rest/sources",
        tags=["sources"]
    )
    app.include_router(
        exports.router,
        prefix="/rest/exports",
        tags=["exports"]
    )
    app.include_router(
        reports.router,
        prefix="/rest/reports",
        tags=["reports"]
    )
    app.include_router(
        directories.router,
        prefix="/rest/directories",
        tags=["directories"]
    )
    app.include_router(
        versions.router,
        prefix="/rest/versions",
        tags=["versions"]
    )
    app.include_router(
        tokens.router,
        prefix="/rest/tokens",
        tags=["tokens"]
    )
    
    # JSON-RPC compatibility endpoint
    app.include_router(jsonrpc.router, prefix="/json", tags=["jsonrpc"])
    
    # Root endpoint - home page
    @app.get("/", include_in_schema=False)
    async def root():
        """Root endpoint returning basic server info."""
        return {
            "name": "Kleio Translation Server",
            "version": "0.1.0",
            "endpoints": {
                "rest": "/rest/",
                "jsonrpc": "/json/",
                "docs": "/docs",
                "openapi": "/openapi.json"
            }
        }
    
    # Health check endpoint
    @app.get("/health")
    async def health():
        """Health check endpoint."""
        return {"status": "healthy"}
    
    return app


# Module-level ASGI application instance.
#
# uvicorn / Dockerfile.python / README_PKLEIO.md all reference
# `kleio.api.app:app`. Created eagerly from environment variables (KleioConfig
# reads KLEIO_HOME_DIR, KLEIO_ADMIN_TOKEN, etc.), so `uvicorn kleio.api.app:app`
# just works. Tests build their own app via create_app(test_config) and are not
# affected by this instance.
app = create_app()
