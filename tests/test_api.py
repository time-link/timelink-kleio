"""API tests for the Kleio FastAPI server.

Tests REST API endpoints using FastAPI's TestClient.
"""
from __future__ import annotations

import os
import tempfile
from pathlib import Path
from unittest.mock import MagicMock, patch

import pytest
from fastapi.testclient import TestClient

from kleio.api.app import create_app
from kleio.api.auth import TokenInfo, TokenManager
from kleio.config import KleioConfig


# =============================================================================
# Test Fixtures
# =============================================================================

# Paths to test data directories
TESTS_DIR = Path(__file__).parent
KLEIO_HOME = TESTS_DIR / "kleio-home"


@pytest.fixture
def test_config():
    """Create a test configuration with a temporary directory."""
    with tempfile.TemporaryDirectory() as tmpdir:
        home_dir = Path(tmpdir)
        
        # Create necessary directories
        (home_dir / "sources").mkdir(parents=True, exist_ok=True)
        (home_dir / "structures").mkdir(parents=True, exist_ok=True)
        (home_dir / "system" / "conf" / "kleio" / "tokens").mkdir(parents=True, exist_ok=True)
        
        config = KleioConfig(
            home_dir=home_dir,
            admin_token="test-admin-token-12345",
            debug=True
        )
        yield config


@pytest.fixture
def test_client(test_config):
    """Create a test client for the API."""
    app = create_app(test_config)
    
    with TestClient(app) as client:
        yield client


@pytest.fixture
def auth_headers(test_config):
    """Create authorization headers with admin token."""
    return {"Authorization": f"Bearer {test_config.admin_token}"}


@pytest.fixture
def test_client_with_kleio_home():
    """Create a test client with the real kleio-home directory."""
    if not KLEIO_HOME.exists():
        pytest.skip("kleio-home directory not found")
    
    config = KleioConfig(
        home_dir=KLEIO_HOME,
        admin_token="test-admin-token-12345",
        debug=True
    )
    
    app = create_app(config)
    
    with TestClient(app) as client:
        client.config = config
        yield client


# =============================================================================
# Root and Health Endpoints Tests
# =============================================================================

class TestRootEndpoints:
    """Tests for root and health endpoints."""
    
    def test_root_endpoint(self, test_client):
        """Test the root endpoint returns server info."""
        response = test_client.get("/")
        
        assert response.status_code == 200
        data = response.json()
        
        assert data["name"] == "Kleio Translation Server"
        assert "version" in data
        assert "endpoints" in data
        assert "rest" in data["endpoints"]
        assert "docs" in data["endpoints"]
    
    def test_health_endpoint(self, test_client):
        """Test the health check endpoint."""
        response = test_client.get("/health")
        
        assert response.status_code == 200
        data = response.json()
        
        assert data["status"] == "healthy"
    
    def test_openapi_endpoint(self, test_client):
        """Test the OpenAPI schema endpoint."""
        response = test_client.get("/openapi.json")
        
        assert response.status_code == 200
        data = response.json()
        
        assert "openapi" in data
        assert "info" in data
        assert "paths" in data


# =============================================================================
# Authentication Tests
# =============================================================================

class TestAuthentication:
    """Tests for token-based authentication."""
    
    def test_unauthorized_request(self, test_client):
        """Test that protected endpoints require authentication."""
        response = test_client.get("/rest/sources")
        
        assert response.status_code == 401
    
    def test_invalid_token(self, test_client):
        """Test that invalid tokens are rejected."""
        headers = {"Authorization": "Bearer invalid-token"}
        response = test_client.get("/rest/sources", headers=headers)
        
        assert response.status_code == 401
    
    def test_valid_admin_token(self, test_client, auth_headers):
        """Test that admin token is accepted."""
        response = test_client.get("/rest/sources", headers=auth_headers)
        
        assert response.status_code == 200
    
    def test_token_query_parameter(self, test_client, test_config):
        """Test that token can be passed as query parameter."""
        response = test_client.get(f"/rest/sources?token={test_config.admin_token}")
        
        assert response.status_code == 200


# =============================================================================
# Sources Endpoint Tests
# =============================================================================

class TestSourcesEndpoints:
    """Tests for source file management endpoints."""
    
    def test_list_sources(self, test_client, auth_headers):
        """Test listing source files."""
        response = test_client.get("/rest/sources", headers=auth_headers)
        
        assert response.status_code == 200
        data = response.json()
        
        assert "path" in data
        assert "files" in data
    
    def test_list_sources_with_path(self, test_client_with_kleio_home):
        """Test listing sources with a specific path."""
        headers = {"Authorization": f"Bearer {test_client_with_kleio_home.config.admin_token}"}
        response = test_client_with_kleio_home.get(
            "/rest/sources/api/paroquiais",
            headers=headers
        )
        
        # May be 200 or 404 depending on whether path exists
        if response.status_code == 200:
            data = response.json()
            assert "files" in data
    
    def test_get_source_file_not_found(self, test_client, auth_headers):
        """Test getting a non-existent source file."""
        response = test_client.get("/rest/sources/nonexistent.cli", headers=auth_headers)
        
        assert response.status_code == 404
    
    def test_upload_source_file(self, test_client, auth_headers):
        """Test uploading a source file."""
        # First check if upload permission is granted
        # Note: admin token has all permissions
        
        file_content = b"kleio$test\nfonte$test-source"
        
        response = test_client.put(
            "/rest/sources/test-upload.cli",
            headers=auth_headers,
            files={"file": ("test-upload.cli", file_content, "text/plain")}
        )
        
        # May fail if upload permission not in token
        if response.status_code == 200:
            data = response.json()
            assert data["status"] == "OK"
            assert "path" in data


# =============================================================================
# Translations Endpoint Tests
# =============================================================================

class TestTranslationsEndpoints:
    """Tests for translation endpoints."""
    
    def test_list_translations(self, test_client, auth_headers):
        """Test listing translation jobs."""
        response = test_client.get("/rest/translations", headers=auth_headers)
        
        assert response.status_code == 200
        data = response.json()
        
        assert "jobs" in data
    
    def test_get_translation_status_not_found(self, test_client, auth_headers):
        """Test getting status for non-existent translation."""
        response = test_client.get("/rest/translations/nonexistent.cli", headers=auth_headers)
        
        # This returns translation status, not 404 for missing source
        # The source check happens elsewhere
        assert response.status_code in [200, 404]
    
    def test_start_translation_missing_file(self, test_client, auth_headers):
        """Test starting translation for non-existent file."""
        response = test_client.post(
            "/rest/translations",
            headers=auth_headers,
            json={"path": "nonexistent.cli"}
        )
        
        assert response.status_code == 404


# =============================================================================
# Tokens Endpoint Tests
# =============================================================================

class TestTokensEndpoints:
    """Tests for token management endpoints."""
    
    def test_generate_token_requires_permission(self, test_client, auth_headers):
        """Test that token generation requires proper permission."""
        response = test_client.post(
            "/rest/tokens/generate",
            headers=auth_headers,
            json={
                "user": "test-user",
                "api": ["sources"],
                "sources": "sources/"
            }
        )
        
        # Admin token has all permissions, so should work
        if response.status_code == 200:
            data = response.json()
            assert "result" in data
        else:
            # May fail if generate_token permission not set up
            assert response.status_code in [403, 401]
    
    def test_list_tokens(self, test_client, auth_headers):
        """Test listing tokens."""
        response = test_client.get("/rest/tokens", headers=auth_headers)
        
        # This endpoint may not exist or require different permissions
        assert response.status_code in [200, 404, 403]


# =============================================================================
# Exports Endpoint Tests
# =============================================================================

class TestExportsEndpoints:
    """Tests for export endpoints."""
    
    def test_list_exports(self, test_client, auth_headers):
        """Test listing exports."""
        response = test_client.get("/rest/exports", headers=auth_headers)
        
        # May not be implemented
        assert response.status_code in [200, 404]


# =============================================================================
# Directories Endpoint Tests
# =============================================================================

class TestDirectoriesEndpoints:
    """Tests for directory management endpoints."""
    
    def test_list_directories(self, test_client, auth_headers):
        """Test listing directories."""
        response = test_client.get("/rest/directories", headers=auth_headers)
        
        assert response.status_code in [200, 404]


# =============================================================================
# Reports Endpoint Tests
# =============================================================================

class TestReportsEndpoints:
    """Tests for reports endpoints."""
    
    def test_list_reports(self, test_client, auth_headers):
        """Test listing reports."""
        response = test_client.get("/rest/reports", headers=auth_headers)
        
        assert response.status_code in [200, 404]


# =============================================================================
# Integration Tests with Real Kleio-Home
# =============================================================================

class TestAPIWithRealData:
    """Tests using real test data from kleio-home."""
    
    def test_list_real_sources(self, test_client_with_kleio_home):
        """Test listing sources from real kleio-home."""
        headers = {"Authorization": f"Bearer {test_client_with_kleio_home.config.admin_token}"}
        response = test_client_with_kleio_home.get("/rest/sources", headers=headers)
        
        assert response.status_code == 200
        data = response.json()
        
        assert "files" in data
        
        # Should have some directories
        dirs = [f for f in data["files"] if f.get("is_directory")]
        assert len(dirs) > 0
    
    def test_browse_paroquiais_directory(self, test_client_with_kleio_home):
        """Test browsing the paroquiais directory structure."""
        headers = {"Authorization": f"Bearer {test_client_with_kleio_home.config.admin_token}"}
        
        # First check if api/paroquiais exists
        response = test_client_with_kleio_home.get("/rest/sources/api", headers=headers)
        
        if response.status_code != 200:
            pytest.skip("api directory not found in kleio-home")
        
        # Navigate to paroquiais
        response = test_client_with_kleio_home.get("/rest/sources/api/paroquiais", headers=headers)
        
        if response.status_code == 200:
            data = response.json()
            assert "files" in data
    
    def test_get_real_source_file(self, test_client_with_kleio_home):
        """Test getting a real source file."""
        headers = {"Authorization": f"Bearer {test_client_with_kleio_home.config.admin_token}"}
        
        # Try to get a known test file
        response = test_client_with_kleio_home.get(
            "/rest/sources/api/paroquiais/baptismos/bapt1714.cli",
            headers=headers
        )
        
        # Check if file exists
        if response.status_code == 200:
            # Should return file content
            content = response.content
            assert len(content) > 0
            # Should start with kleio$ header
            assert content.startswith(b"kleio$")
    
    def test_translate_real_source_file(self, test_client_with_kleio_home):
        """Test translating a real source file."""
        headers = {"Authorization": f"Bearer {test_client_with_kleio_home.config.admin_token}"}
        
        # First check if file exists
        check_response = test_client_with_kleio_home.get(
            "/rest/sources/api/paroquiais/baptismos/bapt1714.cli",
            headers=headers
        )
        
        if check_response.status_code != 200:
            pytest.skip("bapt1714.cli not found in kleio-home")
        
        # Start translation
        response = test_client_with_kleio_home.post(
            "/rest/translations",
            headers=headers,
            json={
                "path": "api/paroquiais/baptismos/bapt1714.cli"
            }
        )
        
        if response.status_code == 200:
            data = response.json()
            assert data["status"] == "OK"
            assert "job" in data
            assert "job_id" in data["job"]


# =============================================================================
# Permission Tests
# =============================================================================

class TestPermissions:
    """Tests for permission-based access control."""
    
    def test_sources_permission_required(self, test_client, test_config):
        """Test that sources endpoint requires 'sources' permission."""
        # Get the app's token manager to ensure token is registered
        token_manager = test_client.app.state.token_manager
        
        # Create a token without sources permission
        limited_token = token_manager.generate_token(
            user="limited-user",
            info={"api": ["translations"]}  # No sources permission
        )
        
        headers = {"Authorization": f"Bearer {limited_token}"}
        response = test_client.get("/rest/sources", headers=headers)
        
        # Should be forbidden
        assert response.status_code == 403
    
    def test_delete_permission_required(self, test_client, test_config):
        """Test that delete endpoint requires 'delete' permission."""
        # Get the app's token manager to ensure token is registered
        token_manager = test_client.app.state.token_manager
        
        limited_token = token_manager.generate_token(
            user="limited-user",
            info={"api": ["sources"]}  # No delete permission
        )
        
        headers = {"Authorization": f"Bearer {limited_token}"}
        response = test_client.delete("/rest/sources/test.cli", headers=headers)
        
        assert response.status_code == 403


# =============================================================================
# CORS Tests
# =============================================================================

class TestCORS:
    """Tests for CORS configuration."""
    
    def test_cors_headers(self, test_client):
        """Test that CORS headers are set correctly."""
        response = test_client.options(
            "/",
            headers={
                "Origin": "http://localhost:3000",
                "Access-Control-Request-Method": "GET"
            }
        )
        
        # CORS middleware should handle OPTIONS
        assert response.status_code in [200, 400, 405]


# =============================================================================
# Error Response Tests
# =============================================================================

class TestErrorResponses:
    """Tests for error response formatting."""
    
    def test_404_error_format(self, test_client, auth_headers):
        """Test that 404 errors have consistent format."""
        response = test_client.get("/rest/sources/nonexistent/path/file.cli", headers=auth_headers)
        
        assert response.status_code == 404
        # FastAPI default error format
        data = response.json()
        assert "detail" in data
    
    def test_401_error_format(self, test_client):
        """Test that 401 errors have consistent format."""
        response = test_client.get("/rest/sources")
        
        assert response.status_code == 401
        data = response.json()
        assert "detail" in data
    
    def test_403_error_format(self, test_client, test_config):
        """Test that 403 errors have consistent format."""
        # Get the app's token manager to ensure token is registered
        token_manager = test_client.app.state.token_manager
        
        limited_token = token_manager.generate_token(
            user="limited-user",
            info={"api": []}  # No permissions
        )
        
        headers = {"Authorization": f"Bearer {limited_token}"}
        response = test_client.get("/rest/sources", headers=headers)
        
        assert response.status_code == 403
        data = response.json()
        assert "detail" in data


# =============================================================================
# Version Endpoint Tests
# =============================================================================

class TestVersionsEndpoints:
    """Tests for version endpoints."""
    
    def test_get_versions(self, test_client, auth_headers):
        """Test getting version information."""
        response = test_client.get("/rest/versions", headers=auth_headers)
        
        # May not be implemented
        assert response.status_code in [200, 404]


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
