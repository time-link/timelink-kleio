"""Configuration for the Kleio server and translator.

Reads configuration from environment variables with sensible defaults.
"""
from __future__ import annotations
import os
from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class KleioConfig:
    """Configuration for the Kleio server."""
    
    # Core paths
    home_dir: Path = field(default_factory=lambda: Path(os.environ.get(
        "KLEIO_HOME_DIR", Path.home() / "kleio-home"
    )))
    
    # Server settings
    host: str = "0.0.0.0"
    port: int = int(os.environ.get("KLEIO_PORT", "8088"))
    debug: bool = os.environ.get("KLEIO_DEBUG", "").lower() in ("true", "1", "yes")
    
    # Authentication
    admin_token: str = os.environ.get("KLEIO_ADMIN_TOKEN", "")
    
    # CORS
    cors_origins: list[str] = field(default_factory=lambda: [
        o.strip() for o in os.environ.get("KLEIO_CORS_SITES", "*").split(",")
    ])
    
    # Translation settings
    max_errors: int = int(os.environ.get("KLEIO_MAX_ERRORS", "100"))
    max_workers: int = int(os.environ.get("KLEIO_MAX_WORKERS", "4"))

    @property
    def sources_dir(self) -> Path:
        """Get the sources directory path."""
        return self.home_dir / "sources"

    @property
    def structures_dir(self) -> Path:
        """Get the structures directory path."""
        return self.home_dir / "structures"

    @property
    def mappings_dir(self) -> Path:
        """Get the mappings directory path."""
        return self.home_dir / "mappings"

    @property
    def system_dir(self) -> Path:
        """Get the system directory path."""
        return self.home_dir / "system"

    @property
    def users_dir(self) -> Path:
        """Get the users directory path."""
        return self.home_dir / "users"

    @property
    def tokens_dir(self) -> Path:
        """Get the tokens directory path."""
        return self.system_dir / "conf" / "kleio" / "tokens"

    def ensure_dirs(self) -> None:
        """Create required directories if they don't exist."""
        for d in [self.sources_dir, self.structures_dir, self.mappings_dir,
                  self.system_dir, self.users_dir]:
            d.mkdir(parents=True, exist_ok=True)

    @classmethod
    def from_env(cls) -> "KleioConfig":
        """Create configuration from environment variables.
        
        Returns:
            A new KleioConfig instance.
        """
        return cls()
