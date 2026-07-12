---
kind: external_dependency
name: Container Deployment Platform
slug: docker
category: external_dependency
category_hints:
    - vendor_identity
scope:
    - '**'
---

### Docker Containerization
- Multi-platform Docker builds using `make build-multi` targeting `timelinkserver/kleio-server` repository
- Development uses `docker-compose.yaml` for local testing with volume mounting
- Production images tagged with semantic versioning (latest, stable, version-specific tags)
- Container exposes HTTP API on port 8088 with configurable admin token via `KLEIO_ADMIN_TOKEN` environment variable
- Supports running under current user context to avoid permission issues