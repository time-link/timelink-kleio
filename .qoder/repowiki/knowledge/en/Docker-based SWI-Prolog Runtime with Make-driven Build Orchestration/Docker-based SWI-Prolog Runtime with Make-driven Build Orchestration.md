---
kind: dependency_management
name: Docker-based SWI-Prolog Runtime with Make-driven Build Orchestration
category: dependency_management
scope:
    - '**'
source_files:
    - Dockerfile
    - Makefile
    - docker-compose.yaml
    - .env-sample
    - tests/docker-compose.yaml
---

This repository does not use a traditional language package manager (no go.mod, package.json, requirements.txt, Cargo.toml, etc.). Instead, dependency management is centered around Docker and the SWI-Prolog runtime image, orchestrated through a top-level Makefile.

System / approach
- The only runtime dependency is the official swipl Docker image (FROM swipl in Dockerfile). All Kleio server code is SWI-Prolog; there are no Python, Node.js, Go, or Rust dependencies declared.
- System packages installed inside the image are limited to git (needed by the server's Git integration), installed via apt-get during the Docker build.
- Image tagging and multi-arch builds are handled by make build-local, make build-multi, and docker buildx; images are pushed to timelinkserver/kleio-server on Docker Hub.
- Development/test tooling (e.g., newman for Postman API tests) is documented as an external prerequisite — it is not installed by the repo's scripts.

Key files
- Dockerfile: base image selection (swipl), system package install (git), source copy, entrypoint.
- Makefile: versioning, image build/push/tag, compose orchestration, test targets, YAML helpers.
- docker-compose.yaml: container service definition, env/volume/port wiring.
- .env-sample: all runtime configuration keys (KLEIO_SERVER_IMAGE, ports, workers, tokens, paths).
- tests/docker-compose.yaml: local-test variant that uses the locally built kleio-server:latest image.

Architecture & conventions
- Single-image deployment: every release is a self-contained Docker image containing the Prolog sources plus git; consumers run it via docker compose against a mounted KLEIO_HOME_DIR.
- Versioning is file-driven: kleio.version.number, kleio.patch.number, kleio.build.number, kleio.build.date at the repo root are bumped via make inc-major|inc-minor|inc-build; the prepare target injects these values into the Dockerfile and Prolog sources before building.
- Image tag policy: build-multi pushes <patch>; tag-multi-latest aliases it to latest; tag-multi-stable additionally tags major and major.minor and creates a matching git tag.
- Local dev vs published image: .env-sample defaults to pulling timelinkserver/kleio-server:stable; developers switch to a local build by setting KLEIO_SERVER_IMAGE=kleio-server:latest and running make build-local first.
- Test harness isolation: tests/ mirrors the full src tree under tests/stable and tests/dev; semantic tests compare outputs between the two trees while the server runs from the current image.

Rules developers should follow
- Do not add new language-specific dependency manifests; if you must, document them as external prerequisites (like newman) rather than baking them into the image.
- Keep the Docker image minimal — only add apt-get packages when strictly required by the Prolog server.
- Bump versions exclusively through the make inc-* targets so the prepare step can propagate them into artifacts.
- When switching between published and local images, update KLEIO_SERVER_IMAGE in .env and ensure the image exists (make build-local or make pull-tag tag=x.y.z).
- For multi-arch releases, always use make build-multi followed by make tag-multi-latest / make tag-multi-stable.