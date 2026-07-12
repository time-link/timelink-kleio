---
kind: build_system
name: Makefile-driven Docker build, versioning & test harness
category: build_system
scope:
    - '**'
source_files:
    - Makefile
    - Dockerfile
    - .env-sample
    - kleio.version.number
    - kleio.patch.number
    - kleio.build.number
    - build-number
    - yaml_reorder.py
    - tests/docker-compose.yaml
    - tests/scripts/env_tests.sh
    - tests/scripts/prepare_tests.sh
    - tests/scripts/run_tests.sh
    - tests/scripts/redo_run_tests.sh
    - tests/scripts/kleio_start_server.sh
    - tests/scripts/kleio_translate_local.sh
    - tests/scripts/kleio_translate_remote.sh
---

The project uses a Makefile-centric build system centered on Docker image creation for the Kleio Prolog server. There is no traditional compiler toolchain; instead, the build consists of preparing source files with injected version metadata and assembling them into container images.

Versioning model:
- Three plain-number files at the repo root hold the version components: kleio.version.number, kleio.patch.number, kleio.build.number. A helper script ./build-number -f <file> reads or increments these values.
- The Makefile composes them into major.minor (version) and major.minor.build (patch/build string).
- During make prepare, three placeholder tokens — @@VERSION@@, @@BUILD@@, @@DATE@@ — are substituted in .build/src/sources-structure.yaml, .build/src/topLevel.pl, and .build/Dockerfile so that the running server reports its exact build identity.

Build targets:
- make prepare copies src/ into .build/, performs token substitution, and prepares a clean Docker context.
- make build-local builds a local kleio-server image tagged with the computed patch number plus latest.
- make build-multi uses docker buildx to push multi-arch (linux/arm64,linux/amd64) images to timelinkserver/kleio-server:<patch>.
- Tagging helpers: tag-local-latest, tag-local-stable, tag-multi-latest, tag-multi-stable (the latter also creates a git tag matching the patch number).
- make pull-tag tag=x.y.z pulls a published image and retags it locally.

Runtime orchestration via docker-compose:
- tests/docker-compose.yaml defines a single kleio service based on the kleio-server image, exposing port 8088 and mounting ${KLEIO_HOME} as /kleio-home.
- The top-level Makefile provides convenience targets kleio-run-latest, kleio-run-current, kleio-run-tag, and kleio-stop that source .env, compute KLEIO_USER, and invoke docker compose up -d.
- .env-sample documents every KLEIO_* environment variable consumed by the server (ports, workers, idle timeout, admin token, home directory layout).

Testing harness:
- Semantic tests live under tests/scripts/ and are orchestrated through make test-semantics / redo-test-semantics.
- tests/scripts/env_tests.sh centralizes all paths: KLEIO_HOME=kleio-home, REFERENCE_SOURCES, TEST_TRANSLATIONS, STABLE_CODE_DIR=stable, DEV_CODE_DIR=dev, TRANSLATOR_SOURCE=../src/.
- prepare_tests.sh cleans output dirs, copies current src/ into tests/dev/, and seeds both reference and test translation trees from tests/kleio-home/sources/reference_sources.
- Two execution modes exist:
  - Local: kleio_translate_local.sh invokes swipl -f <start>.pl -- -sf <structure> -df <file> for each .cli/.kleio/.CLI file.
  - Remote: kleio_start_server.sh boots swipl -f serverStart.pl -g debug_server_until_idle; kleio_translate_remote.sh then calls the REST API (/rest/translations/<path>?token=...&recurse=yes) against the running server.
- compare_test_results.sh diffs outputs between stable and dev runs; redo_run_tests.sh skips the stable baseline and only re-translates with the dev code.
- API contract tests use Newman against Postman collections in api/postman/tests.json, driven by make test-api.

Auxiliary tooling:
- yaml_reorder.py enforces deterministic key ordering in structure YAMLs (priority keys name, description, source first), invoked via make yaml-format FILES='...'.
- make yaml-stru-cpy copies canonical structures from tests/kleio-home/structures/*.yaml back into src/stru.
- make bootstrap-token generates an admin token and registers it against a freshly started server using the JSON-RPC endpoints.

Conventions developers should follow:
- Never edit kleio.version.number, kleio.patch.number, or kleio.build.number by hand; use make inc-major, inc-minor, inc-build.
- All version-sensitive strings must use the @@VERSION@@ / @@BUILD@@ / @@DATE@@ placeholders; they will be replaced during make prepare.
- Keep runtime configuration in .env (copied from .env-sample) and never hard-code ports/tokens in scripts.
- When adding new structure definitions, update them under tests/kleio-home/structures/ and run make yaml-stru-cpy to propagate to src/stru.
- Run semantic tests from the repo root via make test-semantics; use redo-test-semantics after changing only the dev code to avoid re-baselining.
- For multi-arch releases, ensure docker login has been performed before make build-multi.