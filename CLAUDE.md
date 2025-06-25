# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Timelink-Kleio is a translation server for Kleio notation files. It provides REST API services to translate historical source files written in Kleio notation into normalized XML data for the Timelink database system. The server is written in SWI-Prolog and runs in Docker containers.

## Key Architecture Components

### Core Translation System
- **src/serverStart.pl**: Main server entry point with debugging and production modes
- **src/restServer.pl**: HTTP REST API server implementation  
- **src/apiTranslations.pl**: Core translation API endpoints
- **src/gacto.pl**: Main translation engine for Kleio notation
- **src/struCode.pl** & **src/struSyntax.pl**: Structure file processing (defines document schemas)
- **src/yamlSupport.pl**: YAML structure file support (new alternative to .str files)

### Data Processing Pipeline
- **src/dataSyntax.pl** & **src/dataCode.pl**: Kleio notation parsing
- **src/gactoxml.pl**: XML output generation
- **src/linkedData.pl**: External data linking support (Wikidata, etc.)
- **src/persistence.pl**: Thread-safe data storage
- **src/errors.pl** & **src/reports.pl**: Error handling and reporting

### API Services
- **src/apiSources.pl**: Source file management
- **src/apiDirectories.pl**: Directory operations
- **src/apiTokens.pl**: Authentication token management
- **src/apiGit.pl**: Git operations for version control

### Structure Files
- **.str files**: Define document schemas in Kleio notation format
- **.str.yaml files**: YAML alternative format for structure definitions
- **src/str/**: New modular YAML structure components (groups.yaml, elements.yaml, system.yaml)

## Development Commands

### Local Development with SWI-Prolog
For debugging, install SWI-Prolog locally and use VSCode with VSC-Prolog extension:

1. Open `src/serverStart.pl` in VSCode
2. Load file with `Option+X+L`
3. In Prolog terminal:
   ```prolog
   setenv('KLEIO_ADMIN_TOKEN','mytoken').
   run_debug_server.
   ```

### Docker Development Commands

```bash
# Build local docker image
make build-local

# Run server with latest image
make kleio-run-latest

# Run server with current build
make kleio-run-current  

# Stop server
make kleio-stop

# Generate admin token
make gen-token
```

### Testing Commands

```bash
# Run semantic tests (compares translation outputs)
make test-semantics

# Run API tests (requires newman: npm install -g newman)
make test-api

# View test results
cat reports/test_report_*.diff
```

### Version Management

```bash
# Show current version info
make show-current
make show-last

# Increment version
make inc-major
make inc-minor

# Build and push multi-platform images
make build-multi
make tag-multi-stable
make tag-multi-latest
```

## File Structure Conventions

### Source Files (.cli)
- Located in `tests/kleio-home/sources/` hierarchy
- Use Kleio notation for historical document transcription
- Require corresponding .str or .str.yaml structure files

### Structure Files
- **Traditional**: `.str` files define document schemas in Kleio notation
- **Modern**: `.str.yaml` files use YAML format for better tooling
- **Modular**: `src/str/` contains reusable YAML components

### Testing Structure
- `tests/kleio-home/`: Complete Kleio installation for testing
- `tests/stable/`: Reference translator version for semantic tests
- `tests/scripts/`: Test automation scripts

## Environment Configuration

Key environment variables (set in `.env` file):
- `KLEIO_ADMIN_TOKEN`: Admin authentication token
- `KLEIO_HOME_DIR`: Base directory for Kleio files
- `KLEIO_SERVER_PORT`: Internal server port (default: 8088)
- `KLEIO_EXTERNAL_PORT`: External port mapping (default: 8089)
- `KLEIO_DEBUG`: Enable debug logging if "true"

## Important Development Notes

- Server uses thread-safe data structures (non-standard Prolog)
- Translation results are deterministic - semantic tests ensure output consistency
- Structure files define both data schema and processing rules
- API uses both REST and JSON-RPC formats
- Docker containers run under specific user permissions for file access

## Debugging and Testing

### Unit Testing
1. Create test files in `tests/kleio-home/sources/api/issues/`
2. Add `translate_file('sources/api/issues/yourfile.cli', true).` to `src/serverStart.pl`
3. Run `run_tests(server).` in Prolog terminal
4. Use `tspy(predicate_name)` for breakpoints in threaded code

### API Testing
- Use Postman collections in `api/postman/`
- Run `make test-api` for automated testing
- Debug with VSCode while running API tests

The server provides comprehensive translation services with robust testing infrastructure to ensure consistent output across versions.