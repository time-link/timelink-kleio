# Timelink-Kleio - Developer Documentation

## Project Overview

**Timelink-Kleio** is a specialized translation service for historical document processing, specifically designed to handle _Kleio_ files - a notation system created by Manfred Thaller for transcribing historical sources.

## Core Purpose

The project provides an API server that translates Kleio source files into structured data suitable for import into the _Timelink_ relational database system. Timelink is designed for handling person-oriented information from historical documents.

## Key Features

### Translation Engine
- **Intelligent normalization**: Processes historical source information and infers context to reduce data entry overhead
- **Historical document processing**: Handles complex historical documents like baptism records, marriage certificates, etc.
- **Linked data support**: Integrates with external sources like Wikidata through annotation system
- **Complex date handling**: Processes date ranges and relative dates with rich metadata

### API Services
- **Translation services**: Convert Kleio files to structured XML/JSON
- **File management**: Upload, download, delete, and organize source files
- **Git integration**: Basic version control operations
- **Token-based authentication**: Secure permission management
- **Source inspection**: Error checking and validation

## Technical Architecture

### Language & Platform
- **Primary language**: SWI-Prolog (88% of codebase)
- **Containerized**: Docker-based deployment
- **REST API**: JSON-based web services

### Project Structure
```
├── src/                 # Core Prolog source files (422K+ lines)
├── tests/              # Comprehensive test suites
├── docs/               # API documentation
├── api/                # Postman collections for API testing
├── .build/             # Build artifacts
└── configuration files
```

### Key Components
- **Translation engine** (`inference.pl`, `gacto.pl`)
- **API server** (`restServer.pl`, `serverStart.pl`)
- **Data processing** (`dataDictionary.pl`, `dataCode.pl`)
- **File management** (`kleioFiles.pl`, `gitUtilities.pl`)

## Development & Testing

### Development Tools
- **VSCode with VSC-Prolog** for development
- **Postman** for API testing
- **Docker** for containerization
- **Makefile** with comprehensive build targets

### Testing Strategy
- **Semantic tests**: Compare translations against reference outputs
- **API tests**: Verify REST endpoints functionality
- **Multi-environment testing**: Local, Docker, and integration testing

## Deployment & Configuration

### Docker Deployment
```bash
docker run -v $(PWD):/kleio-home -p 8088:8088 -d timelink-server/kleio-server
```

### Configuration Options
- **Environment variables**: `KLEIO_ADMIN_TOKEN`, `KLEIO_HOME`, `KLEIO_DEBUG`
- **Token management**: Automatic bootstrap token generation
- **CORS support**: Configurable cross-origin resource sharing

## Historical Context

The project represents a continuation of Manfred Thaller's Kleio historical database system, focusing specifically on the powerful notation system for historical source transcription while integrating with modern web technologies and database systems.

## Current Status

The project appears to be actively maintained with recent releases (latest version 12.9.588 from June 2025), regular bug fixes, and feature enhancements. It serves as a critical component in the Timelink ecosystem for processing historical documents into structured, queryable data.

## Development Workflow

### Running the Server Locally for Debugging
```prolog
% In SWI-Prolog terminal
setenv('KLEIO_ADMIN_TOKEN','mytoken').
setup_and_run_server(run_debug_server,[port(8089)]).
```

### Building and Testing
```bash
# Build local image
make build-local

# Run semantic tests
make test-semantics

# Run API tests
make test-api

# Generate documentation
make docs
```

### Debugging with MHK
When debugging with the MHK system:
1. Set `mhk.kleio.service` to `http://host.docker.internal:8088` in MHK configuration
2. Start local Kleio server in VSCode
3. Use `run_from_mhk_home/2` predicate for integration

## Codebase Statistics
- **Total files**: 274
- **Total lines**: 422,374
- **Primary languages**: YAML (38%), JSON (36%), Perl (19%), HTML (5%)
- **Core Prolog modules**: 50+ files in `src/` directory

## Release Management

### Versioning
- Uses semantic versioning (major.minor.patch)
- Build numbers tracked for development versions
- Stable releases tagged with version numbers

### Release Process
1. Build multi-platform Docker image
2. Run comprehensive tests
3. Update version with `make inc-major` or `make inc-minor`
4. Tag and push to Docker repository
5. Update release notes

## API Documentation

Comprehensive API documentation is available at `docs/api/index.html` with detailed endpoint specifications, request/response formats, and authentication requirements.
