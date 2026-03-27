# Getting Started

<cite>
**Referenced Files in This Document**
- [README.md](file://README.md)
- [.env-sample](file://.env-sample)
- [docker-compose.yaml](file://docker-compose.yaml)
- [Dockerfile](file://Dockerfile)
- [Makefile](file://Makefile)
- [.install.sh](file://.install.sh)
- [src/serverStart.pl](file://src/serverStart.pl)
- [src/restServer.pl](file://src/restServer.pl)
- [src/apiTokens.pl](file://src/apiTokens.pl)
- [src/apiTranslations.pl](file://src/apiTranslations.pl)
- [api/postman/api.json](file://api/postman/api.json)
- [api/postman/environment.json](file://api/postman/environment.json)
- [api/postman/tests.postman_environment.json](file://api/postman/tests.postman_environment.json)
</cite>

## Table of Contents
1. [Introduction](#introduction)
2. [System Requirements](#system-requirements)
3. [Installation Approaches](#installation-approaches)
4. [Running the Server Locally](#running-the-server-locally)
5. [Environment Variables](#environment-variables)
6. [Accessing the API](#accessing-the-api)
7. [Basic API Usage Examples](#basic-api-usage-examples)
8. [Practical Examples](#practical-examples)
9. [Troubleshooting Guide](#troubleshooting-guide)
10. [Conclusion](#conclusion)

## Introduction
This guide helps you quickly set up and use the Timelink Kleio server. It covers:
- System requirements and recommended tools
- Multiple installation approaches: Docker, local SWI-Prolog, and development setup
- How to run the server locally, configure environment variables, and access the API
- Practical examples for authentication, uploading source files, and requesting translations
- Troubleshooting and environment-specific considerations

## System Requirements
- SWI-Prolog runtime to run the server locally
- Docker for containerized deployment
- Recommended developer tools:
  - VSCode with the VSC-Prolog extension for editing and debugging
  - Postman for API exploration and testing
- Optional: Newman CLI for running Postman test collections from the command line

These tools are referenced in the project’s documentation and development setup.

**Section sources**
- [README.md](file://README.md#L149-L160)

## Installation Approaches
You can deploy the Kleio server using one of the following methods:

### Option A: Docker Deployment (recommended for most users)
- Pull or build the image and run via Docker Compose
- Configure environment variables using a .env file
- Expose the server on a desired host port

Key references:
- Docker Compose configuration and environment variables
- Makefile targets for running the server with Docker
- Sample environment variables

**Section sources**
- [README.md](file://README.md#L68-L146)
- [docker-compose.yaml](file://docker-compose.yaml#L1-L22)
- [.env-sample](file://.env-sample#L1-L119)
- [Makefile](file://Makefile#L161-L217)

### Option B: Local SWI-Prolog Installation
- Install SWI-Prolog locally
- Open the server startup file in VSCode and load it
- Set environment variables and start the server in debug mode

**Section sources**
- [README.md](file://README.md#L162-L179)
- [src/serverStart.pl](file://src/serverStart.pl#L19-L26)

### Option C: Development Environment Setup
- Use VSCode with VSC-Prolog
- Optionally run the server inside the Docker container for debugging
- Configure MHK integration if needed

**Section sources**
- [README.md](file://README.md#L179-L240)

## Running the Server Locally
Follow these steps to run the server locally for development and debugging:

1. Install SWI-Prolog and VSCode with VSC-Prolog
2. Open the server startup file in VSCode
3. Load the file in the Prolog REPL
4. Set the admin token and start the debug server

Notes:
- The local server does not read the .env file
- Use spy points to debug predicates
- Tokens configured this way can be used by clients

**Section sources**
- [README.md](file://README.md#L162-L179)
- [src/serverStart.pl](file://src/serverStart.pl#L19-L26)

## Environment Variables
Configure the server using environment variables. The server reads the following variables at runtime:

- KLEIO_HOME_DIR: Base working directory for the server
- KLEIO_SOURCE_DIR: Directory for source files to be translated
- KLEIO_CONF_DIR: Directory for configuration files and tokens
- KLEIO_STRU_DIR: Directory for global structure files
- KLEIO_DEFAULT_STRU: Default structure file used by default
- KLEIO_TOKEN_DB: Path to the token database
- KLEIO_SERVER_PORT: Port for the REST server (default 8088)
- KLEIO_SERVER_WORKERS: Number of worker threads
- KLEIO_IDLE_TIMEOUT: Connection idle timeout
- KLEIO_DEBUG: Enable debug logs
- KLEIO_ADMIN_TOKEN: Admin token with full privileges
- KLEIO_CORS_SITES: Allowed origins for CORS

For Docker Compose, variables are passed from the .env file and mapped into the container.

**Section sources**
- [src/restServer.pl](file://src/restServer.pl#L107-L119)
- [.env-sample](file://.env-sample#L52-L119)
- [docker-compose.yaml](file://docker-compose.yaml#L16-L21)

## Accessing the API
The server exposes:
- REST endpoints for file operations and metadata
- JSON-RPC endpoints for token and user management

Endpoints overview:
- REST: GET/POST/DELETE operations on files, directories, and translation status
- JSON-RPC: Methods for token generation/invalidation and user management

Reference collections and environments for Postman are included in the repository.

**Section sources**
- [README.md](file://README.md#L50-L62)
- [api/postman/api.json](file://api/postman/api.json#L1-L200)
- [api/postman/environment.json](file://api/postman/environment.json#L1-L109)
- [api/postman/tests.postman_environment.json](file://api/postman/tests.postman_environment.json#L1-L119)

## Basic API Usage Examples
Below are practical examples of common tasks using the API. Replace placeholders with your actual values.

### 1) Authentication with Tokens
- Obtain an admin token (either generated at startup or provided via environment variable)
- Use the token in the Authorization header for bearer authentication

References:
- Token generation and invalidation endpoints
- Example JSON-RPC calls in the Postman collection

**Section sources**
- [README.md](file://README.md#L92-L112)
- [src/apiTokens.pl](file://src/apiTokens.pl#L41-L88)
- [api/postman/api.json](file://api/postman/api.json#L11-L130)

### 2) Uploading Source Files
- Use the files endpoint to upload source files
- Ensure the target directory is within the configured source directory

References:
- Files API endpoints and directory management
- Postman environment variables for endpoints and tokens

**Section sources**
- [api/postman/api.json](file://api/postman/api.json#L1-L200)
- [api/postman/environment.json](file://api/postman/environment.json#L1-L109)

### 3) Requesting Translations
- Call the translations endpoint to start translation of a file or directory
- Optionally enable recursion and echo inclusion
- Retrieve translation status and results

References:
- Translation API methods and parameters
- Status caching behavior and filtering

**Section sources**
- [src/apiTranslations.pl](file://src/apiTranslations.pl#L34-L82)
- [src/apiTranslations.pl](file://src/apiTranslations.pl#L86-L122)

## Practical Examples
### Example A: Start the Server with Docker
- Copy the sample environment file to .env and adjust variables
- Use Makefile targets to run the server with Docker Compose

Steps:
- Prepare environment variables
- Build or pull the image
- Start the service with docker compose

**Section sources**
- [README.md](file://README.md#L125-L146)
- [Makefile](file://Makefile#L161-L217)
- [.env-sample](file://.env-sample#L1-L119)

### Example B: Local Development with VSCode
- Install SWI-Prolog and VSC-Prolog
- Load the server startup file in VSCode
- Set the admin token and run the debug server

**Section sources**
- [README.md](file://README.md#L162-L179)
- [src/serverStart.pl](file://src/serverStart.pl#L19-L26)

### Example C: Using Postman Collections
- Import the Postman collection and environment files
- Configure the endpoint and token variables
- Run requests to test authentication, uploads, and translations

**Section sources**
- [README.md](file://README.md#L153-L159)
- [api/postman/api.json](file://api/postman/api.json#L1-L200)
- [api/postman/environment.json](file://api/postman/environment.json#L1-L109)
- [api/postman/tests.postman_environment.json](file://api/postman/tests.postman_environment.json#L1-L119)

## Troubleshooting Guide
Common issues and resolutions:

- Permission errors on Linux with Docker
  - Run the container under the current user to avoid root-owned files
  - Reference: user override in Docker Compose

- Admin token not set
  - If not provided, the server generates an admin token at startup
  - Retrieve it from the configuration directory

- CORS issues
  - Configure allowed origins via KLEIO_CORS_SITES

- Long-running translation timeouts
  - Increase KLEIO_IDLE_TIMEOUT for large XML exports

- Running the server locally without .env
  - The local debug server does not read .env; set variables manually in the Prolog REPL

**Section sources**
- [README.md](file://README.md#L113-L124)
- [README.md](file://README.md#L125-L146)
- [README.md](file://README.md#L174-L179)
- [src/restServer.pl](file://src/restServer.pl#L183-L184)
- [docker-compose.yaml](file://docker-compose.yaml#L7-L10)

## Conclusion
You now have multiple paths to deploy and use the Timelink Kleio server:
- Docker for quick, reproducible deployments
- Local SWI-Prolog for development and debugging
- Postman and Newman for API exploration and automation

Use the environment variables to tailor the server to your setup, and refer to the API documentation and Postman collections for hands-on examples.