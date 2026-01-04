# Installation and Setup

<cite>
**Referenced Files in This Document**   
- [README.md](file://README.md)
- [.env-sample](file://.env-sample)
- [Dockerfile](file://Dockerfile)
- [docker-compose.yaml](file://docker-compose.yaml)
- [Makefile](file://Makefile)
- [.kleio.json](file://.kleio.json)
- [improved_zshrc](file://improved_zshrc)
- [README_shell_config.md](file://README_shell_config.md)
- [src/serverStart.pl](file://src/serverStart.pl)
- [src/restServer.pl](file://src/restServer.pl)
</cite>

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [Docker-Based Deployment](#docker-based-deployment)
3. [Local Development Setup](#local-development-setup)
4. [Environment Configuration](#environment-configuration)
5. [Runtime Configuration](#runtime-configuration)
6. [Shell Configuration](#shell-configuration)
7. [Starting the Server](#starting-the-server)
8. [Verification and Health Checks](#verification-and-health-checks)
9. [Troubleshooting Common Issues](#troubleshooting-common-issues)

## Prerequisites

Before setting up the timelink-kleio environment, ensure the following prerequisites are installed on your system:

- **SWI-Prolog**: The core runtime environment for timelink-kleio, as the server is implemented in SWI-Prolog. Install from [https://www.swi-prolog.org](https://www.swi-prolog.org).
- **Docker**: Required for containerized deployment. Install Docker Desktop or Docker Engine from [https://www.docker.com](https://www.docker.com).
- **Git**: Necessary for version control and repository management. Install from [https://git-scm.com](https://git-scm.com).
- **Make**: Used to execute build and deployment targets defined in the Makefile. Ensure GNU Make is available in your system PATH.

These tools form the foundation for both Docker-based and local development setups, enabling seamless operation of the timelink-kleio server.

**Section sources**
- [README.md](file://README.md#L149-L160)

## Docker-Based Deployment

The timelink-kleio server can be deployed using Docker for isolated and consistent execution. Two primary methods are available: running pre-built images from Docker Hub or building a local Docker image.

### Running from Docker Hub

To deploy the latest version directly from Docker Hub, execute the following command:

```bash
docker run -v $(PWD):/kleio-home -p 8088:8088 -d timelinkserver/kleio-server
```

This command mounts the current directory as `/kleio-home` inside the container, maps port 8088 for API access, and runs the container in detached mode.

### Building and Running Locally

To build and run a local Docker image, use the provided Makefile targets:

```bash
make build-local
docker run -v $PWD:/kleio-home -p 8088:8088 -d kleio-server
```

The `build-local` target compiles a Docker image tagged with a sequential build number, latest, and patch version, ensuring version control for local development.

### Customizing Docker Configuration

For advanced configuration, utilize Docker Compose with the `docker-compose.yaml` file. This approach allows setting environment variables via a `.env` file, enabling customization of image tags, ports, and admin tokens. The `docker-compose.yaml` file defines the service configuration, including volume mounting, port mapping, and environment variable injection, supporting multi-platform builds for ARM64 and AMD64 architectures.

**Section sources**
- [README.md](file://README.md#L68-L145)
- [Dockerfile](file://Dockerfile#L1-L21)
- [docker-compose.yaml](file://docker-compose.yaml#L1-L21)
- [Makefile](file://Makefile#L102-L127)

## Local Development Setup

For development and debugging, the timelink-kleio server can be run locally without Docker, leveraging SWI-Prolog directly.

### Prerequisites for Local Execution

Ensure SWI-Prolog is installed and accessible in your system PATH. Recommended development tools include:
- **VSCode** with the **VSC-Prolog** extension for editing and debugging Prolog code.
- **Postman** for testing API endpoints, with collections provided in the `api/postman/` directory.

### Starting the Server Locally

To run the server locally for debugging:
1. Install SWI-Prolog and the VSC-Prolog extension in VSCode.
2. Open `serverStart.pl` in VSCode and load the file.
3. In the Prolog terminal, execute:
   ```prolog
   setenv('KLEIO_ADMIN_TOKEN','mytoken').
   setup_and_run_server(run_debug_server,[port(8089)]).
   ```
This starts the server in debug mode on port 8089, with the specified admin token for authentication.

### Debugging with MHK Integration

For integrated debugging with MHK (Manfred Thaller's Historical Knowledge base), stop the Kleio server started with MHK and run a local server associated with the MHK home directory. Set the `mhk.kleio.service` property in `mhk-home/system/conf/mhk_system.properties` to `http://host.docker.internal:8088` to enable communication between MHK and the local Kleio server.

**Section sources**
- [README.md](file://README.md#L162-L240)
- [src/serverStart.pl](file://src/serverStart.pl#L13-L67)

## Environment Configuration

The timelink-kleio environment is configured using environment variables, primarily managed through the `.env-sample` file and Docker Compose.

### Configuration via .env File

Copy `.env-sample` to `.env` and customize the variables as needed. Key configuration options include:
- **KLEIO_SERVER_IMAGE**: Specifies the Docker image to run (default: `timelinkserver/kleio-server:stable`).
- **KLEIO_ADMIN_TOKEN**: Sets the admin token for API access; if unset, a token is generated and stored in `.kleio.json`.
- **KLEIO_HOME_DIR**: Defines the root working directory for the server, mapped to `/kleio-home` in the container.
- **KLEIO_SERVER_PORT** and **KLEIO_EXTERNAL_PORT**: Configure the internal and exposed server ports, respectively.
- **KLEIO_CORS_SITES**: Sets allowed sites for CORS, with `*` permitting all origins.

### Environment Variable Usage

The `.env` file is read by Docker Compose to set environment variables before launching the container. For standalone execution, scripts can source the `.env` file to configure the environment. The Makefile targets such as `kleio-run-latest` automate this process, ensuring consistent configuration across deployments.

**Section sources**
- [.env-sample](file://.env-sample#L1-L119)
- [README.md](file://README.md#L130-L145)
- [Makefile](file://Makefile#L160-L170)

## Runtime Configuration

Runtime behavior is influenced by configuration files and environment variables that dictate server operation and file management.

### Directory Structure Initialization

The server expects a specific directory structure within `KLEIO_HOME_DIR`:
- **sources/**: Base directory for Kleio source files to be translated.
- **system/conf/kleio/**: Configuration files, including token database and structure files.
- **users/**: Base directory for user-specific configuration files.

If the server detects an MHK_HOME structure, it defaults configuration paths accordingly, facilitating integration with MHK installations.

### Configuration via .kleio.json

Upon startup, the server generates a `.kleio.json` file containing runtime configuration, including the admin token, server URL, and version information. This file serves as a reference for the current server state and can be used to verify successful startup.

```json
{
  "kleio_admin_token": "2e5cb59a7d1765d91b1916c8ffd7fce6bc769145",
  "kleio_url": "http://localhost:8088",
  "kleio_version": "11.0"
}
```

**Section sources**
- [.kleio.json](file://.kleio.json#L1-L13)
- [README.md](file://README.md#L457-L459)
- [src/restServer.pl](file://src/restServer.pl#L228-L267)

## Shell Configuration

Proper shell configuration ensures that necessary tools and paths are accessible, enhancing development efficiency.

### Optimizing Shell Environment

The repository includes `improved_zshrc` and `README_shell_config.md` to guide shell setup. Key recommendations include:
- Moving PYENV_ROOT and PATH setup to `.zprofile` for login shells to avoid redundancy.
- Keeping SWI-Prolog and pip user bin paths in `.zshrc` for interactive shells.
- Setting SSL certificate handling via `SSL_CERT_FILE` to ensure secure connections.

### Example Configuration

The `improved_zshrc` file configures the shell with:
- SWI-Prolog path: `/Applications/SWI-Prolog.app/Contents/MacOS`
- Pip user bin directory: `/Users/jrc/.local/bin`
- SSL certificate file: dynamically set using Python's certifi module.

This setup ensures that Prolog, Python, and related tools are readily available in the development environment.

**Section sources**
- [improved_zshrc](file://improved_zshrc#L1-L12)
- [README_shell_config.md](file://README_shell_config.md#L1-L93)

## Starting the Server

The server can be started using Makefile targets, which streamline the deployment process.

### Using Makefile Targets

The Makefile provides several targets for server management:
- **kleio-run-latest**: Starts the server with the latest multi-platform image, using `.env` configuration.
- **kleio-run-current**: Launches the server with the most recent local build.
- **kleio-run-tag**: Runs the server with a specific image tag, useful for version testing.

To start the server, ensure a `.env` file exists and execute:
```bash
make kleio-run-latest
```

This command pulls the latest image, configures environment variables, and starts the container using Docker Compose.

### Stopping the Server

To stop the running server, use:
```bash
make kleio-stop
```

This target stops the Docker Compose services, ensuring clean shutdown and resource release.

**Section sources**
- [Makefile](file://Makefile#L160-L215)

## Verification and Health Checks

After starting the server, verify its operation through startup logs and health checks.

### Successful Startup Logs

Upon successful startup, the server outputs configuration details, including:
- Version, debug mode, REST port, workers, and timeout settings.
- Paths for `kleio_home_dir`, `kleio_conf_dir`, and `kleio_source_dir`.
- Admin token (partially masked) and token database status.

Example log output:
```
Version: 12.9.588
Debug mode: false
REST port: 8088 (check if mapped in docker)
Workers: 3
Timeout: 900
/kleio_home dir on host: 
Kleio_admin_token: No
Token db status: Tokens exist
```

### Health Check Procedures

To verify server health:
1. Check the `.kleio.json` file for the admin token and server URL.
2. Access the API documentation at `docs/api/index.html` to confirm endpoint availability.
3. Use Postman collections in `api/postman/` to test API endpoints, ensuring responses are as expected.

These steps confirm that the server is operational and ready for use.

**Section sources**
- [src/restServer.pl](file://src/restServer.pl#L186-L226)
- [README.md](file://README.md#L62-L67)

## Troubleshooting Common Issues

Address common setup issues with the following guidance.

### Port Conflicts

If port 8088 is in use, change the `KLEIO_EXTERNAL_PORT` in the `.env` file or use a different port in the Docker run command:
```bash
docker run -p 8089:8088 -v $PWD:/kleio-home -d kleio-server
```

### Permission Errors

On Linux systems, Docker runs as root, causing permission issues for generated files. Run the container with the current user's UID and GID:
```bash
docker run -v $PWD:/kleio-home -u $(id -u):$(id -g) -p 8088:8088 -d kleio-server
```

### Dependency Resolution

Ensure all prerequisites (SWI-Prolog, Docker, Git) are correctly installed and accessible in the PATH. For missing dependencies, refer to the installation guides provided in the documentation.

### Token Generation Issues

If no admin token is set and the token database is empty, the server generates a bootstrap token valid for 5 minutes. Use this token to generate a new one via the API, or set `KLEIO_ADMIN_TOKEN` in the `.env` file to avoid bootstrap reliance.

**Section sources**
- [README.md](file://README.md#L114-L123)
- [src/restServer.pl](file://src/restServer.pl#L270-L292)