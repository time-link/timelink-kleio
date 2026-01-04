# Development and Testing

<cite>
**Referenced Files in This Document**   
- [Makefile](file://Makefile)
- [tests/README.md](file://tests/README.md)
- [tests/scripts/run_tests.sh](file://tests/scripts/run_tests.sh)
- [tests/scripts/prepare_tests.sh](file://tests/scripts/prepare_tests.sh)
- [tests/scripts/compare_test_results.sh](file://tests/scripts/compare_test_results.sh)
- [tests/scripts/kleio_translate_local.sh](file://tests/scripts/kleio_translate_local.sh)
- [tests/scripts/kleio_translate_remote.sh](file://tests/scripts/kleio_translate_remote.sh)
- [tests/scripts/kleio_start_server.sh](file://tests/scripts/kleio_start_server.sh)
- [tests/scripts/kleio_stop_server.sh](file://tests/scripts/kleio_stop_server.sh)
- [tests/scripts/exclude_while_comparing.grep](file://tests/scripts/exclude_while_comparing.grep)
- [tests/docker-compose.yaml](file://tests/docker-compose.yaml)
- [api/postman/tests.json](file://api/postman/tests.json)
- [api/postman/tests.postman_environment.json](file://api/postman/tests.postman_environment.json)
</cite>

## Table of Contents
1. [Development Environment Setup](#development-environment-setup)
2. [Running Tests with Makefile Targets](#running-tests-with-makefile-targets)
3. [Test Suite Organization](#test-suite-organization)
4. [API Testing with Postman](#api-testing-with-postman)
5. [Test Reports and Results](#test-reports-and-results)
6. [Writing New Tests](#writing-new-tests)
7. [CI/CD and Code Coverage](#cicd-and-code-coverage)
8. [Performance Benchmarking](#performance-benchmarking)
9. [Debugging and Troubleshooting](#debugging-and-troubleshooting)

## Development Environment Setup

To set up a local development environment for timelink-kleio, follow these steps:

1. Clone the repository and navigate to the project root directory.
2. Copy the `.env-sample` file to `.env` and configure the environment variables as needed.
3. Ensure Docker and Docker Compose are installed on your system.
4. Install SWI-Prolog, which is required for running local tests.
5. Install Newman (npm install -g newman) for running API tests.

The development environment uses Docker containers to ensure consistency across different systems. The `docker-compose.yaml` file in the tests directory configures the container with appropriate volume mounts and environment variables.

**Section sources**
- [Makefile](file://Makefile#L1-L278)
- [.env-sample](file://.env-sample)
- [tests/docker-compose.yaml](file://tests/docker-compose.yaml#L1-L20)

## Running Tests with Makefile Targets

The Makefile provides several targets for running tests:

- `make test-semantics`: Runs semantic tests by comparing the output of the current development version with a stable reference version.
- `make test-api`: Runs API tests using Newman against a locally running server instance.
- `make current-to-stable`: Copies the current code from src to tests/stable, making it the new reference version for semantic tests.

To run semantic tests, execute `make test-semantics` from the repository root. This command runs the `run_tests.sh` script in the tests directory, which:
1. Prepares the test environment by copying source files and setting up directories
2. Translates reference sources with the stable translator
3. Translates the same sources with the development version (server mode)
4. Compares the results and generates a report

For API tests, use `make test-api`, which starts a local server and runs the Postman collection against it.

**Section sources**
- [Makefile](file://Makefile#L251-L263)
- [tests/scripts/run_tests.sh](file://tests/scripts/run_tests.sh#L1-L39)

## Test Suite Organization

The test suite is organized in the `tests/` directory with the following structure:

- `kleio-home/`: Simulates a complete Kleio/Timelink installation with configuration files and source data
- `stable/`: Contains the stable version of the translator code used as a reference
- `dev/`: Contains the development version of the translator code
- `scripts/`: Contains various test automation scripts
- `reports/`: Stores test result reports

The semantic tests compare the output of the translator by running both stable and development versions on the same set of reference source files. The reference sources are located in `kleio-home/sources/reference_sources/` and include various test cases organized by category.

The test scripts use environment variables to configure paths and directories, which are set in the `prepare_tests.sh` script. This script also copies the current development code to the `dev/` directory and sets up the test environment.

**Section sources**
- [tests/README.md](file://tests/README.md#L1-L200)
- [tests/scripts/prepare_tests.sh](file://tests/scripts/prepare_tests.sh#L1-L51)

## API Testing with Postman

API testing is performed using Postman collections located in the `api/postman/` directory. The main test collection is `tests.json`, which contains a sequence of requests to test the Kleio translator API.

The Postman collection includes tests for:
- User token management (creation, invalidation)
- Source file operations (upload, delete, list)
- Directory operations (create, remove)
- Translation operations
- Error handling and authorization

To run the API tests:
1. Ensure Newman is installed (`npm install -g newman`)
2. Start the Kleio server
3. Run `make test-api` from the repository root

The tests use environment variables defined in `tests.postman_environment.json`, including the endpoint URL and admin token. The collection also includes pre-request scripts and test scripts written in JavaScript to handle authentication, store tokens, and validate responses.

**Section sources**
- [api/postman/tests.json](file://api/postman/tests.json#L1-L4391)
- [api/postman/tests.postman_environment.json](file://api/postman/tests.postman_environment.json)

## Test Reports and Results

Test results are stored in the `tests/reports/` directory as diff files with timestamps in their names (e.g., `test_report_2025-12-13_11:19:25.diff`). These reports contain the output of the comparison between the stable and development translator outputs.

The comparison process uses the `compare_test_results.sh` script, which runs `diff -r` on the reference and test translation directories, filtering out expected differences using patterns defined in `exclude_while_comparing.grep`. This file contains regex patterns for lines that should be ignored in the comparison, such as:
- Timestamps and dates
- File paths containing `reference_translations` or `test_translations`
- Auto-generated IDs
- Version information
- Temporary file names

When the test output shows "Only in" messages for `.gitignore` files, this is expected and can be ignored. Full compatibility is indicated when no meaningful differences are found after filtering.

**Section sources**
- [tests/scripts/compare_test_results.sh](file://tests/scripts/compare_test_results.sh#L1-L9)
- [tests/scripts/exclude_while_comparing.grep](file://tests/scripts/exclude_while_comparing.grep#L1-L45)
- [tests/reports/](file://tests/reports/)

## Writing New Tests

To write new semantic tests:
1. Create a test data file in `tests/kleio-home/sources/reference_sources/issues/`
2. In `src/serverStart.pl`, add a clause to the `translate_file` predicate with the relative path to your new file
3. Set the second argument to `true` to activate the test
4. Run `make test-semantics` to execute the test

For API tests, new requests can be added to the Postman collection. When adding file upload tests, update the `src` parameter in the form data to point to the correct file path. The Postman collection uses environment variables to make tests more flexible.

When implementing new features that intentionally change output, you have three options:
1. Backport the change to the reference implementation
2. Make the current version the reference by running `make current-to-stable`
3. Exclude the difference in the output by adding a pattern to `exclude_while_comparing.grep`

**Section sources**
- [tests/README.md](file://tests/README.md#L76-L98)
- [src/serverStart.pl](file://src/serverStart.pl)

## CI/CD and Code Coverage

The Makefile includes targets that support CI/CD workflows:
- `make build-local`: Builds a local Docker image with a new build number
- `make build-multi`: Builds multi-platform Docker images and pushes them
- `make tag-local-stable`: Tags the local image with version numbers
- `make show-build`, `show-version`, `show-current`: Display version and build information

For code coverage analysis, the project would need to integrate with a Prolog code coverage tool, though specific configuration for this is not evident in the current codebase. The semantic testing approach provides a form of functional coverage by ensuring that changes don't affect existing functionality.

The CI/CD pipeline should:
1. Run `make test-semantics` to verify semantic compatibility
2. Run `make test-api` to verify API functionality
3. Build and tag Docker images if tests pass
4. Push images to the registry

**Section sources**
- [Makefile](file://Makefile#L102-L147)

## Performance Benchmarking

The project includes some performance considerations in its testing infrastructure:
- The server can be configured with multiple workers (`KLEIO_WORKERS`)
- Idle timeout settings (`KLEIO_IDLE_TIMEOUT`) help manage resource usage
- The test scripts measure execution time using the `time` command

To perform performance benchmarking:
1. Use the `run_tests.sh` script to measure translation times
2. Compare results between different versions
3. Monitor resource usage during testing

The semantic tests provide a baseline for performance comparison, as changes that significantly impact translation speed would be noticeable in the timing output. For more detailed performance analysis, additional profiling tools for Prolog would be needed.

**Section sources**
- [tests/scripts/run_tests.sh](file://tests/scripts/run_tests.sh#L21-L36)
- [tests/scripts/prepare_tests.sh](file://tests/scripts/prepare_tests.sh#L20-L21)

## Debugging and Troubleshooting

Common debugging scenarios and tools available for developers:

### Debugging Semantic Test Failures
When semantic tests fail:
1. Examine the diff report in `tests/reports/`
2. Determine if the difference is expected (e.g., timestamps, IDs)
3. If expected, add a pattern to `exclude_while_comparing.grep`
4. If unexpected, debug the translator code

### Server Debugging
The development environment supports server debugging:
- Set `KLEIO_DEBUG=true` to enable debug output
- Use the `debug_server_until_idle` goal to run the server with automatic shutdown
- The server logs are captured in `kleio_start_server.log`

### Common Issues and Solutions
- **Test files not found**: Ensure the `reference_sources` directory contains the expected files
- **Permission errors**: The Docker container runs with the current user's UID/GID to avoid file ownership issues
- **Port conflicts**: Change `KLEIO_EXTERNAL_PORT` in the environment file
- **Token errors**: Regenerate tokens using `make gen-token`

### Debugging Tools
- SWI-Prolog debugging facilities can be used with the VSC-Prolog extension in VSCode
- The `tspy` predicate can set breakpoints in threaded code
- The test scripts provide verbose output to help diagnose issues

**Section sources**
- [tests/scripts/kleio_start_server.sh](file://tests/scripts/kleio_start_server.sh#L1-L22)
- [tests/scripts/kleio_stop_server.sh](file://tests/scripts/kleio_stop_server.sh#L1-L6)
- [tests/README.md](file://tests/README.md#L99-L100)