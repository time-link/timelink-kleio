# Configuration and Customization

<cite>
**Referenced Files in This Document**   
- [.env-sample](file://.env-sample)
- [.kleio.json](file://.kleio.json)
- [src/serverStart.pl](file://src/serverStart.pl)
- [src/restServer.pl](file://src/restServer.pl)
- [src/kleioFiles.pl](file://src/kleioFiles.pl)
- [src/logging.pl](file://src/logging.pl)
- [src/threadSupport.pl](file://src/threadSupport.pl)
</cite>

## Table of Contents
1. [Environment Variables Configuration](#environment-variables-configuration)
2. [Runtime Configuration and Startup Options](#runtime-configuration-and-startup-options)
3. [Customizable Parameters](#customizable-parameters)
4. [Common Configuration Scenarios](#common-configuration-scenarios)
5. [Environment Management Best Practices](#environment-management-best-practices)
6. [Configuration Troubleshooting](#configuration-troubleshooting)

## Environment Variables Configuration

The `.env-sample` file provides a comprehensive template for configuring the timelink-kleio server through environment variables. These variables control fundamental server behavior including ports, database connections, and operational parameters. The configuration follows a hierarchical approach where environment variables override default values and directory-based detection mechanisms.

### Server Endpoints and Ports
The server configuration includes several key port-related variables:
- `KLEIO_SERVER_PORT` (default: 8088): Specifies the internal port used by the Kleio server, whether running in a container or standalone
- `KLEIO_EXTERNAL_PORT` (default: 8088): Defines the exposed port when running in Docker
- `KLEIO_END_POINT`: Sets the URL endpoint for the Kleio service, with different configurations for Docker Compose, local host access, and direct local connections

These port settings are critical for proper network communication and must be coordinated with any reverse proxy or load balancer configurations.

### Worker and Timeout Settings
Performance-related parameters include:
- `KLEIO_SERVER_WORKERS`: Determines the number of simultaneous worker processes for parallel translations (default: 3)
- `KLEIO_IDLE_TIMEOUT`: Controls the connection keep-alive duration in seconds (default: 900 seconds/15 minutes)

The idle timeout setting is particularly important when handling large file transfers, as insufficient timeout values can lead to premature connection termination during extended operations.

### Security and CORS Configuration
Security parameters include:
- `KLEIO_ADMIN_TOKEN`: Requires a minimum five-character token for full administrative privileges
- `KLEIO_CORS_SITES`: Specifies allowed origins for Cross-Origin Resource Sharing, with `*` permitting all sites

The admin token should be generated using secure methods such as `openssl rand -hex 20` to ensure adequate cryptographic strength.

### Directory Structure Configuration
The system uses a hierarchical directory structure with the following key variables:
- `KLEIO_HOME_DIR`: Root working directory for the Kleio server (default: `/workspaces/timelink-kleio/tests/kleio-home`)
- `KLEIO_CONF_DIR`: Configuration files directory (defaults to `$KLEIO_HOME_DIR/system/conf/kleio`)
- `KLEIO_SOURCE_DIR`: Base directory for Kleio source files (defaults to `$KLEIO_HOME_DIR/sources`)
- `KLEIO_STRU_DIR`: Directory for global structure files (defaults to `$KLEIO_CONF_DIR/stru/`)
- `KLEIO_TOKEN_DB`: Path to the token database (defaults to `$KLEIO_CONF_DIR/token_db`)
- `KLEIO_DEFAULT_STRU`: Default structure file path (defaults to `$KLEIO_CONF_DIR/stru/gacto2.str`)

The system automatically detects MHK-HOME installations when `KLEIO_HOME_DIR` is organized accordingly, adjusting internal defaults to match the MHK_HOME structure.

**Section sources**
- [.env-sample](file://.env-sample#L1-L119)

## Runtime Configuration and Startup Options

The runtime configuration is managed through the `.kleio.json` file and the `serverStart.pl` startup script, which work together to initialize the server with the appropriate settings.

### .kleio.json Configuration File
The `.kleio.json` file serves as a persistent configuration store that captures the server's runtime state. Key configuration parameters include:
- `kleio_admin_token`: Stores the admin token for authentication
- `kleio_home`: Specifies the Kleio home directory path
- `kleio_url`: Defines the server URL endpoint
- `kleio_version`: Records the current version information
- `kleio_log`: Specifies the log file path
- `kleio_conf_dir`: Configuration directory path
- `kleio_token_db_status`: Indicates the status of the token database

This JSON configuration is automatically generated and updated by the `save_kleio_config` predicate in `restServer.pl`, ensuring that runtime configuration changes are persisted across server restarts.

### Server Startup Options
The `serverStart.pl` file provides multiple startup predicates for different operational scenarios:
- `run_server_forever`: Starts the server and holds the thread indefinitely
- `run_debug_server`: Activates both REST and debug servers with debug-level logging
- `run_test_server`: Configures a test server environment with predefined settings for test suites
- `run_from_mhk_home`: Runs a server within an MHK_HOME environment
- `setup_and_run_server`: Flexible startup that accepts various configuration parameters

The startup process integrates environment variable detection, with the `run_server_forever` predicate checking for `KLEIO_DEBUG` to determine whether to start in debug mode.

**Section sources**
- [.kleio.json](file://.kleio.json#L1-L13)
- [src/serverStart.pl](file://src/serverStart.pl#L1-L442)

## Customizable Parameters

The timelink-kleio system offers extensive customization options across various operational domains, allowing administrators to fine-tune performance and behavior.

### Logging Configuration
The `logging.pl` module provides comprehensive logging capabilities with configurable severity levels:
- Supported log levels: emerg, alert, crit, err, warning, notice, info, debug
- Configurable through `set_log_level/1` predicate
- Log output destination can be directed to files or standard output
- Automatic log directory creation at `KLEIO_HOME_DIR/.kleio/logs/`

The logging system uses shared properties to manage log state and supports dynamic log level changes at runtime without requiring server restarts.

### Thread Pool Configuration
The `threadSupport.pl` module manages worker thread pools with the following configurable aspects:
- Dynamic worker creation through `create_workers/1`
- Configurable pool mode (message queue or thread pool)
- Job queuing and processing tracking
- Thread pool properties including local, global, and trail memory limits

The system defaults to a message-based worker model but can be configured to use Prolog's thread pool mechanism for different performance characteristics.

### Server Operational Parameters
Key operational parameters are managed through the `restServer.pl` module:
- REST port configuration via `default_value(rest_port,RP)`
- Worker count determination through `default_value(workers,Workers)`
- Connection timeout settings via `default_value(timeout,Timeout)`
- CORS policy configuration through `default_value(cors,CorsList)`

These parameters are retrieved from environment variables when available, falling back to sensible defaults otherwise, providing flexibility across different deployment environments.

**Section sources**
- [src/logging.pl](file://src/logging.pl#L1-L161)
- [src/threadSupport.pl](file://src/threadSupport.pl#L1-L153)
- [src/restServer.pl](file://src/restServer.pl#L172-L184)

## Common Configuration Scenarios

### Adjusting Memory Limits for Large Translations
For processing large translation files, modify the following settings:
1. Increase `KLEIO_IDLE_TIMEOUT` to prevent connection timeouts during extended operations
2. Adjust thread pool memory limits in `threadSupport.pl` to accommodate larger working sets
3. Ensure adequate disk space for temporary files in the working directory

Example configuration for large translations:
```
KLEIO_IDLE_TIMEOUT=1800  # 30 minute timeout
KLEIO_SERVER_WORKERS=6   # Increased worker count
```

### Configuring Backup Intervals
While the system doesn't have explicit backup interval settings, implement regular backups through:
1. External cron jobs or scheduled tasks
2. Automated scripts that copy the `KLEIO_HOME_DIR` contents
3. Database dump utilities for the token database

Recommended backup strategy:
- Daily backups of configuration directories
- Weekly full system backups
- Transaction log archiving for recovery point objectives

### Performance Optimization for High-Load Environments
For environments with heavy concurrent usage:
1. Increase `KLEIO_SERVER_WORKERS` to match available CPU cores
2. Optimize thread pool settings in `threadSupport.pl`
3. Configure reverse proxy timeout settings to align with `KLEIO_IDLE_TIMEOUT`
4. Implement load balancing across multiple server instances

**Section sources**
- [src/restServer.pl](file://src/restServer.pl#L331-L349)
- [src/threadSupport.pl](file://src/threadSupport.pl#L57-L60)

## Environment Management Best Practices

### Managing Configuration Across Environments
Implement a consistent strategy for managing configuration across development, staging, and production environments:

#### Environment-Specific Configuration Files
Maintain separate `.env` files for each environment:
- `.env.development`: Optimized for debugging and development
- `.env.staging`: Mirrors production with additional monitoring
- `.env.production`: Optimized for performance and security

#### Configuration Hierarchy
Follow a consistent precedence order:
1. Environment variables (highest precedence)
2. `.env` file settings
3. Default values in code
4. Directory-based auto-detection

#### Secure Settings Management
Implement security best practices:
- Never commit `.env` files with sensitive data to version control
- Use environment variables for secrets rather than configuration files
- Rotate `KLEIO_ADMIN_TOKEN` regularly
- Restrict `KLEIO_CORS_SITES` in production to specific domains rather than using `*`

#### Configuration Validation
Implement validation procedures:
- Verify configuration syntax before deployment
- Test configuration changes in staging before production
- Document all configuration changes and their rationale
- Maintain configuration backups

**Section sources**
- [.env-sample](file://.env-sample#L42-L50)
- [.kleio.json](file://.kleio.json#L3)

## Configuration Troubleshooting

### Common Configuration Issues and Solutions
Address frequent configuration-related problems:

#### Server Fails to Start
- Verify `KLEIO_HOME_DIR` exists and is accessible
- Check that required subdirectories (sources, system/conf/kleio) are present
- Ensure proper file permissions for configuration directories
- Validate that port numbers are not already in use

#### Connection Timeouts
- Increase `KLEIO_IDLE_TIMEOUT` value for large file operations
- Verify network connectivity between client and server
- Check firewall rules for blocked ports
- Ensure reverse proxy timeout settings exceed `KLEIO_IDLE_TIMEOUT`

#### Authentication Failures
- Verify `KLEIO_ADMIN_TOKEN` meets minimum length requirement
- Check that token database is properly initialized
- Ensure consistent token usage across client applications
- Validate token expiration settings

#### File Access Issues
- Confirm `KLEIO_SOURCE_DIR` contains necessary source files
- Verify directory permissions for read/write operations
- Check that relative paths resolve correctly
- Ensure symbolic links (if used) are properly configured

### Diagnostic Procedures
Implement systematic troubleshooting:
1. Check server logs at the configured log path
2. Use `print_server_config` to verify current settings
3. Validate environment variables are properly loaded
4. Test configuration with minimal settings before adding complexity

**Section sources**
- [src/restServer.pl](file://src/restServer.pl#L186-L226)
- [src/serverStart.pl](file://src/serverStart.pl#L22-L24)
- [src/logging.pl](file://src/logging.pl#L98-L113)