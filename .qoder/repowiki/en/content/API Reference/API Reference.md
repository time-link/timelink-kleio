# API Reference

<cite>
**Referenced Files in This Document**   
- [api.json](file://api/postman/api.json)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json)
- [restServer.pl](file://src/restServer.pl)
- [apiSources.pl](file://src/apiSources.pl)
- [apiTranslations.pl](file://src/apiTranslations.pl)
- [apiExports.pl](file://src/apiExports.pl)
- [apiTokens.pl](file://src/apiTokens.pl)
- [apiDirectories.pl](file://src/apiDirectories.pl)
- [apiGit.pl](file://src/apiGit.pl)
- [errors.pl](file://src/errors.pl)
- [test_report_2025-12-13_11:19:25.diff](file://tests/reports/test_report_2025-12-13_11:19:25.diff)
- [environment.json](file://api/postman/environment.json)
- [tests.postman_environment.json](file://api/postman/tests.postman_environment.json)
</cite>

## Table of Contents
1. [Introduction](#introduction)
2. [Authentication](#authentication)
3. [Endpoint Overview](#endpoint-overview)
4. [/sources Endpoint](#sources-endpoint)
5. [/translations Endpoint](#translations-endpoint)
6. [/exports Endpoint](#exports-endpoint)
7. [/tokens Endpoint](#tokens-endpoint)
8. [/directories Endpoint](#directories-endpoint)
9. [/versions Endpoint](#versions-endpoint)
10. [Error Handling](#error-handling)
11. [Rate Limiting and Security](#rate-limiting-and-security)
12. [Client Implementation Guidelines](#client-implementation-guidelines)
13. [Debugging and Monitoring](#debugging-and-monitoring)
14. [Migration Notes](#migration-notes)

## Introduction

The timelink-kleio RESTful API provides programmatic access to the Kleio translation system, enabling clients to manage source files, initiate translations, retrieve exports, and perform version control operations. The API is available in both REST and JSON-RPC 2.0 versions, with REST being preferred for simple file operations and JSON-RPC for complex interactions.

The API follows a resource-oriented architecture with endpoints for managing sources, translations, exports, directories, and Git operations. All endpoints require token-based authentication, with tokens granting specific permissions based on user roles. The API supports both synchronous and asynchronous operations, with translation jobs being processed in the background.

This documentation covers all API endpoints, including their HTTP methods, URL patterns, request/response schemas, authentication requirements, and error handling strategies. Examples are drawn from actual Postman collections and test cases to ensure accuracy.

**Section sources**
- [api.json](file://api/postman/api.json)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json)
- [restServer.pl](file://src/restServer.pl)

## Authentication

The timelink-kleio API uses token-based authentication for all endpoints. Clients must include a valid token in the Authorization header of each request using the Bearer scheme.

### Token Generation

Tokens are generated through the `/json/` JSON-RPC endpoint using the `tokens_generate` method. The token generation request requires an admin token and specifies the user's permissions and access restrictions.

```json
{
    "jsonrpc": "2.0",
    "method": "tokens_generate",
    "params": {
        "user": "tester",
        "info": {
            "comment": "An user able to translate, upload and delete files, and also create and remove directories",
            "api": [
                "sources",
                "kleioset",
                "files",
                "structures",
                "translations",
                "upload",
                "delete",
                "mkdir",
                "rmdir"
            ],
            "structures": "users/tester/stru",
            "sources": "sources/api_tests"
        },
        "token": "{{testadmintoken}}"
    },
    "id": 1
}
```

The response contains the generated token:

```json
{
    "jsonrpc": "2.0",
    "result": "8312070ca229c81feaeb29a973f5132d795a0db4",
    "id": 1
}
```

### Token Permissions

Tokens are associated with specific permissions that determine what operations the user can perform:

- **sources**: Access to source files
- **kleioset**: Retrieve translation status information
- **files**: Download files
- **structures**: Access to structure files
- **translations**: Initiate translations
- **upload**: Upload or update files
- **delete**: Delete files or directories
- **mkdir**: Create directories
- **rmdir**: Remove directories
- **generate_token**: Generate new tokens (admin privilege)
- **invalidate_token**: Revoke tokens (admin privilege)
- **invalidate_user**: Revoke all tokens for a user (admin privilege)

### Authentication Header

All API requests must include the Authorization header:

```
Authorization: Bearer 8312070ca229c81feaeb29a973f5132d795a0db4
```

Requests without a valid token will receive a 401 Unauthorized response.

**Section sources**
- [api.json](file://api/postman/api.json)
- [apiTokens.pl](file://src/apiTokens.pl)
- [restServer.pl](file://src/restServer.pl)

## Endpoint Overview

The timelink-kleio API provides several endpoints for managing different aspects of the Kleio translation system. The API supports both REST and JSON-RPC 2.0 protocols, with REST being the preferred method for most operations.

### Protocol Selection

The API offers two protocols:
- **REST**: Preferred for simple file operations like retrieving sources and exports
- **JSON-RPC 2.0**: Required for complex operations like token management and should be used for all other functions

Clients should use JSON-RPC for most operations and fall back to REST only for simple file fetching.

### Base URL

The base URL for the API is configurable but typically follows the pattern:
```
http://localhost:8088/
```

The port number may vary based on the server configuration.

### Request Structure

REST requests use standard HTTP methods (GET, POST, PUT, DELETE) with parameters passed in the query string or request body. JSON-RPC requests are POST requests to the `/json/` endpoint with a JSON payload containing the method, parameters, and request ID.

### Response Format

Successful responses return JSON data with a 200 OK status code. Error responses follow the JSON-RPC 2.0 error format with appropriate HTTP status codes.

**Section sources**
- [api.json](file://api/postman/api.json)
- [restServer.pl](file://src/restServer.pl)

## /sources Endpoint

The `/sources` endpoint manages Kleio source files, allowing clients to retrieve, upload, copy, move, and delete source files.

### GET /sources/{path}

Retrieves a source file or lists files in a directory.

**HTTP Method**: GET

**URL Pattern**: `/rest/sources/{path}`

**Parameters**:
- `path` (required): Path to the file or directory
- `recurse` (optional): If "yes", recursively list files in subdirectories
- `url` (optional): If "yes", return URLs for retrieving files

**Authentication**: Required (files permission)

**Request Example**:
```
GET /rest/sources/paroquiais/casamentos/cas1714-1722.cli?id=1987 HTTP/1.1
Authorization: Bearer {{remotetoken}}
Accept: application/json
```

**Response Schema (JSON)**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "mime_type": "text/cli",
        "url": "/rest/sources/paroquiais/casamentos/cas1714-1722.cli"
    }
}
```

When requesting a directory with `url=yes`, the response contains a list of file URLs:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        "/rest/sources/paroquiais/baptismos/b1685.cli",
        "/rest/sources/paroquiais/baptismos/b1686.cli"
    ]
}
```

### POST /sources/{path} (Upload)

Uploads a new source file.

**HTTP Method**: POST

**URL Pattern**: `/rest/sources/{path}`

**Content-Type**: multipart/form-data

**Authentication**: Required (upload permission)

**Request Example**:
```
POST /rest/sources/new_directory/new_file.cli HTTP/1.1
Authorization: Bearer {{remotetoken}}
Content-Type: multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxkTrZu0gW

------WebKitFormBoundary7MA4YWxkTrZu0gW
Content-Disposition: form-data; name="file"; filename="new_file.cli"
Content-Type: text/cli

[File content]
------WebKitFormBoundary7MA4YWxkTrZu0gW--
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": "new_directory/new_file.cli"
}
```

### PUT /sources/{path} (Update)

Updates an existing source file.

**HTTP Method**: PUT

**URL Pattern**: `/rest/sources/{path}`

**Content-Type**: multipart/form-data

**Authentication**: Required (upload permission)

**Request Example**:
```
PUT /rest/sources/existing_directory/existing_file.cli HTTP/1.1
Authorization: Bearer {{remotetoken}}
Content-Type: multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxkTrZu0gW

------WebKitFormBoundary7MA4YWxkTrZu0gW
Content-Disposition: form-data; name="file"; filename="existing_file.cli"
Content-Type: text/cli

[Updated file content]
------WebKitFormBoundary7MA4YWxkTrZu0gW--
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": "existing_directory/existing_file.cli"
}
```

### POST /sources/{path} (Copy)

Copies a source file to a new location.

**HTTP Method**: POST

**URL Pattern**: `/rest/sources/{path}?origin={source_path}`

**Authentication**: Required (upload permission)

**Request Example**:
```
POST /rest/sources/new_location/copied_file.cli?origin=sources/old_location/original_file.cli&id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": "new_location/copied_file.cli"
}
```

### PUT /sources/{path} (Move)

Moves a source file to a new location.

**HTTP Method**: PUT

**URL Pattern**: `/rest/sources/{path}?origin={source_path}`

**Authentication**: Required (upload permission)

**Request Example**:
```
PUT /rest/sources/new_location/moved_file.cli?origin=sources/old_location/original_file.cli&id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": "new_location/moved_file.cli"
}
```

### DELETE /sources/{path}

Deletes a source file or directory.

**HTTP Method**: DELETE

**URL Pattern**: `/rest/sources/{path}`

**Parameters**:
- `force` (optional): If "yes", removes directory even if not empty

**Authentication**: Required (delete permission)

**Request Example**:
```
DELETE /rest/sources/unwanted_directory?force=yes&id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        "unwanted_directory/file1.cli",
        "unwanted_directory/file2.cli"
    ]
}
```

**Section sources**
- [api.json](file://api/postman/api.json)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json)
- [apiSources.pl](file://src/apiSources.pl)

## /translations Endpoint

The `/translations` endpoint manages the translation process, allowing clients to initiate translations, retrieve translation status, and clean translation results.

### POST /translations/{path}

Starts a translation of the specified file or directory.

**HTTP Method**: POST

**URL Pattern**: `/rest/translations/{path}`

**Authentication**: Required (translations permission)

**Parameters**:
- `structure` (optional): Structure file to use for translation
- `echo` (optional): If "yes", include source lines in the report
- `recurse` (optional): If "yes", translate files in subdirectories
- `status` (optional): Filter files by translation status
- `spawn` (optional): If "yes", distribute files to different workers

**Request Example**:
```
POST /rest/translations/paroquiais/baptismos?recurse=yes&echo=yes&id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
Content-Type: application/json

{
    "structure": "structures/baptismos.yaml"
}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        {
            "job": 1,
            "sources": [
                "paroquiais/baptismos/b1685.cli",
                "paroquiais/baptismos/b1686.cli"
            ]
        }
    ]
}
```

### GET /translations/{path}

Retrieves the translation status of files.

**HTTP Method**: GET

**URL Pattern**: `/rest/translations/{path}`

**Authentication**: Required (translations permission)

**Parameters**:
- `recurse` (optional): If "yes", include files in subdirectories
- `status` (optional): Filter by translation status (V, T, E, W, P, Q)
- `tstatus` (optional): Filter by file type (F for files, D for directories)

**Request Example**:
```
GET /rest/translations?recurse=yes&tstatus=T&id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
Accept: application/json
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        {
            "name": "b1685.cli",
            "path": "paroquiais/baptismos/b1685.cli",
            "source_url": "/rest/sources/paroquiais/baptismos/b1685.cli",
            "status": "V",
            "modified": 1556194205.0,
            "modified_string": "2019-04-25 12:30:05",
            "modified_rfc1123": "Thu, 25 Apr 2019 12:30:05 GMT",
            "modified_iso": "2019-04-25T12:30:05",
            "size": 1234,
            "directory": "paroquiais/baptismos",
            "errors": 0,
            "warnings": 2,
            "version": "ClioInput 2.0",
            "translated": 1556194205.0,
            "translated_string": "25-Apr-2019 12:30",
            "rpt_url": "/rest/reports/paroquiais/baptismos/b1685.cli",
            "xml_url": "/rest/exports/paroquiais/baptismos/b1685.cli",
            "more_url": "/rest/sources/paroquiais/baptismos/b1685.cli.files.json"
        }
    ]
}
```

### DELETE /translations/{path}

Cleans translation results (deletes derived files).

**HTTP Method**: DELETE

**URL Pattern**: `/rest/translations/{path}`

**Parameters**:
- `recurse` (optional): If "yes", clean translations in subdirectories

**Authentication**: Required (translations permission)

**Request Example**:
```
DELETE /rest/translations/paroquiais/baptismos?recurse=yes&d=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        "paroquiais/baptismos/b1685.cli",
        "paroquiais/baptismos/b1686.cli"
    ]
}
```

**Section sources**
- [api.json](file://api/postman/api.json)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json)
- [apiTranslations.pl](file://src/apiTranslations.pl)

## /exports Endpoint

The `/exports` endpoint retrieves XML export files generated from translated Kleio source files.

### GET /exports/{path}

Retrieves an XML export file or lists exports in a directory.

**HTTP Method**: GET

**URL Pattern**: `/rest/exports/{path}`

**Authentication**: Required (files permission)

**Request Example**:
```
GET /rest/exports/paroquiais/baptismos/b1685.cli HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response**:
The response returns the XML file content with appropriate Content-Type headers:
```
HTTP/1.1 200 OK
Content-Type: application/xml
Content-Disposition: attachment; filename="b1685.xml"

[XML content]
```

When requesting a directory, the response lists available exports:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        "/rest/exports/paroquiais/baptismos/b1685.cli",
        "/rest/exports/paroquiais/baptismos/b1686.cli"
    ]
}
```

**Section sources**
- [apiExports.pl](file://src/apiExports.pl)
- [api.json](file://api/postman/api.json)

## /tokens Endpoint

The `/tokens` endpoint manages authentication tokens, allowing administrators to generate and invalidate tokens.

### POST /json/ (tokens_generate)

Generates a new authentication token.

**HTTP Method**: POST

**URL Pattern**: `/json/`

**Authentication**: Required (generate_token permission)

**Request Schema**:
```json
{
    "jsonrpc": "2.0",
    "method": "tokens_generate",
    "params": {
        "user": "string",
        "info": {
            "comment": "string",
            "api": ["string"],
            "structures": "string",
            "sources": "string"
        },
        "token": "string"
    },
    "id": "number"
}
```

**Request Example**:
```json
{
    "jsonrpc": "2.0",
    "method": "tokens_generate",
    "params": {
        "user": "tester",
        "info": {
            "comment": "An user able to translate, upload and delete files",
            "api": [
                "sources",
                "kleioset",
                "files",
                "structures",
                "translations",
                "upload",
                "delete",
                "mkdir",
                "rmdir"
            ],
            "structures": "users/tester/stru",
            "sources": "sources/api_tests"
        },
        "token": "7c689b969e59c0a244a5778b43cb7da135a90056"
    },
    "id": 1
}
```

**Response Schema**:
```json
{
    "jsonrpc": "2.0",
    "result": "8312070ca229c81feaeb29a973f5132d795a0db4",
    "id": 1
}
```

### DELETE /json/ (tokens_invalidate)

Invalidates a token.

**HTTP Method**: POST

**URL Pattern**: `/json/`

**Authentication**: Required (invalidate_token permission)

**Request Schema**:
```json
{
    "jsonrpc": "2.0",
    "method": "tokens_invalidate",
    "params": {
        "user_token": "string",
        "token": "string"
    },
    "id": "number"
}
```

**Request Example**:
```json
{
    "jsonrpc": "2.0",
    "method": "tokens_invalidate",
    "params": {
        "user_token": "8312070ca229c81feaeb29a973f5132d795a0db4",
        "token": "7c689b969e59c0a244a5778b43cb7da135a90056"
    },
    "id": 1
}
```

**Response Schema**:
```json
{
    "jsonrpc": "2.0",
    "result": "8312070ca229c81feaeb29a973f5132d795a0db4",
    "id": 1
}
```

**Section sources**
- [api.json](file://api/postman/api.json)
- [apiTokens.pl](file://src/apiTokens.pl)

## /directories Endpoint

The `/directories` endpoint manages directories containing source files.

### GET /directories/{path}

Lists directories in the specified path.

**HTTP Method**: GET

**URL Pattern**: `/rest/directories/{path}`

**Parameters**:
- `recurse` (optional): If "yes", recursively list subdirectories

**Authentication**: Required (files permission)

**Request Example**:
```
GET /rest/directories/paroquiais?recurse=yes&id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        "paroquiais/baptismos",
        "paroquiais/casamentos",
        "paroquiais/obitos"
    ]
}
```

### POST /directories/{path}

Creates a new directory.

**HTTP Method**: POST

**URL Pattern**: `/rest/directories/{path}`

**Authentication**: Required (mkdir permission)

**Request Example**:
```
POST /rest/directories/new_directory?id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": "new_directory"
}
```

### POST /directories/{path} (Copy)

Copies a directory to a new location.

**HTTP Method**: POST

**URL Pattern**: `/rest/directories/{path}?origin={source_path}`

**Authentication**: Required (mkdir permission)

**Request Example**:
```
POST /rest/directories/new_location?origin=directories/old_location&id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": "new_location"
}
```

### DELETE /directories/{path}

Removes a directory.

**HTTP Method**: DELETE

**URL Pattern**: `/rest/directories/{path}`

**Parameters**:
- `force` (optional): If "yes", removes directory even if not empty

**Authentication**: Required (delete permission)

**Request Example**:
```
DELETE /rest/directories/unwanted_directory?force=yes&id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": "unwanted_directory"
}
```

**Section sources**
- [api.json](file://api/postman/api.json)
- [apiDirectories.pl](file://src/apiDirectories.pl)

## /versions Endpoint

The `/versions` endpoint provides Git repository operations for version control of source files.

### GET /versions/status/global/{path}

Retrieves the global status of the Git repository.

**HTTP Method**: GET

**URL Pattern**: `/rest/versions/status/global/{path}`

**Authentication**: Required (files permission)

**Request Example**:
```
GET /rest/versions/status/global/sources?id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "directory": "tests/kleio-home/sources",
        "git_root": "tests/kleio-home/sources",
        "branch": "main",
        "status": "clean",
        "report": "On branch main\nYour branch is up to date with 'origin/main'.\n\nnothing to commit, working tree clean\n"
    }
}
```

### GET /versions/remotes/branches/{path}

Lists branches in the remote repository.

**HTTP Method**: GET

**URL Pattern**: `/rest/versions/remotes/branches/{path}`

**Authentication**: Required (files permission)

**Request Example**:
```
GET /rest/versions/remotes/branches/sources?id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": [
        "main",
        "develop",
        "feature/new-translator"
    ]
}
```

### GET /versions/pull/{path}

Pulls changes from the remote repository.

**HTTP Method**: GET

**URL Pattern**: `/rest/versions/pull/{path}`

**Authentication**: Required (files permission)

**Request Example**:
```
GET /rest/versions/pull/sources?id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "git_output": [
            "remote: Enumerating objects: 5, done.",
            "remote: Counting objects: 100% (5/5), done.",
            "remote: Compressing objects: 100% (3/3), done.",
            "remote: Total 3 (delta 1), reused 0 (delta 0), pack-reused 0",
            "Unpacking objects: 100% (3/3), 305 bytes | 305.00 KiB/s, done.",
            "From github.com:user/repo",
            "   abc1234..def5678  main       -> origin/main",
            "Updating abc1234..def5678",
            "Fast-forward",
            " file1.cli | 2 ++",
            " 1 file changed, 2 insertions(+)"
        ],
        "git_error": [],
        "git_exit_status": 0
    }
}
```

### PUT /versions/push/{path}

Pushes changes to the remote repository.

**HTTP Method**: PUT

**URL Pattern**: `/rest/versions/push/{path}`

**Authentication**: Required (files permission)

**Request Example**:
```
PUT /rest/versions/push/sources?id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "git_output": [
            "Counting objects: 3, done.",
            "Delta compression using up to 8 threads.",
            "Compressing objects: 100% (3/3), done.",
            "Writing objects: 100% (3/3), 305 bytes | 305.00 KiB/s, done.",
            "Total 3 (delta 1), reused 0 (delta 0)",
            "remote: Resolving deltas: 100% (1/1), completed with 1 local object.",
            "To github.com:user/repo.git",
            "   abc1234..def5678  main -> main"
        ],
        "git_error": [],
        "git_exit_status": 0
    }
}
```

### PUT /versions/commit/{path}

Commits changes to the local repository.

**HTTP Method**: PUT

**URL Pattern**: `/rest/versions/commit/{path}`

**Parameters**:
- `commit_message`: Commit message
- `add_files`: Files to add (optional)
- `commit_files`: Specific files to commit (optional)

**Authentication**: Required (files permission)

**Request Example**:
```
PUT /rest/versions/commit/sources?commit_message=Update+sources&id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "git_output": [
            "[main abc1234] Update sources",
            " 1 file changed, 2 insertions(+)"
        ],
        "git_error": [],
        "git_exit_status": 0
    }
}
```

### PUT /versions/set-user-info/{path}

Sets the Git user information.

**HTTP Method**: PUT

**URL Pattern**: `/rest/versions/set-user-info/{path}`

**Parameters**:
- `user_name`: User name
- `user_email`: User email

**Authentication**: Required (files permission)

**Request Example**:
```
PUT /rest/versions/set-user-info/sources?user_name=John+Doe&user_email=john@example.com&id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "git_output": [],
        "git_error": [],
        "git_exit_status": 0
    }
}
```

### DELETE /versions/reset/{path}

Resets the local repository to a specific commit.

**HTTP Method**: DELETE

**URL Pattern**: `/rest/versions/reset/{path}`

**Parameters**:
- `reset_mode`: Reset mode (--soft, --mixed, --hard)
- `commit_ref`: Commit reference (default: HEAD)

**Authentication**: Required (files permission)

**Request Example**:
```
DELETE /rest/versions/reset/sources?reset_mode=--hard&commit_ref=HEAD~1&id=1 HTTP/1.1
Authorization: Bearer {{remotetoken}}
```

**Response Schema**:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "git_output": [
            "HEAD is now at abc1234 Previous commit"
        ],
        "git_error": [],
        "git_exit_status": 0
    }
}
```

**Section sources**
- [api.json](file://api/postman/api.json)
- [apiGit.pl](file://src/apiGit.pl)

## Error Handling

The timelink-kleio API follows standard HTTP status codes and JSON-RPC 2.0 error formats for error reporting.

### HTTP Status Codes

- **200 OK**: Successful response
- **400 Bad Request**: Invalid request parameters
- **401 Unauthorized**: Missing or invalid authentication token
- **403 Forbidden**: Token does not have required permissions
- **404 Not Found**: Requested resource does not exist
- **405 Method Not Allowed**: HTTP method not supported for the endpoint
- **500 Internal Server Error**: Server error during request processing

### JSON-RPC Error Format

Error responses follow the JSON-RPC 2.0 specification:

```json
{
    "jsonrpc": "2.0",
    "error": {
        "code": -32602,
        "message": "Invalid params",
        "data": "Structure file does not exist"
    },
    "id": 1
}
```

### Common Error Codes

- **-32700**: Parse error (invalid JSON)
- **-32600**: Invalid request
- **-32601**: Method not found
- **-32602**: Invalid params
- **-32003**: Delete failed (directory not empty)
- **-32004**: Could not create directory
- **-32005**: Directory already exists

### Error Response Examples

**Authentication Required**:
```json
{
    "jsonrpc": "2.0",
    "error": {
        "code": -32602,
        "message": "Bad token"
    },
    "id": 1
}
```

**Insufficient Permissions**:
```json
{
    "jsonrpc": "2.0",
    "error": {
        "code": 403,
        "message": "Forbidden"
    },
    "id": 1
}
```

**Resource Not Found**:
```json
{
    "jsonrpc": "2.0",
    "error": {
        "code": 404,
        "message": "Not Found"
    },
    "id": 1
}
```

**Invalid Parameters**:
```json
{
    "jsonrpc": "2.0",
    "error": {
        "code": -32602,
        "message": "Invalid params",
        "data": "Structure file does not exist"
    },
    "id": 1
}
```

The API also includes detailed error reporting in translation processes, with error counts and specific error messages in the translation reports.

**Section sources**
- [api.json](file://api/postman/api.json)
- [errors.pl](file://src/errors.pl)
- [restServer.pl](file://src/restServer.pl)

## Rate Limiting and Security

The timelink-kleio API implements several security measures to protect the system and ensure fair usage.

### Rate Limiting

The API does not implement explicit rate limiting at the application level. However, server configuration may impose limits through:

- Worker thread limits (configured via KLEIO_SERVER_WORKERS environment variable)
- Request timeout (default 300 seconds, configurable via server configuration)
- Connection limits imposed by the underlying HTTP server

Clients should implement reasonable retry logic with exponential backoff when encountering server errors.

### Security Considerations

#### Authentication and Authorization

- All endpoints require token-based authentication
- Tokens are associated with specific permissions that limit user actions
- Admin tokens are required for token management operations
- Tokens can be invalidated individually or for entire users

#### Input Validation

- All file operations validate paths against the user's allowed directories
- File uploads check for existing files to prevent accidental overwrites
- Directory operations validate that target directories exist before file operations

#### Server Configuration

Security-related environment variables:
- **KLEIO_ADMIN_TOKEN**: Admin token for initial setup
- **KLEIO_CORS_SITES**: CORS allowed sites (default: *)
- **KLEIO_SERVER_WORKERS**: Number of worker threads
- **KLEIO_IDLE_TIMEOUT**: Server idle timeout

#### Secure Practices

- Use HTTPS in production environments
- Rotate admin tokens periodically
- Limit token permissions to the minimum required
- Monitor server logs for suspicious activity
- Keep the server software updated

**Section sources**
- [restServer.pl](file://src/restServer.pl)
- [apiTokens.pl](file://src/apiTokens.pl)

## Client Implementation Guidelines

This section provides guidelines for implementing clients for the timelink-kleio API in various programming languages.

### General Guidelines

1. **Use JSON-RPC for most operations**: While REST is available for simple file operations, JSON-RPC is the preferred protocol for most interactions.

2. **Handle authentication properly**: Store tokens securely and handle token expiration and invalidation.

3. **Implement proper error handling**: Check HTTP status codes and JSON-RPC error responses.

4. **Use appropriate timeouts**: Set reasonable timeouts for requests, especially for translation operations which may take time.

5. **Follow rate limiting best practices**: Implement exponential backoff for retries.

### Python Example

```python
import requests
import json

class TimelinkKleioClient:
    def __init__(self, base_url, token):
        self.base_url = base_url
        self.token = token
        self.headers = {
            'Authorization': f'Bearer {token}',
            'Content-Type': 'application/json'
        }
    
    def make_json_rpc_call(self, method, params):
        payload = {
            'jsonrpc': '2.0',
            'method': method,
            'params': params,
            'id': 1
        }
        
        response = requests.post(
            f'{self.base_url}/json/',
            headers=self.headers,
            json=payload
        )
        
        if response.status_code == 200:
            result = response.json()
            if 'error' in result:
                raise Exception(f"API Error: {result['error']}")
            return result['result']
        else:
            response.raise_for_status()
    
    def get_sources(self, path, recurse=False):
        params = {
            'path': path,
            'token': self.token
        }
        if recurse:
            params['recurse'] = 'yes'
            
        return self.make_json_rpc_call('sources_get', params)
    
    def translate(self, path, structure=None):
        params = {
            'path': path,
            'token': self.token
        }
        if structure:
            params['structure'] = structure
            
        return self.make_json_rpc_call('translations_translate', params)
```

### JavaScript Example

```javascript
class TimelinkKleioClient {
    constructor(baseUrl, token) {
        this.baseUrl = baseUrl;
        this.token = token;
        this.headers = {
            'Authorization': `Bearer ${token}`,
            'Content-Type': 'application/json'
        };
    }
    
    async makeJsonRpcCall(method, params) {
        const payload = {
            jsonrpc: '2.0',
            method: method,
            params: params,
            id: 1
        };
        
        const response = await fetch(`${this.baseUrl}/json/`, {
            method: 'POST',
            headers: this.headers,
            body: JSON.stringify(payload)
        });
        
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }
        
        const result = await response.json();
        if (result.error) {
            throw new Error(`API Error: ${JSON.stringify(result.error)}`);
        }
        
        return result.result;
    }
    
    async getSources(path, recurse = false) {
        const params = {
            path: path,
            token: this.token
        };
        
        if (recurse) {
            params.recurse = 'yes';
        }
        
        return await this.makeJsonRpcCall('sources_get', params);
    }
    
    async translate(path, structure = null) {
        const params = {
            path: path,
            token: this.token
        };
        
        if (structure) {
            params.structure = structure;
        }
        
        return await this.makeJsonRpcCall('translations_translate', params);
    }
}
```

### Java Example

```java
import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;
import com.google.gson.*;

public class TimelinkKleioClient {
    private String baseUrl;
    private String token;
    private Gson gson;
    
    public TimelinkKleioClient(String baseUrl, String token) {
        this.baseUrl = baseUrl;
        this.token = token;
        this.gson = new Gson();
    }
    
    public Object makeJsonRpcCall(String method, Object params) 
            throws IOException {
        String url = baseUrl + "/json/";
        String jsonPayload = gson.toJson(new JsonRpcRequest(method, params));
        
        HttpURLConnection connection = (HttpURLConnection) new URL(url).openConnection();
        connection.setRequestMethod("POST");
        connection.setRequestProperty("Content-Type", "application/json");
        connection.setRequestProperty("Authorization", "Bearer " + token);
        connection.setDoOutput(true);
        
        try (OutputStream os = connection.getOutputStream()) {
            byte[] input = jsonPayload.getBytes(StandardCharsets.UTF_8);
            os.write(input, 0, input.length);
        }
        
        int responseCode = connection.getResponseCode();
        if (responseCode != 200) {
            throw new IOException("HTTP error code: " + responseCode);
        }
        
        try (BufferedReader br = new BufferedReader(
                new InputStreamReader(connection.getInputStream(), StandardCharsets.UTF_8))) {
            StringBuilder response = new StringBuilder();
            String responseLine;
            while ((responseLine = br.readLine()) != null) {
                response.append(responseLine.trim());
            }
            
            JsonObject jsonResponse = JsonParser.parseString(response.toString()).getAsJsonObject();
            if (jsonResponse.has("error")) {
                throw new IOException("API Error: " + jsonResponse.get("error").toString());
            }
            
            return jsonResponse.get("result");
        }
    }
    
    // Inner classes for JSON-RPC request
    private static class JsonRpcRequest {
        public String jsonrpc = "2.0";
        public String method;
        public Object params;
        public int id = 1;
        
        public JsonRpcRequest(String method, Object params) {
            this.method = method;
            this.params = params;
        }
    }
}
```

**Section sources**
- [api.json](file://api/postman/api.json)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json)

## Debugging and Monitoring

This section covers debugging tools and monitoring approaches for the timelink-kleio API.

### Test Report Diffs

The system generates test report diffs to compare translation results. These diffs are stored in the `tests/reports/` directory and can be used to verify the consistency of translation outputs.

Example diff file (`test_report_2025-12-13_11:19:25.diff`):
```
Sat Dec 13 11:19:25 CST 2025 tests
Comparing translation results.
Sat Dec 13 11:20:53 CST 2025 tests
Only in kleio-home/sources/reference_translations: .gitignore
diff -r kleio-home/sources/reference_translations/issue15/issue15.cli kleio-home/sources/test_translations/issue15/issue15.cli
> kleio$structures.vereacoes.yaml
< 
>             
< 
>             
19a20
> 
Only in kleio-home/sources/reference_translations/issue15: issue15.err
Only in kleio-home/sources/reference_translations/issue15: issue15.files.json
Only in kleio-home/sources/reference_translations/issue15: issue15.org
Only in kleio-home/sources/reference_translations/issue15: issue15.rpt
Only in kleio-home/sources/reference_translations/issue15: issue15.xml
Comparing translation results finished.
```

These diffs show:
- Files present in one directory but not the other
- Line-by-line differences in file content
- Translation artifacts (err, rpt, xml files) that exist in the reference but not in the test

### Server Monitoring

The API server provides several monitoring endpoints and capabilities:

#### Home Page
The server home page (root URL) provides real-time status information:
- Current server time
- REST and JSON-RPC request counts
- Server configuration values
- Processing status of translation jobs

#### Activity Monitoring
The `show_server_activity/0` predicate provides detailed server status:
- Current processing status of jobs
- Active threads
- Request counts

### Logging

The system maintains comprehensive logs for debugging:
- **Request logging**: All incoming requests are logged with headers and parameters
- **Error logging**: Detailed error information is recorded
- **Translation logging**: Progress of translation jobs is tracked

Log entries include timestamps and contextual information to aid in debugging.

### Postman Collections

The API includes comprehensive Postman collections for testing and debugging:
- **api.json**: Main API specification with examples
- **api-tests.postman_collection.json**: Complete test suite for API functionality
- **environment.json**: Environment variables for testing
- **tests.postman_environment.json**: Test-specific environment configuration

These collections can be imported into Postman to test API endpoints and verify functionality.

**Section sources**
- [test_report_2025-12-13_11:19:25.diff](file://tests/reports/test_report_2025-12-13_11:19:25.diff)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json)
- [environment.json](file://api/postman/environment.json)
- [tests.postman_environment.json](file://api/postman/tests.postman_environment.json)
- [restServer.pl](file://src/restServer.pl)

## Migration Notes

This section provides information about backwards compatibility and migration considerations.

### Backwards Compatibility

The timelink-kleio API maintains backwards compatibility for existing clients:

- **Endpoint stability**: Core endpoints (/sources, /translations, /exports) have stable URL patterns
- **Response format consistency**: JSON response structures remain consistent across versions
- **Error code stability**: Standard HTTP status codes and JSON-RPC error codes are preserved

### Deprecation Policy

When features are deprecated:
1. They remain functional for at least two major releases
2. Deprecation notices are included in API responses
3. Documentation is updated to indicate deprecated features
4. Alternative approaches are provided

### Versioning

The API does not use URL versioning. Instead, versioning is managed through:
- **Semantic versioning** of the server software
- **Backwards-compatible changes** in API behavior
- **Deprecation cycles** for removing features

Clients should specify the exact server version they are tested against in their documentation.

### Breaking Changes

No breaking changes are planned for the core API endpoints. If breaking changes are necessary in the future:
1. A new API version will be introduced with a versioned URL path
2. The old version will be maintained for a migration period
3. Comprehensive migration guides will be provided

**Section sources**
- [api.json](file://api/postman/api.json)
- [restServer.pl](file://src/restServer.pl)