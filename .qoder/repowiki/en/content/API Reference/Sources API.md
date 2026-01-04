# Sources API

<cite>
**Referenced Files in This Document**   
- [apiSources.pl](file://src/apiSources.pl)
- [restServer.pl](file://src/restServer.pl)
- [apiCommon.pl](file://src/apiCommon.pl)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json)
</cite>

## Table of Contents
1. [Introduction](#introduction)
2. [HTTP Methods and Operations](#http-methods-and-operations)
3. [Authentication and Authorization](#authentication-and-authorization)
4. [Query Parameters](#query-parameters)
5. [Request and Response Schemas](#request-and-response-schemas)
6. [Error Handling](#error-handling)
7. [Rate Limiting and Security](#rate-limiting-and-security)
8. [Client Implementation Guidelines](#client-implementation-guidelines)

## Introduction

The Sources API provides RESTful endpoints for managing source files within the timelink-kleio system. This API enables clients to perform file operations such as retrieving, uploading, copying, moving, and deleting source files and directories. The `/sources` endpoint serves as the primary interface for file management, supporting various HTTP methods to accommodate different operations.

Source files are typically stored in the `KLEIO_HOME/sources` directory by default, though this can be overridden by environment variables. The API supports both direct file access and directory traversal operations, with options for recursive processing and status filtering. All operations require proper authentication via bearer tokens, with permissions scoped to specific operations.

The API supports both REST and JSON-RPC interfaces, with content negotiation through Accept headers or query parameters. When requesting JSON output, the API returns structured data with file paths and metadata, while direct file requests return the raw file content with appropriate MIME types.

**Section sources**
- [apiSources.pl](file://src/apiSources.pl#L8-L11)
- [restServer.pl](file://src/restServer.pl#L109)
- [apiCommon.pl](file://src/apiCommon.pl#L48)

## HTTP Methods and Operations

The Sources API supports four primary HTTP methods: GET, POST, PUT, and DELETE, each corresponding to specific file operations.

### GET Operation

The GET method retrieves either a specific file or a directory listing from the sources directory. When requesting a file path, the API returns the file content directly. When requesting a directory path, it returns a list of `.cli` and `.kleio` files within that directory.

For directory listings, the API supports two response formats:
- When Accept header is `application/json`, returns a JSON array of file paths
- When no Accept header or `text/plain`, returns a direct file or directory listing

The API also supports the `url=yes` parameter, which returns a list of absolute URLs for downloading the files rather than just the file paths.

### POST Operation

The POST method supports two distinct operations based on the request content:

1. **Multipart Upload**: When the request contains multipart/form-data with a file parameter, the API uploads the file to the specified path. The destination must not exist unless using appropriate parameters.

2. **Copy Operation**: When the request includes an `origin` parameter specifying an existing file path, the API copies the source file to the destination path. The destination must not exist.

### PUT Operation

The PUT method supports two operations:

1. **Multipart Update**: When the request contains multipart/form-data with a file parameter, the API updates (overwrites) an existing file at the specified path.

2. **Move Operation**: When the request includes an `origin` parameter, the API moves the source file to the destination path, deleting the original file. The destination must not exist.

### DELETE Operation

The DELETE method removes source files or directories. When targeting a file, it deletes that specific file and all derived artifacts from translation processes. When targeting a directory, it deletes all files within the directory. The operation supports the `recurse=yes` parameter to delete files in subdirectories recursively.

Files that are currently being processed or queued for processing cannot be deleted.

**Section sources**
- [apiSources.pl](file://src/apiSources.pl#L28-L86)
- [apiCommon.pl](file://src/apiCommon.pl#L48-L54)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json#L49-L53)

## Authentication and Authorization

All operations on the Sources API require authentication using bearer tokens passed in the Authorization header with the format `Bearer <token>`.

```http
Authorization: Bearer abc123xyz
```

The API implements role-based access control with different permission levels:

- **Files Access**: Required for GET operations to read source files
- **Upload Permission**: Required for POST and PUT operations to create, upload, or modify files
- **Delete Permission**: Required for DELETE operations to remove files

Tokens are validated against the token database, and the associated user's permissions are checked before executing any operation. Users with limited privileges receive 403 Forbidden responses when attempting unauthorized operations.

The token information includes the user's allowed API operations and directory access restrictions, ensuring that users can only access files within their permitted directories.

**Section sources**
- [apiSources.pl](file://src/apiSources.pl#L89-L177)
- [restServer.pl](file://src/restServer.pl#L561-L562)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json#L19-L42)

## Query Parameters

The Sources API supports several query parameters to modify operation behavior and response format.

### recurse Parameter

The `recurse=yes` parameter controls directory traversal depth:

- When `recurse=yes`: The operation traverses all subdirectories recursively
- When `recurse=no` or omitted: The operation only processes the first level of the directory

This parameter is commonly used with GET requests to list all files in a directory tree or with DELETE operations to remove entire directory structures.

### tstatus Parameter

The `tstatus` parameter filters results based on translation status:

- `tstatus=T`: Returns files that have been translated
- `tstatus=D`: Returns directories (collections)
- When omitted: Returns all files regardless of status

This filtering helps clients identify which files have been processed and which are pending translation.

### id Parameter

The `id` parameter specifies a request identifier that is echoed back in the response headers:

```http
Request-id: 12345
```

This parameter helps clients correlate requests with responses, especially in asynchronous operations or when debugging issues.

### url Parameter

The `url=yes` parameter modifies directory listing responses to return absolute URLs for file downloads instead of just file paths. This is useful for clients that need direct download links.

**Section sources**
- [apiSources.pl](file://src/apiSources.pl#L46-L49)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json#L609-L630)

## Request and Response Schemas

### Request Schemas

#### File Upload Request
```json
POST /rest/sources/path/to/directory
Content-Type: multipart/form-data
Authorization: Bearer <token>

Form Data:
- file: <file_content>
- id: <request_id>
```

#### Copy Request
```json
POST /rest/sources/destination/path
Authorization: Bearer <token>

Query Parameters:
- origin: /sources/origin/path
- id: <request_id>
```

#### Move Request
```json
PUT /rest/sources/destination/path
Authorization: Bearer <token>

Query Parameters:
- origin: /sources/origin/path
- id: <request_id>
```

### Response Schemas

#### Successful File List Response (JSON)
```json
{
  "id": "request_123",
  "result": [
    "path/to/file1.cli",
    "path/to/file2.kleio",
    "subdir/file3.cli"
  ],
  "error": null
}
```

#### Successful File Upload Response
```json
{
  "id": "request_123",
  "result": [
    "path/to/uploaded_file.cli"
  ],
  "error": null
}
```

#### Error Response
```json
{
  "id": "request_123",
  "result": null,
  "error": {
    "code": 404,
    "message": "File not found",
    "data": {
      "file": "path/to/missing_file.cli"
    }
  }
}
```

#### Direct File Response
When requesting a specific file without JSON format, the API returns the raw file content with appropriate headers:
```http
HTTP/1.1 200 OK
Content-Type: text/plain
Location: /sources/path/to/file.cli
Request-id: 12345

<file content>
```

**Section sources**
- [apiSources.pl](file://src/apiSources.pl#L212-L233)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json#L800-L917)

## Error Handling

The Sources API implements comprehensive error handling with appropriate HTTP status codes and error responses.

### Common Error Types

#### 400 Bad Request
Returned when:
- File upload request lacks a file parameter
- Destination already exists for copy/move operations
- Directory does not exist for file operations
- Invalid parameters in the request

#### 403 Forbidden
Returned when:
- User lacks required permissions for the operation
- Token is valid but doesn't grant access to the requested resource
- User is attempting to access restricted directories

#### 404 Not Found
Returned when:
- Requested file or directory does not exist
- Path is invalid or malformed
- User token doesn't provide access to the specified path

#### 409 Conflict
Returned when:
- Destination file already exists for copy/move operations
- File is currently being processed and cannot be modified

### Error Response Structure

All errors return structured JSON responses with the following format:
```json
{
  "id": "<request_id>",
  "result": null,
  "error": {
    "code": <http_status_code>,
    "message": "<error_description>",
    "data": {
      "file": "<affected_file_path>",
      "directory": "<affected_directory_path>"
    }
  }
}
```

The API also includes a Request-id header in error responses to help with debugging and request tracking.

Special cases include file access conflicts where files are being processed, which return specific error codes to indicate the file is locked.

**Section sources**
- [apiSources.pl](file://src/apiSources.pl#L106-L107)
- [apiSources.pl](file://src/apiSources.pl#L341-L346)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json#L978-L1031)

## Rate Limiting and Security

### Rate Limiting Considerations

While the current implementation does not enforce explicit rate limits, bulk operations should be handled with care:

- Large file uploads should be chunked when possible
- Bulk deletions with `recurse=yes` may impact system performance
- Concurrent operations on the same files may cause conflicts

Clients should implement reasonable retry logic with exponential backoff for failed requests, particularly for operations that fail due to file locking.

### Security Implications

The Sources API implements several security measures:

#### File Path Validation
All file paths are resolved relative to the user's source directory, preventing directory traversal attacks. The `kleio_resolve_source_file` function ensures paths stay within permitted directories.

#### Permission Scoping
Tokens are associated with specific directory access rights, limiting users to their designated areas. For example, a user token might only have access to `sources/test_translations` while an administrator token has broader access.

#### File Type Restrictions
The API primarily handles `.cli` and `.kleio` files, reducing the risk of executing malicious scripts. File uploads are stored in designated upload directories before processing.

#### Concurrent Access Protection
Files being processed or queued for translation cannot be deleted, preventing race conditions and data corruption.

#### Secure Token Management
Tokens should be treated as sensitive credentials and transmitted only over HTTPS. The system supports token invalidation to revoke access when needed.

**Section sources**
- [apiSources.pl](file://src/apiSources.pl#L101-L102)
- [apiSources.pl](file://src/apiSources.pl#L306-L314)
- [apiSources.pl](file://src/apiSources.pl#L66-L67)

## Client Implementation Guidelines

### Handling File Downloads

When downloading files, clients should:

1. Use the Accept header to specify response format:
   ```http
   Accept: application/json  # For metadata
   Accept: text/plain        # For file content
   ```

2. Handle both direct file responses and JSON metadata responses appropriately.

3. Follow Location headers when redirected to file downloads.

4. Implement proper error handling for 404 and 403 responses.

### Handling File Uploads

For multipart file uploads, clients should:

1. Use the correct content type:
   ```http
   Content-Type: multipart/form-data; boundary=----WebKitFormBoundary
   ```

2. Include the file in the form data with the field name "file".

3. Handle 400 responses for missing files or 409 for existing destinations.

4. Verify successful uploads by checking the response and optionally retrieving the file.

### Copy and Move Operations

When performing copy or move operations:

1. Use the origin parameter to specify the source path:
   ```
   POST /rest/sources/destination?origin=/sources/origin
   ```

2. Handle 404 responses when the origin file doesn't exist.

3. Handle 409 responses when the destination already exists.

4. For move operations, verify the original file has been removed.

### Directory Operations

For directory listings and recursive operations:

1. Use the recurse=yes parameter for full directory traversal.

2. Use tstatus filters to identify file types and statuses.

3. Handle pagination for large directory listings (if implemented).

4. Be aware that directory operations may take longer to complete.

### Error Handling Best Practices

Clients should implement robust error handling:

1. Check HTTP status codes and response content types.

2. Parse JSON error responses for detailed error information.

3. Implement retry logic with exponential backoff for transient errors.

4. Log Request-id values for debugging purposes.

5. Handle authentication errors by refreshing tokens when necessary.

**Section sources**
- [apiSources.pl](file://src/apiSources.pl#L323-L352)
- [apiSources.pl](file://src/apiSources.pl#L364-L382)
- [apiSources.pl](file://src/apiSources.pl#L391-L410)