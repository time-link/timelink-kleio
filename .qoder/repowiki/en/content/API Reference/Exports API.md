# Exports API

<cite>
**Referenced Files in This Document**   
- [apiExports.pl](file://src/apiExports.pl)
- [apiSources.pl](file://src/apiSources.pl)
- [restServer.pl](file://src/restServer.pl)
- [gactoxml.pl](file://src/gactoxml.pl)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json)
</cite>

## Table of Contents
1. [Introduction](#introduction)
2. [API Endpoint Overview](#api-endpoint-overview)
3. [Relationship Between Source Files and Exports](#relationship-between-source-files-and-exports)
4. [Error Handling](#error-handling)
5. [Caching and Performance](#caching-and-performance)
6. [Client Implementation Guidance](#client-implementation-guidance)
7. [Postman Collection Examples](#postman-collection-examples)

## Introduction
The Exports API provides access to XML export files generated from translated Kleio sources within the timelink-kleio system. This API endpoint allows clients to retrieve normalized XML representations of historical data extracted from Kleio source files. The `/exports` endpoint is a GET method that serves as the primary interface for accessing structured data derived from the translation process. Authentication is required via bearer tokens, ensuring secure access to export files. The API is designed to handle both individual file requests and directory listings, providing flexibility for different use cases. This documentation details the complete functionality of the exports endpoint, including its behavior with file extensions, directory paths, response formats, and integration with the underlying translation system.

## API Endpoint Overview

The `/exports` endpoint follows a RESTful design pattern and is implemented as a GET method for retrieving XML export files. The endpoint URL pattern is `/exports/path`, where `path` represents the relative path to a source file or directory within the user's accessible source directory. The HTTP method is exclusively GET, as this endpoint is designed for data retrieval only.

When a client makes a request to a specific file path (e.g., `/exports/paroquiais/baptismos/b1685.cli`), the server returns the corresponding XML export file with the appropriate `Content-Type: application/xml` header. Notably, the file extension in the request is ignored, meaning that requests for `/exports/paroquiais/baptismos/b1685.xml`, `/exports/paroquiais/baptismos/b1685.kleio`, or `/exports/paroquiais/baptismos/b1685` will all return the same XML export file, as the system maps the source file to its generated XML representation regardless of the extension provided in the request.

When the path parameter refers to a directory rather than a file, the endpoint returns a list of available export files within that directory and its subdirectories (if the `recurse=yes` parameter is included). This behavior allows clients to discover available exports without prior knowledge of specific file names.

Authentication is implemented using bearer tokens passed in the Authorization header (`Authorization: Bearer <token>`). The token is validated against the user's permissions, and access is granted based on the token's associated privileges. The API supports both REST and JSON-RPC protocols, with response format determined by the `Accept` header or `json` parameter in the request.

**Section sources**
- [apiExports.pl](file://src/apiExports.pl#L1-L21)
- [apiCommon.pl](file://src/apiCommon.pl#L60-L60)
- [restServer.pl](file://src/restServer.pl#L469-L545)

## Relationship Between Source Files and Exports

The relationship between source files, translation results, and exported XML is a fundamental aspect of the timelink-kleio system's data processing pipeline. This process begins with Kleio source files (typically with `.cli` or `.kleio` extensions) that contain transcribed historical data using the Kleio notation system. These source files are processed by the translation system, which applies structural definitions from `.str` files to interpret the data.

The translation process, implemented in `gactoxml.pl`, performs a normalization of the source data into a structured XML format. This normalization process involves several key steps: parsing the Kleio notation, validating data against the structure definition, resolving hierarchical relationships between entities (such as acts, persons, and objects), and generating standardized XML output. The `gactoxml.pl` module acts as an export module for the Clio translator, implementing the required predicates `db_init/0`, `db_store/0`, and `db_close/0` to interface with the translation system.

During translation, the system creates several derivative files, with the XML export being the primary structured output. The XML schema is defined in `kleioExport.xsd`, ensuring consistency across all generated exports. The translation process also generates auxiliary files such as `.rpt` (reports), `.err` (errors), and `.ids` (pretty-printed versions with IDs), but the XML export contains the normalized data in a format suitable for downstream processing and integration.

The relationship between source and export is maintained through filename mapping, where an input file `path/to/file.cli` generates an export file `path/to/file.xml`. This one-to-one mapping allows the exports API to resolve requests for XML files based on the source file path, regardless of the extension provided in the request.

**Section sources**
- [gactoxml.pl](file://src/gactoxml.pl#L1-L800)
- [apiSources.pl](file://src/apiSources.pl#L89-L104)
- [kleioExport.xsd](file://src/kleioExport.xsd)

## Error Handling

The Exports API implements comprehensive error handling for various failure scenarios, returning appropriate HTTP status codes and error messages to help clients diagnose issues. The primary error conditions include missing exports, conversion failures, and permission issues.

When a requested export does not exist, the API returns a 404 Not Found status code. This occurs when the source file has not been translated, the translation failed, or the export file was deleted. The error response includes a descriptive message indicating that the resource was not found, along with the request ID for correlation with server logs.

Permission issues are handled with a 403 Forbidden status code. This occurs when the authenticated user's token does not have sufficient privileges to access the requested resource. The token's permissions are validated against the user's access rights, which are defined when the token was generated. Users require the "sources" permission to access export files, and attempts to access restricted directories will result in a 403 response.

Conversion failures during the translation process are typically detected before the export stage, but if an export file becomes corrupted or unreadable, the system returns a 500 Internal Server Error. This indicates a problem with the server-side processing rather than a client request issue.

Authentication errors, such as missing or invalid tokens, result in a 401 Unauthorized status code. The API requires a valid bearer token in the Authorization header, and requests without proper authentication are rejected immediately.

All error responses include a `Request-id` header that matches the `id` parameter from the request, enabling correlation between client requests and server-side processing for debugging purposes.

**Section sources**
- [restServer.pl](file://src/restServer.pl#L1412-L1599)
- [apiSources.pl](file://src/apiSources.pl#L106-L107)
- [apiExports.pl](file://src/apiExports.pl#L14-L16)

## Caching and Performance

The Exports API's performance characteristics are influenced by both caching mechanisms and the on-demand nature of XML generation. The system implements a hybrid approach to handling export requests, balancing immediate availability with computational efficiency.

For frequently accessed exports, the system benefits from filesystem-level caching, where recently accessed XML files are likely to be served from memory rather than disk. However, the timelink-kleio system does not implement application-level caching of export responses, meaning each request for an existing XML file results in a filesystem read operation.

A significant performance consideration is that XML generation is not performed on-demand for export requests. Instead, XML files are generated during the translation process, which is a separate operation triggered by the `/translations` endpoint. When a client requests an export file, the system serves the pre-generated XML file if it exists. If no export exists (because translation has not occurred), the request fails with a 404 error rather than triggering translation.

This separation of concerns means that export retrieval is generally fast for translated sources, as it involves only a filesystem read and HTTP response. However, clients requiring up-to-date exports must first ensure translation has occurred, which can be a time-consuming process depending on the size and complexity of the source file.

For directory listings, the performance impact scales with the number of files in the directory tree, as the system must scan the filesystem to enumerate available exports. The use of the `recurse=yes` parameter can significantly increase response time for directories with deep nesting or large numbers of files.

**Section sources**
- [gactoxml.pl](file://src/gactoxml.pl#L123-L166)
- [apiSources.pl](file://src/apiSources.pl#L263-L285)
- [restServer.pl](file://src/restServer.pl#L1014-L1034)

## Client Implementation Guidance

Clients integrating with the Exports API should follow specific patterns for handling XML responses and integrating with downstream systems. The primary consideration is proper authentication using bearer tokens, which must be included in the Authorization header of all requests.

When retrieving individual export files, clients should expect XML content with the `Content-Type: application/xml` header. The XML structure follows the schema defined in `kleioExport.xsd`, and clients should validate or parse the response accordingly. For applications that need to discover available exports, requesting a directory path (with optional `recurse=yes` parameter) returns a list of available files, which can be used to construct subsequent individual file requests.

Error handling in client code should account for the various HTTP status codes returned by the API. A 404 response indicates the export does not exist, which may require the client to initiate a translation process before retrying. A 403 response indicates insufficient permissions, requiring either a different token or administrative action to modify permissions. All error responses include a `Request-id` header that should be logged for debugging purposes.

For performance optimization, clients should implement their own caching layer for frequently accessed exports, as the server does not provide cache headers or ETag support. When processing large numbers of exports, clients should consider the performance implications of directory listing operations and may benefit from maintaining their own index of available exports.

Integration with downstream systems should account for the hierarchical nature of the data in the XML exports, which represents complex relationships between historical entities such as acts, persons, and objects. The normalized structure facilitates data import into databases or analysis tools that can process hierarchical XML data.

**Section sources**
- [apiExports.pl](file://src/apiExports.pl#L14-L16)
- [gactoxml.pl](file://src/gactoxml.pl#L444-L458)
- [apiSources.pl](file://src/apiSources.pl#L212-L218)

## Postman Collection Examples

The Postman collection provides practical examples of interacting with the Exports API, demonstrating both successful requests and error conditions. These examples illustrate the expected request structure, headers, and response patterns.

For retrieving an individual export file, a GET request is made to `/rest/exports/path/to/file` with a bearer token in the Authorization header. The response contains the XML content with appropriate headers, including `Content-Type: application/xml` and `Request-id` matching the request parameter. Tests in the collection verify the 200 status code, presence of the Request-id header, and that the response contains expected XML content.

For directory listings, a GET request to a directory path returns a JSON array of available export files when the Accept header specifies `application/json`. The collection includes tests that verify the response includes nested directories when `recurse=yes` is specified, demonstrating the recursive directory traversal capability.

The collection also includes examples of error conditions, such as requests for non-existent files (expected to return 404) and requests with insufficient permissions (expected to return 403). These tests validate that the API properly enforces access controls and returns appropriate error responses.

The examples demonstrate the use of environment variables for dynamic values like the endpoint URL and authentication tokens, making the collection reusable across different deployment environments. The tests also show how to extract random file paths from directory listings for use in subsequent requests, illustrating a pattern for automated testing of the API.

**Section sources**
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json#L800-L1200)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json#L1200-L1999)