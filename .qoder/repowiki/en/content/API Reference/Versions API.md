# Versions API

<cite>
**Referenced Files in This Document**   
- [apiGit.pl](file://src/apiGit.pl)
- [gitUtilities.pl](file://src/gitUtilities.pl)
- [kleioFiles.pl](file://src/kleioFiles.pl)
- [restServer.pl](file://src/restServer.pl)
- [api.json](file://api/postman/api.json)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json)
</cite>

## Table of Contents
1. [Introduction](#introduction)
2. [Authentication and Authorization](#authentication-and-authorization)
3. [API Endpoints](#api-endpoints)
4. [Request/Response Schemas](#requestresponse-schemas)
5. [Source Directory and Git Repository Mapping](#source-directory-and-git-repository-mapping)
6. [Automatic Commit Generation](#automatic-commit-generation)
7. [Error Handling](#error-handling)
8. [Security Considerations](#security-considerations)
9. [Client Implementation Guidelines](#client-implementation-guidelines)
10. [Postman Collection Examples](#postman-collection-examples)

## Introduction
The Versions API provides a comprehensive set of JSON-RPC methods for managing Git version control operations within the timelink-kleio system. This API enables users to perform essential Git functions such as repository initialization, committing changes, pushing to and pulling from remotes, checking repository status, and managing user information. The API is designed to support collaborative translation workflows, allowing multiple users to work on the same set of source files while maintaining version control and synchronization.

The API operates on Kleio source directories, which are mapped to Git repositories, enabling version tracking of translation projects. All operations are performed through JSON-RPC calls, with authentication handled via bearer tokens. The API provides detailed status information about the repository, including divergence from the remote branch, local changes, and commit history.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L1-L201)
- [gitUtilities.pl](file://src/gitUtilities.pl#L1-L686)

## Authentication and Authorization
The Versions API uses bearer token authentication for all requests. Clients must include a valid bearer token in the Authorization header of each request. The token is validated against the system's token database, and access to Git operations is granted based on the permissions associated with the token.

Authorization is enforced at the directory level, with tokens having specific permissions to access certain directories within the Kleio home directory. The `is_api_allowed/2` predicate in the `apiGit.pl` module checks whether a token has the necessary permissions to perform Git operations on a specific directory. Only tokens with the "files" permission can execute Git commands.

Tokens are generated through the `tokens_generate` method, which creates a token with specific permissions and access to designated source and structure directories. The token information is passed to the Git operations through the `token_info` parameter, which contains details about the user's access rights and directory mappings.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L38-L41)
- [apiGit.pl](file://src/apiGit.pl#L55-L58)
- [apiGit.pl](file://src/apiGit.pl#L75-L78)
- [apiGit.pl](file://src/apiGit.pl#L88-L91)
- [apiGit.pl](file://src/apiGit.pl#L101-L104)
- [apiGit.pl](file://src/apiGit.pl#L114-L117)
- [apiGit.pl](file://src/apiGit.pl#L130-L133)
- [apiGit.pl](file://src/apiGit.pl#L147-L150)
- [tokens.pl](file://src/apiTokens.pl)

## API Endpoints
The Versions API provides the following JSON-RPC methods for Git operations:

### versions_get_global_status
Retrieves the global status of a Git repository, including information about the current branch, remote tracking, divergence from the remote branch, and local changes. This method corresponds to the `git status` command and provides a comprehensive overview of the repository's state.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L157-L160)
- [apiGit.pl](file://src/apiGit.pl#L51-L68)

### versions_get_remotes_branches
Fetches information about remote branches associated with the repository. This includes details about each remote branch, such as its name, the number of commits ahead and behind the current branch, and information about the remote repository.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L161-L164)
- [apiGit.pl](file://src/apiGit.pl#L34-L48)

### versions_pull
Performs a Git pull operation to fetch and merge changes from the remote repository into the local branch. This method updates the local repository with changes from the remote, resolving any conflicts that may arise.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L169-L173)
- [apiGit.pl](file://src/apiGit.pl#L71-L82)

### versions_push
Pushes local commits to the remote repository. This method uploads the local branch's commits to the configured remote, making them available to other collaborators.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L174-L177)
- [apiGit.pl](file://src/apiGit.pl#L97-L108)

### versions_commit
Creates a new commit with the specified files and commit message. This method stages the specified files and creates a commit with the provided message, recording the changes in the repository's history.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L178-L181)
- [apiGit.pl](file://src/apiGit.pl#L110-L124)

### versions_set_user_info
Sets the Git user information (name and email) for the repository. This information is used in commit messages to identify the author of changes.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L182-L185)
- [apiGit.pl](file://src/apiGit.pl#L126-L139)

### versions_reset
Resets the repository to a specified commit, discarding changes made after that point. This method can be used to undo changes or revert to a previous state.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L186-L189)
- [apiGit.pl](file://src/apiGit.pl#L141-L154)

## Request/Response Schemas
The Versions API uses JSON-RPC 2.0 for communication, with requests and responses following the standard JSON-RPC format.

### Request Schema
All requests follow the JSON-RPC 2.0 format:
```json
{
  "jsonrpc": "2.0",
  "method": "method_name",
  "params": {
    "path": "repository_path",
    "token": "bearer_token",
    "additional_params": "value"
  },
  "id": "request_id"
}
```

The `params` object contains the following common parameters:
- `path`: The path to the repository or file within the Kleio source directory
- `token`: The bearer token for authentication
- Additional parameters specific to the method being called

### Response Schema
Successful responses follow the JSON-RPC 2.0 success format:
```json
{
  "jsonrpc": "2.0",
  "result": {
    "property": "value"
  },
  "id": "request_id"
}
```

Error responses follow the JSON-RPC 2.0 error format:
```json
{
  "jsonrpc": "2.0",
  "error": {
    "code": error_code,
    "message": "error_message"
  },
  "id": "request_id"
}
```

The specific structure of the `result` object varies depending on the method called, containing the relevant data for the operation performed.

**Section sources**
- [api.json](file://api/postman/api.json#L2-L7)
- [apiGit.pl](file://src/apiGit.pl#L192-L198)

## Source Directory and Git Repository Mapping
The timelink-kleio system maps Kleio source directories to Git repositories, enabling version control of translation projects. The mapping is established through the token system, where each token is associated with a specific source directory within the Kleio home directory.

The `kleio_resolve_source_file/3` predicate in the `apiGit.pl` module resolves a relative path to an absolute path within the user's source directory. This function uses the token information to determine the base directory for the user's sources and then constructs the full path to the requested file or directory.

The Kleio home directory structure follows a standard layout:
- `/sources` or `/projects`: Base directory for source files
- `/users/<username>/stru`: Directory for user-specific structure files
- `/system/conf/kleio`: Configuration directory containing the token database and default structure files

When a Git operation is performed, the system resolves the requested path relative to the user's source directory, ensuring that users can only access files within their authorized directories. This mapping allows multiple users to work on different parts of the same project while maintaining isolation and security.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L60-L65)
- [apiGit.pl](file://src/apiGit.pl#L80-L85)
- [kleioFiles.pl](file://src/kleioFiles.pl#L733-L738)
- [kleioFiles.pl](file://src/kleioFiles.pl#L746-L750)

## Automatic Commit Generation
The timelink-kleio system automatically generates commits during translation operations to track changes to source files. When a translation is completed, the system creates a commit with a standardized message format that includes information about the translation process.

The commit message typically includes details such as the source file name, translation date, and any relevant metadata. This automatic commit generation ensures that all changes to source files are recorded in the repository history, providing a complete audit trail of the translation process.

The system also handles the staging of files before committing, ensuring that only the relevant files are included in each commit. This includes the source file (.cli), translation report (.rpt), error file (.err), and XML output file (.xml). The automatic commit process helps maintain a clean and organized repository history, making it easier to track changes and collaborate on translation projects.

**Section sources**
- [kleioFiles.pl](file://src/kleioFiles.pl#L69-L91)
- [gitUtilities.pl](file://src/gitUtilities.pl#L371-L424)

## Error Handling
The Versions API implements comprehensive error handling for Git operations, providing detailed information about failures and issues encountered during execution.

### Merge Conflicts
When a pull operation results in merge conflicts, the Git command returns a non-zero exit status, and the error output contains information about the conflicting files. The API returns this information to the client, allowing them to resolve the conflicts manually. Clients should implement logic to detect merge conflicts and prompt users to resolve them before proceeding with other operations.

### Network Issues
Network issues during push or pull operations are handled by the underlying Git commands, which return specific error codes and messages. The API captures these errors and returns them to the client, including information about connection timeouts, authentication failures, and repository not found errors. Clients should implement retry logic with exponential backoff to handle transient network issues.

### Authentication Failures
Authentication failures occur when the provided bearer token is invalid, expired, or lacks the necessary permissions for the requested operation. The API returns a 401 Unauthorized error with a descriptive message. Clients should handle this by prompting the user to obtain a new token or verify their credentials.

### Repository State Errors
The API checks the repository state before performing operations, returning appropriate errors for invalid states. For example, attempting to commit when there are no changes staged results in an error message indicating that there is nothing to commit. Similarly, attempting to push when the local branch is behind the remote branch may result in an error, requiring the client to pull first.

All errors are returned in the JSON-RPC error format, with a descriptive message and error code to help clients diagnose and handle issues appropriately.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L47-L48)
- [apiGit.pl](file://src/apiGit.pl#L64-L65)
- [apiGit.pl](file://src/apiGit.pl#L81-L82)
- [apiGit.pl](file://src/apiGit.pl#L107-L108)
- [gitUtilities.pl](file://src/gitUtilities.pl#L209-L217)
- [gitUtilities.pl](file://src/gitUtilities.pl#L256-L264)

## Security Considerations
The Versions API implements several security measures to protect repository access and user data.

### Credential Management
Bearer tokens are used for authentication, with tokens generated through a secure process that associates them with specific user permissions and directory access. Tokens should be stored securely by clients and transmitted over HTTPS to prevent interception. The system supports token invalidation through the `users_invalidate` method, allowing administrators to revoke access when necessary.

### Repository Access Control
Access to Git repositories is controlled through the token system, with each token granting access to specific directories within the Kleio home directory. This ensures that users can only access repositories they are authorized to work on, preventing unauthorized access to sensitive data. The `is_api_allowed/2` predicate enforces these access controls at the API level.

### Input Validation
All input parameters are validated before being passed to Git commands, preventing command injection attacks. The system uses the `option/3` predicate to safely extract parameters from the request, ensuring that only expected parameters are processed. Path resolution is performed using the `kleio_resolve_source_file/3` predicate, which validates that the requested path is within the user's authorized directory.

### Secure Communication
All API communication should occur over HTTPS to encrypt data in transit and prevent eavesdropping. The system should be configured with valid SSL certificates, and clients should verify the server's identity when establishing connections.

These security measures work together to protect the integrity of the Git repositories and ensure that only authorized users can perform version control operations.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl#L38-L41)
- [apiGit.pl](file://src/apiGit.pl#L55-L58)
- [apiGit.pl](file://src/apiGit.pl#L75-L78)
- [apiGit.pl](file://src/apiGit.pl#L88-L91)
- [apiGit.pl](file://src/apiGit.pl#L101-L104)
- [apiGit.pl](file://src/apiGit.pl#L114-L117)
- [apiGit.pl](file://src/apiGit.pl#L130-L133)
- [apiGit.pl](file://src/apiGit.pl#L147-L150)
- [kleioFiles.pl](file://src/kleioFiles.pl#L60-L65)
- [kleioFiles.pl](file://src/kleioFiles.pl#L80-L85)

## Client Implementation Guidelines
When implementing a client for the Versions API, consider the following guidelines to ensure robust and efficient integration.

### Connection Management
Establish a persistent connection to the API server to reduce latency and improve performance. Use connection pooling if multiple operations are performed in sequence. Always handle connection errors gracefully, with retry logic for transient issues.

### Error Handling
Implement comprehensive error handling for all API calls, parsing the JSON-RPC error responses to determine the appropriate action. For merge conflicts, provide a user interface for resolving conflicts manually. For authentication failures, prompt the user to refresh their token.

### State Management
Maintain local state information about the repository to minimize API calls. Cache repository status information and update it only when necessary. Implement polling or use webhooks (if available) to detect changes to the repository state.

### User Experience
Provide clear feedback to users about the progress of Git operations, especially for long-running operations like push and pull. Display detailed information about conflicts and errors, helping users understand and resolve issues. Implement undo functionality where possible, allowing users to revert changes if needed.

### Performance Optimization
Batch multiple operations when possible to reduce the number of API calls. Use the `recurse` parameter to retrieve directory listings efficiently. Implement caching of frequently accessed data, such as repository status and branch information.

### Distributed Collaboration
Support distributed collaboration scenarios by implementing conflict detection and resolution workflows. Notify users of changes made by other collaborators and provide tools for merging changes. Implement locking mechanisms or optimistic concurrency control to prevent overwriting changes.

By following these guidelines, clients can provide a seamless and efficient experience for users working with the Versions API in collaborative translation projects.

**Section sources**
- [apiGit.pl](file://src/apiGit.pl)
- [gitUtilities.pl](file://src/gitUtilities.pl)
- [restServer.pl](file://src/restServer.pl)

## Postman Collection Examples
The Postman collection provides examples of API requests for the Versions API, demonstrating how to perform various Git operations.

### Repository Status Check
The `versions_get_global_status` request demonstrates how to check the status of a repository. The request includes the path to the repository and the bearer token for authentication. The response contains detailed information about the repository's state, including the current branch, remote tracking, and local changes.

### Pull Operation
The `versions_pull` request shows how to pull changes from the remote repository. The request specifies the path to the repository and includes the bearer token. The response contains the output from the Git pull command, including information about merged commits and any conflicts.

### Push Operation
The `versions_push` request demonstrates how to push local commits to the remote repository. The request includes the repository path and authentication token. The response contains the output from the Git push command, indicating whether the push was successful.

### Commit Creation
The `versions_commit` request shows how to create a new commit with specified files and a commit message. The request includes the path to the repository, the files to commit, the commit message, and the authentication token. The response contains the output from the Git commit command.

### User Information Management
The `versions_set_user_info` request demonstrates how to set the Git user information for a repository. The request includes the repository path, the user's name and email, and the authentication token. The response confirms that the user information was updated successfully.

These examples in the Postman collection provide a practical guide for implementing client applications that interact with the Versions API, showing the correct request format and expected responses for each operation.

**Section sources**
- [api.json](file://api/postman/api.json)
- [api-tests.postman_collection.json](file://api/postman/api-tests.postman_collection.json)