---
kind: error_handling
name: 'Dual-Layer Error Handling: JSON-RPC Codes and HTTP Replies'
category: error_handling
scope:
    - '**'
source_files:
    - src/errors.pl
    - src/restServer.pl
    - src/logging.pl
    - src/apiDirectories.pl
    - src/apiGit.pl
---

## Overview

The Kleio server implements a dual-layer error handling strategy that separates translation-time errors (file parsing, structure validation) from runtime API errors (JSON-RPC and REST endpoints). Each layer uses distinct conventions for signaling, propagating, and presenting failures.

## Translation-Time Errors (src/errors.pl)

The errors module provides the legacy translation pipeline's error reporting mechanism:
- Structured counters: initErrorCount/0, error_count/1, warning_count/1 track cumulative counts via shared state in counters.pl.
- Contextual messages: error_out/2 and warning_out/2 accept an option list with $file, $line_number, $line_text, $last_line_text to produce human-readable reports including source context.
- Abort policy: check_continuation/0 enforces a configurable maximum error threshold (max_errors, default 100); exceeding it causes translation to abort via fail.
- Report integration: All messages are routed through reports:report/1, which writes to .rpt files alongside translations.

This layer is used throughout the parser, lexical analyzer, and translator predicates - not by the HTTP server.

## Runtime API Errors (src/restServer.pl)

The REST/JSON-RPC server uses two parallel exception-based protocols:

### JSON-RPC Layer
- Standard codes: The module documents reserved ranges per JSON-RPC 2.0: -32700 (parse), -32600 (invalid request), -32601 (method not found), -32602 (invalid params), -32603 (internal error), -32000..-32099 (server-specific).
- Custom server codes: -32003 (directory not empty), -32004 (create dir failed), -32005 (dir exists), -32006 (forbidden), -32007 (destination exists), -32008 (resource not found), -32009 (dir not found), -32010 (copy dir failed).
- Throwing convention: Business logic throws error(Id, Code, Message) terms; the central return_error(json, ...) dispatcher matches these against specific patterns and emits a JSON-RPC {jsonrpc:'2.0', error:{code, message}, id} response.
- Parsing/validation wrappers: process_json_rpc_/1 wraps http_read_json/2 to convert parse exceptions into parse_error(Id, ErrorMessage); json_decode_command/4 validates required fields and throws invalid_params(Id, 'Missing parameter: ...').

### REST Layer
- HTTP reply exceptions: Handlers throw http_reply(StatusTerm, Headers, Context) using SWI-Prolog's built-in HTTP status constructors (not_found/1, bad_request/1, forbidden/1, method_not_allowed/2, server_error/1, etc.).
- Cross-wiring: Some JSON-RPC errors are caught by REST handlers and re-thrown as http_reply(resource_error(...)) or http_reply(bad_request(...)) to map semantic errors to HTTP semantics.
- Top-level catch-all: process_rest/1 wraps each request in catch(process_rest_request/1, Error, process_rest_error(Error)); process_rest_error/1 delegates to return_error(rest, Error), which either re-throws http_reply/1 (letting SWI handle it) or converts unknown exceptions to http_reply(server_error(...)).

### Centralized Formatting (return_error/2)
- return_error(rest, E): If E = http_reply(...), re-throw unchanged; otherwise log and wrap as server_error.
- return_error(json, E): Pattern-matches on known shapes (parse_error, invalid_request, method_not_found, invalid_params, internal_error, server_error, error(Id,Code,_), bare http_reply(...)) and serializes them via json_error_output(Code, Message, Id).

## Logging Integration (src/logging.pl)

Errors are consistently recorded through a structured logger:
- Levels: emerg, alert, crit, err, warning, notice, info, debug.
- log_error/2 is the primary call site for runtime errors; output goes to a file under ~/.kleio/logs/kleio_service.log (or current_output when started interactively).
- Timestamps and caller info are included; backtraces are commented out but available.

## Conventions for Developers

| Scenario | Throw pattern | Where it's handled |
|---|---|---|
| Missing/malformed JSON-RPC params | error(Id, -32602, 'Message') | return_error(json, ...) -> JSON-RPC error object |
| Resource not found (files/dirs) | error(Id, -32008/-32009, Path) or http_reply(not_found(Path)) | REST handler catches & maps to HTTP 404 |
| Directory operations | error(Id, -32003/-32004/-32005, Path) | REST handler maps to bad_request(directory_*) |
| Authentication/authorization | http_reply(forbidden(Context)) or http_reply(bad_request(token_missing)) | return_error(json,...) -> code -32006 |
| Unknown method | method_not_found(Id, Op) | json_exec/3 catches existence_error and raises this |
| Unexpected Prolog error | Any uncaught exception | Top-level catch -> server_error(-32000) / HTTP 500 |

Key files:
- src/errors.pl - translation-time error/warning reporting
- src/restServer.pl - JSON-RPC + REST error dispatch, HTTP mapping
- src/logging.pl - structured logging facade
- src/apiDirectories.pl, src/apiGit.pl - example consumers throwing http_reply(...) and error(Id,Code,Msg)