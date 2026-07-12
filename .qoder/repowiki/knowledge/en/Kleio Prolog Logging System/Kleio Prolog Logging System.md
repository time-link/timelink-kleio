---
kind: logging_system
name: Kleio Prolog Logging System
category: logging_system
scope:
    - '**'
source_files:
    - src/logging.pl
    - src/kleioFiles.pl
    - src/serverStart.pl
    - src/apiLog.pl
---

The Kleio server uses a custom SWI-Prolog logging module (src/logging.pl) that provides syslog-style log levels, file-based output, and runtime level control. It is the single logging subsystem used across all API modules.

### What system/approach is used
- Framework: A hand-written logging module (no external logging library).
- Log levels (in decreasing priority): emerg, alert, crit, err, warning, notice, info, debug. The default threshold is notice (priority index 5); lower-priority messages are silently dropped.
- Sinks: A single append-mode file stream identified by the shared property log/alias. By default it writes to <kleio_log_dir>/kleio_service.log; start_log(current_output) redirects to stdout.
- Format: Each line is [YYYY-MM-DD HH:MM:SS [LEVEL] Caller: message\n]. The caller field is currently empty in production builds; backtrace capture for errors is commented out.
- Runtime control: set_log_level/1 updates a global shared value; get_log_level/1 reads it. Levels are validated against log_levels/1.

### Key files and packages
- src/logging.pl - core logger: level definitions, log/3, convenience predicates log_debug/2 ... log_emergency/2, set_log_level/1, start_log/1, stop_log/0, sink management.
- src/kleioFiles.pl - resolves kleio_log_dir/1 from environment variables (see below), which determines the default log file path.
- src/serverStart.pl - sets set_log_level(debug) for debug/test server entry points.
- src/apiLog.pl - REST endpoint client_log/5 that lets authenticated clients write arbitrary messages into the same log stream via the API.

### Architecture and conventions
- Single source of truth: Every module imports logging and calls logging:log_* predicates; there is no per-module logger instance.
- Destination resolution order (via kleio_log_dir/1):
  1. $KLEIO_LOG_DIR env var -> <KLEIO_LOG_DIR>/kleio_service.log
  2. $KLEIO_HOME_DIR/system/conf/kleio/logs/ directory
  3. $HOME/.kleio/logs/ directory
  4. Falls back to current_output if none resolve.
- Shared state: Log destination alias, open flag, and current level live in SWI-Prolog's shared properties/values, making them process-global.
- API integration: apiLog:client_log/5 accepts {message,level} options and forwards to logging:log/3, gated by token permissions (files privilege required).

### Rules developers should follow
- Always call logging:log_<level>(Format, Args) rather than format(user_error,...) or writeln/1.
- Use the appropriate level:
  - debug / info for operational traces.
  - warning for recoverable anomalies.
  - error / critical / alert / emergency for failures that may abort an operation.
- Do not rely on the Caller field being populated; it is intentionally blank in non-debug builds.
- To change verbosity at runtime, call logging:set_log_level(Level) early in startup (e.g., serverStart.pl already does this for debug servers).
- For remote diagnostics, use POST /api/log with {token, message, level} instead of opening new sinks.
- Avoid calling open_log/1 directly; prefer start_log(Destination) so the shared state is set consistently.