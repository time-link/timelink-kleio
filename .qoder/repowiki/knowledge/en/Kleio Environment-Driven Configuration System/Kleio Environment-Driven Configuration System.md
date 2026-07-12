---
kind: configuration_system
name: Kleio Environment-Driven Configuration System
category: configuration_system
scope:
    - '**'
source_files:
    - src/kleioFiles.pl
    - src/restServer.pl
    - src/serverStart.pl
    - .env-sample
    - .kleio.json
---

## Overview

The Kleio server uses a purely environment-variable-driven configuration system with no dedicated config file parser. All runtime behavior is controlled through `KLEIO_*` environment variables, resolved at startup by Prolog predicates in `src/kleioFiles.pl`. The system follows a layered home-directory convention where paths are discovered by probing filesystem layout rather than declared in a single manifest.

## How Configuration Is Loaded

1. **Environment-first resolution**: Every path and setting is read via `getenv('KLEIO_...', Var)` before any filesystem probe. If an env var is set, it is used verbatim (after `absolute_file_name/2`).
2. **Home directory discovery** (`kleio_home_dir/1`): A multi-rule predicate tries these locations in order:
   - `$KLEIO_HOME_DIR` if it exists
   - `/kleio-home`, `/timelink-home`, `/mhk-home` (container defaults)
   - Current working directory if it contains `system/`, `sources|projects/`, `users/`
   - `./kleio-home`, `./tests/kleio-home`, `./timelink-home`, `./mhk-home`
   - `~/kleio-home`, `~/timelink-home`, `~/mhk-home`
3. **Derived directories** (`kleio_conf_dir/1`, `kleio_source_dir/1`, `kleio_stru_dir/1`, `kleio_log_dir/1`, `kleio_token_db/1`, `kleio_default_stru/1`) each follow the same pattern: check env var → compute from `kleio_home_dir` → fall back to another location or create it.

## Key Environment Variables

| Variable | Purpose | Default |
|---|---|---|
| `KLEIO_HOME_DIR` | Base home directory | auto-discovered as above |
| `KLEIO_SOURCE_DIR` | Root of `.cli` source files | `$KLEIO_HOME_DIR/sources` |
| `KLEIO_CONF_DIR` | Configuration directory | `$KLEIO_HOME_DIR/system/conf/kleio` |
| `KLEIO_STRU_DIR` | Global structure definitions | `$KLEIO_CONF_DIR/stru` |
| `KLEIO_TOKEN_DB` | Token database file | `$KLEIO_CONF_DIR/token_db` |
| `KLEIO_DEFAULT_STRU` | Default structure file | `$KLEIO_STRU_DIR/gacto2.str` |
| `KLEIO_LOG_DIR` | Log output directory | `$KLEIO_HOME_DIR/.kleio/logs` |
| `KLEIO_SERVER_PORT` | REST server port | `8088` |
| `KLEIO_DEBUGGER_PORT` | Debug server port | `4000` |
| `KLEIO_SERVER_WORKERS` | Worker thread count | `3` |
| `KLEIO_IDLE_TIMEOUT` | HTTP idle timeout (seconds) | `900` |
| `KLEIO_ADMIN_TOKEN` | Admin bearer token | required for admin endpoints |
| `KLEIO_CORS_SITES` | CORS allowed origins | `*` |
| `KLEIO_DEBUG` | Enable debug logging | `false` |

## Runtime Server Configuration

Server-level settings (port, workers, timeout, CORS) are resolved in `restServer.pl` via `default_value/2` predicates that read `KLEIO_*` env vars and provide hard-coded fallbacks. These values are printed by `print_server_config/0` at startup and also exposed through the JSON-RPC API under the `server_info` response.

## Home Directory Layout Convention

The expected filesystem tree rooted at `KLEIO_HOME_DIR`:

```
KLEIO_HOME_DIR/
├── system/
│   └── conf/
│       └── kleio/          ← KLEIO_CONF_DIR
│           ├── stru/       ← global .str / .yaml structures
│           ├── token_db    ← token store
│           └── .admin_token← admin token file
├── sources/                ← KLEIO_SOURCE_DIR
├── users/                  ← per-user overrides
└── .kleio/                 ← fallback when MHK-style layout not found
    ├── conf/
    └── logs/
```

When the MHK-style layout (`system/`, `sources/`, `users/`) is absent, the server creates `./.kleio/conf` and `./.kleio/logs` automatically.

## Secrets & Tokens

- **Admin token**: Either via `KLEIO_ADMIN_TOKEN` env var or read from `$KLEIO_CONF_DIR/.admin_token` (resolved by `kleio_admin_token_path/1`).
- **Token database**: File-backed store at `KLEIO_TOKEN_DB`; initialized lazily via `tokens:ensure_db`.
- The root `.kleio.json` in this repo is a *generated snapshot* of the running server's configuration (paths, version, URL, token status) — not an input file. It is produced by the server itself for external tooling.

## Docker / Compose Integration

`.env-sample` documents all variables; `docker-compose.yaml` maps `KLEIO_HOME_DIR` to `/kleio-home` inside the container. The Makefile targets (`make start-server`, `make image`, etc.) pass these variables into the container process.

## Rules for Developers

1. **Never hard-code paths** — always call `kleiofiles:kleio_home_dir/1`, `kleiofiles:kleio_conf_dir/1`, etc.
2. **Prefer env vars over files** — there is no config-file loader; add a new `KLEIO_*` variable and a corresponding `kleio_*_dir/1` clause if you need a new path.
3. **Keep MHK layout intact** — the home discovery relies on the presence of `system/`, `sources/`, `users/` subdirectories.
4. **Use `default_value/2` for server knobs** — ports, workers, timeouts belong in `restServer.pl`'s `default_value` clauses, not in `kleioFiles.pl`.
5. **Do not commit secrets** — tokens go in `.admin_token` or env; the generated `.kleio.json` should be ignored by VCS.