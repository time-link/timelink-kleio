#!/usr/bin/env bash
# XML Structural Diff Skill Wrapper
# This script is invoked by Claude Code when using the /xml-diff command

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
python3 "$SCRIPT_DIR/xml-diff.py" "$@"
