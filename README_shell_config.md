# Shell Configuration Optimization Guide

This document explains the issues with your current shell configuration and provides recommendations for improvement.

## Current Issues

1. **Redundant Configuration**: Both `.zprofile` and `.zshrc` contain the same pyenv setup, causing:
   - Variables to be set twice
   - Potential conflicts between settings
   - Slower shell startup

2. **Inefficient PATH Management**: PATH is being modified multiple times across different files.

3. **Shell Loading Order Confusion**: 
   - `.zprofile` is loaded once per login session
   - `.zshrc` is loaded for every new shell session

## Recommended Organization

### .zprofile (Login Shell Configuration)
- Environment variables that should be set once per login session
- PATH modifications that should happen once
- Program setups that don't need to be repeated (like pyenv)

### .zshrc (Interactive Shell Configuration)
- Aliases and functions
- Shell options and customization
- Environment variables needed in every shell session
- PATH additions for interactive use

## Implementation Steps

1. Backup your current files:
   ```bash
   cp ~/.zprofile ~/.zprofile.backup
   cp ~/.zshrc ~/.zshrc.backup
   ```

2. Replace the contents of `~/.zprofile` with:
   ```bash
# Setting PATH for Python 3.13
# The original version is saved in .zprofile.pysave
export PATH="/Library/Frameworks/Python.framework/Versions/3.13/bin:${PATH}"

# Homebrew setup
eval "$(/opt/homebrew/bin/brew shellenv)"

# Pyenv setup - only in .zprofile (login shell)
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - zsh)" >/dev/null 2>&1
   ```

3. Replace the contents of `~/.zshrc` with:
   ```bash
# Pyenv setup - only initialization (moved to .zprofile for efficiency)
# Note: Actual PYENV_ROOT and PATH setup moved to .zprofile

# SWI Prolog path
export PATH="$PATH:/Applications/SWI-Prolog.app/Contents/MacOS"

# Pip user bin directory
export PATH="/Users/jrc/.local/bin:$PATH"

# SSL certificate handling
# requires pip install --upgrade certifi
export SSL_CERT_FILE=$(python -m certifi)
   ```

## Benefits of This Approach

1. **Faster Shell Startup**: Eliminates redundant configuration loading
2. **Clearer Separation**: Login vs. interactive shell settings are properly separated
3. **Reduced Conflicts**: No duplicate environment variable settings
4. **Better Maintainability**: Easier to troubleshoot and modify

## Testing the Changes

After making these changes:
1. Close all terminal windows
2. Open a new terminal
3. Test that all your tools still work:
   ```bash
   which python
   which pyenv
   echo $SSL_CERT_FILE
   ```

If you encounter any issues, you can restore your backup files:
```bash
cp ~/.zprofile.backup ~/.zprofile
cp ~/.zshrc.backup ~/.zshrc
```