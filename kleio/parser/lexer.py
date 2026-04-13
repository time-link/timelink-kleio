"""Lexical tokenizer for Kleio notation files.

This module provides tokenization for Kleio source data files (.cli)
and structure/command files (.str).

Ported from lexical.pl in the original Prolog implementation.

Key concepts:
- Character classification: maps ASCII chars to type names
- Data flags: special characters used in Kleio data files
- Two tokenization modes: data mode and command mode
"""
from __future__ import annotations

from dataclasses import dataclass
from typing import Iterator


@dataclass
class Token:
    """Represents a lexical token.
    
    Attributes:
        type: Token type name (e.g., 'names', 'number', 'fill', 'dquote', 
              'tquote', 'dataflag', or a chartype name)
        value: The string value or dataflag number for dataflag tokens
    """
    type: str
    value: str | int


# Character type mappings (from lexical.pl lines 242-276)
# Maps ASCII codes to character type names
_CHARTYPE_MAP: dict[int, str] = {
    32: 'space',
    9: 'tab',
    33: 'exclamation',
    34: 'doblequote',  # Note: kept original spelling from Prolog
    35: 'cardinal',
    36: 'dollar',
    37: 'percent',
    38: 'and',
    39: 'singlequote',
    40: 'openround',
    41: 'closeround',
    42: 'asterix',
    43: 'plus',
    44: 'comma',
    45: 'minus',
    46: 'point',
    47: 'slash',
    58: 'colon',
    59: 'semicolon',
    60: 'less',
    61: 'equal',
    62: 'greater',
    63: 'question',
    64: 'at',
    91: 'squareopen',
    92: 'backslash',
    93: 'squareclose',
    94: 'circunflex',
    95: 'underscore',
    124: 'pipe',
}


def chartype(char: str) -> str:
    """Get the character type for a single character.
    
    This mirrors the Prolog chartype/2 predicate (lines 242-276).
    
    Args:
        char: A single character
        
    Returns:
        The character type name (e.g., 'lower', 'upper', 'digit', 'space', etc.)
    """
    if len(char) != 1:
        raise ValueError(f"chartype expects a single character, got: {char!r}")
    
    code = ord(char)
    
    # Check explicit mapping first
    if code in _CHARTYPE_MAP:
        return _CHARTYPE_MAP[code]
    
    # Check character classes using Python's str methods
    if char.islower():
        return 'lower'
    if char.isupper():
        return 'upper'
    if char.isdigit():
        return 'digit'
    # Handle newline characters (CR and LF)
    if char in '\r\n':
        return 'return'
    
    # Default: use the character itself as the type name
    # (matches Prolog behavior: chartype(Other,Other))
    return char


# Data flag definitions (from lexical.pl lines 308-327)
# Maps flag numbers to character type names
# Note: flag 8 is configurable (normally semicolon, can be pipe etc.)
_DATA_FLAGS: dict[int, str] = {
    1: 'dollar',
    2: 'slash',
    3: 'equal',
    4: 'cardinal',
    5: 'percent',
    6: 'less',
    7: 'greater',
    9: 'colon',
    10: 'backslash',
}


def data_flag(flag_num: int, entry_separator_type: str = 'semicolon') -> str | None:
    """Get the character type name for a data flag number.
    
    Args:
        flag_num: Data flag number (1-10)
        entry_separator_type: Character type for the entry separator (flag 8)
                             Default is 'semicolon'
        
    Returns:
        Character type name for the flag, or None if not a valid flag
    """
    if flag_num == 8:
        return entry_separator_type
    return _DATA_FLAGS.get(flag_num)


def data_flag_char(flag_num: int, entry_separator_type: str = 'semicolon') -> str | None:
    """Get the character for a data flag number.
    
    Args:
        flag_num: Data flag number (1-10)
        entry_separator_type: Character type for the entry separator (flag 8)
        
    Returns:
        The character for the flag, or None if not found
    """
    type_name = data_flag(flag_num, entry_separator_type)
    if type_name is None:
        return None
    
    # Reverse lookup: find the character with this type
    for code, t in _CHARTYPE_MAP.items():
        if t == type_name:
            return chr(code)
    
    # Handle special types like 'semicolon'
    if type_name == 'semicolon':
        return ';'
    
    return None


def _get_data_flag_for_type(type_name: str, entry_separator_type: str) -> int | None:
    """Get the data flag number for a character type.
    
    Args:
        type_name: Character type name
        entry_separator_type: The type name for the entry separator
        
    Returns:
        Data flag number if the type is a data flag, else None
    """
    for flag_num, flag_type in _DATA_FLAGS.items():
        if flag_type == type_name:
            return flag_num
    
    # Check entry separator (flag 8)
    if type_name == entry_separator_type:
        return 8
    
    return None


class _LexerState:
    """Internal state for the lexer."""
    
    def __init__(self, text: str, entry_separator: str = ';'):
        self.text = text
        self.pos = 0
        self.entry_separator = entry_separator
        self.entry_separator_type = chartype(entry_separator)
    
    def peek(self, offset: int = 0) -> str | None:
        """Peek at character at current position + offset."""
        pos = self.pos + offset
        if pos < len(self.text):
            return self.text[pos]
        return None
    
    def peek_type(self, offset: int = 0) -> str | None:
        """Get the type of character at current position + offset."""
        char = self.peek(offset)
        if char is not None:
            return chartype(char)
        return None
    
    def advance(self, count: int = 1) -> str:
        """Advance position and return consumed characters."""
        result = self.text[self.pos:self.pos + count]
        self.pos += count
        return result
    
    def at_end(self) -> bool:
        """Check if at end of input."""
        return self.pos >= len(self.text)


def _match_fill(state: _LexerState) -> Token | None:
    """Match a sequence of spaces and tabs.
    
    From lexical.pl lines 142-148: fillsp
    """
    if state.at_end():
        return None
    
    char_type = state.peek_type()
    if char_type not in ('space', 'tab'):
        return None
    
    start = state.pos
    while not state.at_end():
        char_type = state.peek_type()
        if char_type not in ('space', 'tab'):
            break
        state.advance()
    
    return Token('fill', state.text[start:state.pos])


def _match_name(state: _LexerState) -> Token | None:
    """Match a name: letter followed by letters/digits/point/minus/underscore.
    
    From lexical.pl lines 177-188: names
    """
    if state.at_end():
        return None
    
    # Names must start with a letter
    char_type = state.peek_type()
    if char_type not in ('upper', 'lower'):
        return None
    
    start = state.pos
    state.advance()  # consume the first letter
    
    # Continue with valid name characters
    valid_types = {'upper', 'lower', 'digit', 'point', 'minus', 'underscore'}
    while not state.at_end():
        char_type = state.peek_type()
        if char_type not in valid_types:
            break
        state.advance()
    
    return Token('names', state.text[start:state.pos])


def _match_number(state: _LexerState) -> Token | None:
    """Match a number: digits with optional decimal point.
    
    From lexical.pl lines 196-204: num
    
    Supports:
    - Integer: "123"
    - Decimal: "3.14"
    - Trailing decimal: "5."
    """
    if state.at_end():
        return None
    
    # Must start with a digit
    char_type = state.peek_type()
    if char_type != 'digit':
        return None
    
    start = state.pos
    
    # Match digits before decimal point
    while not state.at_end() and state.peek_type() == 'digit':
        state.advance()
    
    # Check for decimal point
    if not state.at_end() and state.peek_type() == 'point':
        # Check if there's a digit after the point
        if state.peek(1) is not None and chartype(state.peek(1)) == 'digit':
            # It's a decimal number like "3.14"
            state.advance()  # consume the point
            while not state.at_end() and state.peek_type() == 'digit':
                state.advance()
        else:
            # It's a trailing decimal like "5."
            state.advance()  # consume the point
    
    return Token('number', state.text[start:state.pos])


def _match_tquote(state: _LexerState) -> Token | None:
    """Match triple double-quote.
    
    From lexical.pl line 236: tquote
    """
    if state.at_end():
        return None
    
    # Check for three consecutive double quotes
    if (state.peek() == '"' and 
        state.peek(1) == '"' and 
        state.peek(2) == '"'):
        state.advance(3)
        return Token('tquote', '"""')
    
    return None


def _match_dquote(state: _LexerState) -> Token | None:
    """Match single double-quote.
    
    From lexical.pl line 215: dquote
    """
    if state.at_end():
        return None
    
    if state.peek() == '"':
        state.advance()
        return Token('dquote', '"')
    
    return None


def _match_quoted_string(state: _LexerState) -> Token | None:
    """Match a double-quoted string with escape handling (for command mode).
    
    From lexical.pl lines 219-230: dqstring
    Handles escaped quotes (\\") inside the string.
    """
    if state.at_end():
        return None
    
    if state.peek() != '"':
        return None
    
    start = state.pos
    state.advance()  # consume opening quote
    
    content = []
    while not state.at_end():
        char = state.peek()
        char_type = state.peek_type()
        
        # End of line without closing quote (error case)
        if char_type == 'return':
            # In Prolog, this outputs an error but continues
            # We just return what we have
            break
        
        # Check for backslash escape
        if char == '\\' and state.peek(1) is not None:
            state.advance()  # consume backslash
            if not state.at_end():
                escaped_char = state.advance()
                content.append(escaped_char)
            continue
        
        # Check for closing quote
        if char == '"':
            state.advance()  # consume closing quote
            # Return the string with quotes included (like Prolog qname)
            return Token('string', '"' + ''.join(content) + '"')
        
        # Regular character
        content.append(state.advance())
    
    # Unclosed quote - return what we have
    return Token('string', '"' + ''.join(content))


def tokenize_data(text: str, entry_separator: str = ';') -> list[Token]:
    """Tokenize Kleio source data (data mode).
    
    This matches the behavior of get_tokens(dat, ...) from lexical.pl.
    
    Token types produced:
    - tquote: triple double-quotes (three double-quote chars)
    - dquote: single double-quote
    - names: identifier (letter followed by letters/digits/./-/_) 
    - fill: sequence of spaces/tabs
    - number: digits with optional decimal point
    - dataflag: special Kleio characters ($ / = # % < > separator :)
    - Other: single character with its chartype name
    
    Args:
        text: The input text to tokenize
        entry_separator: Character used as entry separator (flag 8)
                        Default is ';' (semicolon)
    
    Returns:
        List of Token objects
    """
    state = _LexerState(text, entry_separator)
    tokens: list[Token] = []
    
    while not state.at_end():
        # Try to match tokens in priority order (same as Prolog)
        
        # 1. Triple quote (must check before single quote)
        token = _match_tquote(state)
        if token:
            tokens.append(token)
            continue
        
        # 2. Single double quote
        token = _match_dquote(state)
        if token:
            tokens.append(token)
            continue
        
        # 3. Names (identifier)
        token = _match_name(state)
        if token:
            tokens.append(token)
            continue
        
        # 4. Fill (spaces/tabs)
        token = _match_fill(state)
        if token:
            tokens.append(token)
            continue
        
        # 5. Numbers
        token = _match_number(state)
        if token:
            tokens.append(token)
            continue
        
        # 6. Data flags
        char_type = state.peek_type()
        flag_num = _get_data_flag_for_type(char_type, state.entry_separator_type)
        if flag_num is not None:
            state.advance()
            tokens.append(Token('dataflag', flag_num))
            continue
        
        # 7. Other characters
        char = state.advance()
        tokens.append(Token(char_type, char))
    
    return tokens


def tokenize_cmd(text: str) -> list[Token]:
    """Tokenize Kleio commands/structure definitions (command mode).
    
    This matches the behavior of get_tokens(cmd, ...) from lexical.pl.
    
    Token types produced:
    - fill: sequence of spaces/tabs
    - name: identifier (same as 'names' in data mode)
    - number: digits with optional decimal point
    - string: double-quoted string with escape handling
    - dataflag(2): slash separator only
    - Other: single character with its chartype name
    
    Args:
        text: The input text to tokenize
        
    Returns:
        List of Token objects
    """
    # Command mode uses semicolon as default separator but only slash is a dataflag
    state = _LexerState(text, ';')
    tokens: list[Token] = []
    
    while not state.at_end():
        # Try to match tokens in priority order
        
        # 1. Fill (spaces/tabs) - first in command mode
        token = _match_fill(state)
        if token:
            tokens.append(token)
            continue
        
        # 2. Names (called 'name' in command mode)
        token = _match_name(state)
        if token:
            # In command mode, token type is 'name' not 'names'
            tokens.append(Token('name', token.value))
            continue
        
        # 3. Numbers
        token = _match_number(state)
        if token:
            tokens.append(token)
            continue
        
        # 4. Quoted strings
        token = _match_quoted_string(state)
        if token:
            tokens.append(token)
            continue
        
        # 5. Data flag 2 (slash only)
        char_type = state.peek_type()
        if char_type == 'slash':
            state.advance()
            tokens.append(Token('dataflag', 2))
            continue
        
        # 6. Other characters (excluding upper, lower, digit, space, tab)
        char = state.peek()
        char_type = state.peek_type()
        
        # Skip if it would have been caught by other matchers
        if char_type in ('upper', 'lower', 'digit', 'space', 'tab'):
            # This shouldn't happen, but handle gracefully
            state.advance()
            tokens.append(Token(char_type, char))
            continue
        
        state.advance()
        tokens.append(Token(char_type, char))
    
    return tokens


# Convenience function matching Prolog's get_tokens/3 interface
def get_tokens(mode: str, text: str, entry_separator: str = ';') -> list[Token]:
    """Tokenize text according to the specified mode.
    
    This mirrors the Prolog get_tokens/3 predicate.
    
    Args:
        mode: 'dat' for data mode, 'cmd' for command mode
        text: The input text to tokenize
        entry_separator: Character used as entry separator (data mode only)
        
    Returns:
        List of Token objects
        
    Raises:
        ValueError: If mode is not 'dat' or 'cmd'
    """
    if mode == 'dat':
        return tokenize_data(text, entry_separator)
    elif mode == 'cmd':
        return tokenize_cmd(text)
    else:
        raise ValueError(f"Unknown tokenization mode: {mode}. Use 'dat' or 'cmd'.")
