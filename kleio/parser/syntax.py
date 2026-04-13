"""Syntax parser for Kleio data files.

This module parses tokenized Kleio data lines into Action objects.
Ported from dataSyntax.pl in the original Prolog implementation.

The parser processes lines of tokens and produces Action objects that
represent the semantic operations to be performed by the group builder.
"""
from __future__ import annotations

from dataclasses import dataclass

from kleio.parser.lexer import Token, data_flag_char
from kleio.parser.models import (
    NewGroup, NewElement, EndElement, NewEntry, NewAspect, StoreCore, Aspect
)


@dataclass
class QuoteState:
    """Tracks multi-line quote state across lines.
    
    Attributes:
        triple_quote_on: True when inside triple-quote block
        double_quote_on: True when inside double-quote block
    """
    triple_quote_on: bool = False
    double_quote_on: bool = False


def parse_line(tokens: list[Token], state: QuoteState) -> list:
    """Parse a line of tokens into a list of Action objects.
    
    This implements the grammar from dataSyntax.pl:
    
        a_line -> group elements
        a_line -> elements
        
        group -> fill? names dataflag(1)
        
        elements -> element*
    
    Args:
        tokens: List of Token objects from the lexer
        state: QuoteState tracking multi-line quotes (modified in place)
    
    Returns:
        List of action objects (NewGroup, NewElement, EndElement, 
        NewEntry, NewAspect, StoreCore)
    """
    actions: list = []
    i = 0
    n = len(tokens)
    
    # Helper to peek at token at index
    def peek(offset: int = 0) -> Token | None:
        idx = i + offset
        if idx < n:
            return tokens[idx]
        return None
    
    # Check for group at start: optional fill, then names, then dataflag(1)
    # group -> fill? names dataflag(1)
    if n > 0:
        # Skip optional fill
        start_idx = i
        if tokens[i].type == 'fill':
            i += 1
        
        # Check for names followed by dataflag(1)
        if i < n and tokens[i].type == 'names':
            group_name = tokens[i].value
            if i + 1 < n and tokens[i + 1].type == 'dataflag' and tokens[i + 1].value == 1:
                # This is a group line
                actions.append(NewGroup(group_name))
                i += 2  # consume names and dataflag(1)
    
    # If we didn't find a group, reset to start
    if not actions or not isinstance(actions[0], NewGroup):
        i = 0
        actions = []
    
    # Process remaining tokens as elements
    while i < n:
        token = tokens[i]
        
        # Triple quote handling (highest priority)
        if token.type == 'tquote':
            if not state.triple_quote_on:
                # Entering triple-quote mode
                actions.append(StoreCore(token.value))
                state.triple_quote_on = True
            else:
                # Exiting triple-quote mode
                actions.append(StoreCore(token.value))
                state.triple_quote_on = False
            i += 1
            continue
        
        # When inside triple-quote mode: EVERYTHING is StoreCore
        if state.triple_quote_on:
            if token.type == 'dataflag':
                # Store the character representation of the dataflag
                char = data_flag_char(token.value)
                if char:
                    actions.append(StoreCore(char))
            elif token.type == 'return':
                # Store return character
                actions.append(StoreCore(token.value))
            elif token.type in ('fill', 'names', 'number', 'dquote'):
                actions.append(StoreCore(token.value))
            else:
                # Any other token type - store its value as a character
                actions.append(StoreCore(token.value))
            i += 1
            continue
        
        # Double quote handling
        if token.type == 'dquote':
            if not state.double_quote_on:
                # Entering double-quote mode
                actions.append(StoreCore(token.value))
                state.double_quote_on = True
            else:
                # Exiting double-quote mode
                actions.append(StoreCore(token.value))
                state.double_quote_on = False
            i += 1
            continue
        
        # When inside double-quote mode
        if state.double_quote_on:
            if token.type == 'dataflag':
                # Store the character representation of the dataflag
                char = data_flag_char(token.value)
                if char:
                    actions.append(StoreCore(char))
            elif token.type == 'return':
                # Skip returns within double quotes
                pass
            elif token.type == 'fill':
                # Fill becomes single space in double quotes
                actions.append(StoreCore(' '))
            elif token.type in ('names', 'number'):
                actions.append(StoreCore(token.value))
            else:
                # Any other token type - store its value as a character
                actions.append(StoreCore(token.value))
            i += 1
            continue
        
        # Normal mode (not in quotes)
        if token.type == 'fill':
            # Every fill sequence is stored as a single space
            actions.append(StoreCore(' '))
            i += 1
            continue
        
        if token.type == 'names':
            element_name = token.value
            # Check for element= pattern (lookahead for dataflag(3))
            next_token = peek(1)
            if next_token and next_token.type == 'dataflag' and next_token.value == 3:
                # This is an element assignment: element=value
                actions.append(NewElement(element_name))
                i += 2  # consume names and dataflag(3)
                continue
            else:
                # Just a name to store
                actions.append(StoreCore(element_name))
                i += 1
                continue
        
        if token.type == 'dataflag':
            flag_val = token.value
            
            if flag_val == 2:
                # Slash - end element
                actions.append(EndElement())
                i += 1
                continue
            elif flag_val == 8:
                # Semicolon - new entry
                actions.append(NewEntry())
                i += 1
                continue
            elif flag_val == 5:
                # Percent - original aspect
                actions.append(NewAspect(Aspect.ORIGINAL))
                i += 1
                # Check if next token is slash - if so, consume it (it's part of aspect syntax)
                if i < n and tokens[i].type == 'dataflag' and tokens[i].value == 2:
                    i += 1
                continue
            elif flag_val == 4:
                # Cardinal (#) - comment aspect
                actions.append(NewAspect(Aspect.COMMENT))
                i += 1
                # Check if next token is slash - if so, consume it (it's part of aspect syntax)
                if i < n and tokens[i].type == 'dataflag' and tokens[i].value == 2:
                    i += 1
                continue
            elif flag_val == 10:
                # Backslash escape - consume next token as literal
                if i + 1 < n:
                    next_tok = tokens[i + 1]
                    if next_tok.type == 'dataflag':
                        # Store the character for the escaped dataflag
                        char = data_flag_char(next_tok.value)
                        if char:
                            actions.append(StoreCore(char))
                    else:
                        # Store the character value
                        if isinstance(next_tok.value, str) and len(next_tok.value) == 1:
                            actions.append(StoreCore(next_tok.value))
                        else:
                            # For multi-char values, store the first char
                            actions.append(StoreCore(str(next_tok.value)[0]))
                    i += 2
                    continue
                else:
                    # Backslash at end of line - store it literally
                    char = data_flag_char(10)
                    if char:
                        actions.append(StoreCore(char))
                    i += 1
                    continue
            else:
                # Other dataflags - store as literal character
                char = data_flag_char(flag_val)
                if char:
                    actions.append(StoreCore(char))
                i += 1
                continue
        
        if token.type == 'return':
            # Returns are skipped in normal mode
            i += 1
            continue
        
        if token.type == 'number':
            actions.append(StoreCore(token.value))
            i += 1
            continue
        
        # Default: store the value as a string
        actions.append(StoreCore(token.value))
        i += 1
    
    return actions
