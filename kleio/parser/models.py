"""Data models for the Kleio parser.

This module defines the core data structures used during parsing of
Kleio notation files. These models represent the parser's internal
state and output.

The Kleio notation has three aspects for element values:
- core: the primary normalized value
- original: the original/transcribed value (prefixed with %)
- comment: editorial comments (prefixed with #)

Elements can have multiple entries separated by ';' (or configurable separator).
"""
from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Optional


class Aspect(Enum):
    """The three aspects of a Kleio element value.
    
    In Kleio notation, element values can have three "aspects":
    - CORE: The primary, normalized value (the default)
    - ORIGINAL: The original transcription (marked with %)
    - COMMENT: Editorial notes/comments (marked with #)
    
    Example in Kleio notation:
        name=John%/Johann/#written as Johann in original
    """
    CORE = "core"
    ORIGINAL = "original"
    COMMENT = "comment"


@dataclass
class Entry:
    """A single entry (value) within an element aspect.
    
    Elements can have multiple entries separated by ';' (or configurable separator).
    Each entry is a list of string fragments that are concatenated.
    
    Example:
        The value "John;Paul" creates two entries:
        - Entry(values=["John"])
        - Entry(values=["Paul"])
    """
    values: list[str] = field(default_factory=list)

    @property
    def text(self) -> str:
        """Concatenated text of all value fragments."""
        return "".join(self.values)

    def __str__(self) -> str:
        return self.text


@dataclass
class ParsedElement:
    """A parsed element from a Kleio data file.
    
    Each element has a name and three lists of entries (core, original, comment).
    
    Example Kleio notation:
        name=John%/Johann/#written as Johann
        
    This creates a ParsedElement with:
    - name: "name"
    - core_entries: [Entry(values=["John"])]
    - original_entries: [Entry(values=["Johann"])]
    - comment_entries: [Entry(values=["written as Johann"])]
    """
    name: str
    core_entries: list[Entry] = field(default_factory=list)
    original_entries: list[Entry] = field(default_factory=list)
    comment_entries: list[Entry] = field(default_factory=list)

    def get_core_text(self) -> str:
        """Get concatenated core text from all entries."""
        return ";".join(e.text for e in self.core_entries if e.text)

    def get_original_text(self) -> str:
        """Get concatenated original text from all entries."""
        return ";".join(e.text for e in self.original_entries if e.text)

    def get_comment_text(self) -> str:
        """Get concatenated comment text from all entries."""
        return ";".join(e.text for e in self.comment_entries if e.text)


@dataclass
class ParsedGroup:
    """A parsed group from a Kleio data file.
    
    Groups are the main data containers in Kleio notation (e.g., acts, persons).
    They contain elements and can be nested hierarchically.
    
    Example Kleio notation:
        act$a1/01/01/1700/date=mydate/typ=test
        
    This creates a ParsedGroup with:
    - name: "act"
    - id: "a1"
    - elements: positional elements and named elements
    - path: ancestry chain for nested groups
    """
    name: str
    id: str = ""
    elements: list[ParsedElement] = field(default_factory=list)
    children: list[ParsedGroup] = field(default_factory=list)
    path: list[tuple[str, str]] = field(default_factory=list)  # [(group_name, group_id), ...]
    line_number: int = 0
    line_text: str = ""
    level: int = 0  # indentation level in source

    def get_element(self, name: str) -> Optional[ParsedElement]:
        """Find an element by name."""
        for el in self.elements:
            if el.name == name:
                return el
        return None

    def get_element_value(self, name: str) -> str:
        """Get the core text of an element by name."""
        el = self.get_element(name)
        return el.get_core_text() if el else ""


# Action types produced by the syntax parser
# These represent the parser's internal actions during token processing

@dataclass
class NewGroup:
    """Action: a new group was detected.
    
    Emitted when the parser encounters a group name at the start
    of a group definition line.
    """
    name: str


@dataclass
class NewElement:
    """Action: an explicit element assignment was detected (element=value).
    
    Emitted when the parser encounters an element name followed by '='.
    """
    name: str


@dataclass
class EndElement:
    """Action: end of element detected (/ separator).
    
    Emitted when the parser encounters a '/' separator, indicating
    the end of the current element value.
    """
    pass


@dataclass
class NewEntry:
    """Action: new entry separator detected (;).
    
    Emitted when the parser encounters a ';' separator, indicating
    a new entry within the current element aspect.
    """
    pass


@dataclass
class NewAspect:
    """Action: aspect switch detected (% for original, # for comment).
    
    Emitted when the parser encounters aspect markers:
    - '%' switches to ORIGINAL aspect
    - '#' switches to COMMENT aspect
    """
    aspect: Aspect


@dataclass
class StoreCore:
    """Action: store a value fragment in the current aspect.
    
    Emitted for each text fragment that should be stored in the
    current element's current aspect.
    """
    value: str
