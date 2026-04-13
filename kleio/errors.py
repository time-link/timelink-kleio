"""Error accumulation for Kleio translation.

Collects errors and warnings during parsing/translation without aborting,
allowing complete validation of an entire file before reporting.
Mirrors the error handling pattern from the Prolog errors.pl module.
"""
from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional


class Severity(Enum):
    """Error severity levels."""
    WARNING = "warning"
    ERROR = "error"


@dataclass
class KleioError:
    """A single error or warning encountered during processing."""
    message: str
    severity: Severity = Severity.ERROR
    file: str = ""
    line_number: int = 0
    line_text: str = ""
    last_line_text: str = ""

    def __str__(self) -> str:
        """Format the error as a human-readable string."""
        parts = []
        if self.severity == Severity.WARNING:
            parts.append("Warning")
        else:
            parts.append("Error")
        if self.file:
            parts.append(f"in {self.file}")
        if self.line_number:
            parts.append(f"at line {self.line_number}")
        parts.append(f": {self.message}")
        if self.line_text:
            parts.append(f"\n  Line: {self.line_text}")
        return " ".join(parts)


class ErrorAccumulator:
    """Collects errors and warnings during translation.
    
    Allows processing to continue after errors up to a configurable maximum,
    enabling complete file validation in a single pass.
    """

    def __init__(self, max_errors: int = 100):
        """Initialize the error accumulator.
        
        Args:
            max_errors: Maximum number of errors before raising MaxErrorsExceeded.
        """
        self.max_errors = max_errors
        self._errors: list[KleioError] = []
        self._warnings: list[KleioError] = []
        self._context_file: str = ""
        self._context_line: int = 0
        self._context_text: str = ""

    def set_context(self, file: str = "", line_number: int = 0, line_text: str = "") -> None:
        """Set the current file/line context for subsequent errors.
        
        Args:
            file: Current file being processed.
            line_number: Current line number.
            line_text: Current line text content.
        """
        if file:
            self._context_file = file
        if line_number:
            self._context_line = line_number
        if line_text:
            self._context_text = line_text

    def error(self, message: str, *, file: str = "", line_number: int = 0,
              line_text: str = "", last_line_text: str = "") -> None:
        """Record an error.
        
        Args:
            message: Error message.
            file: File where error occurred (uses context if not provided).
            line_number: Line number (uses context if not provided).
            line_text: Line text (uses context if not provided).
            last_line_text: Previous line text for context.
        
        Raises:
            MaxErrorsExceeded: When max_errors limit is reached.
        """
        err = KleioError(
            message=message,
            severity=Severity.ERROR,
            file=file or self._context_file,
            line_number=line_number or self._context_line,
            line_text=line_text or self._context_text,
            last_line_text=last_line_text,
        )
        self._errors.append(err)
        if len(self._errors) >= self.max_errors:
            raise MaxErrorsExceeded(
                f"Maximum number of errors ({self.max_errors}) exceeded"
            )

    def warning(self, message: str, *, file: str = "", line_number: int = 0,
                line_text: str = "") -> None:
        """Record a warning.
        
        Args:
            message: Warning message.
            file: File where warning occurred (uses context if not provided).
            line_number: Line number (uses context if not provided).
            line_text: Line text (uses context if not provided).
        """
        warn = KleioError(
            message=message,
            severity=Severity.WARNING,
            file=file or self._context_file,
            line_number=line_number or self._context_line,
            line_text=line_text or self._context_text,
        )
        self._warnings.append(warn)

    @property
    def errors(self) -> list[KleioError]:
        """Get a copy of the error list."""
        return list(self._errors)

    @property
    def warnings(self) -> list[KleioError]:
        """Get a copy of the warning list."""
        return list(self._warnings)

    @property
    def error_count(self) -> int:
        """Get the number of errors."""
        return len(self._errors)

    @property
    def warning_count(self) -> int:
        """Get the number of warnings."""
        return len(self._warnings)

    def has_errors(self) -> bool:
        """Check if any errors have been recorded."""
        return len(self._errors) > 0

    def clear(self) -> None:
        """Clear all errors and warnings."""
        self._errors.clear()
        self._warnings.clear()

    def get_report(self) -> str:
        """Generate a text report of all errors and warnings."""
        lines = []
        for err in self._errors:
            lines.append(str(err))
        for warn in self._warnings:
            lines.append(str(warn))
        if not lines:
            return "No errors or warnings."
        lines.append(f"\nTotal: {self.error_count} error(s), {self.warning_count} warning(s)")
        return "\n".join(lines)


class MaxErrorsExceeded(Exception):
    """Raised when the maximum number of errors is exceeded."""
    pass
