"""
A SAX-like parser for the Kleio notation.

This module provides a parser for Kleio, a notation used for historical data input.
The parser works in a SAX-like manner, emitting events as it processes the input
document. Users should implement a handler class that inherits from
`KleioParserHandler` to process these events.

The main components are:
- `KleioParser`: The main parser class that takes a handler and processes Kleio text.
- `KleioParserHandler`: An abstract base class defining the interface for event handlers.
"""


class KleioParserHandler:
    """
    Handler interface for Kleio parser events (SAX-like)
    """
    def start_group(self, name: str, level: int):
        """Called when a group starts"""
        pass

    def end_group(self, name: str, level: int):
        """Called when a group ends"""
        pass

    def start_element(self, name: str):
        """Called when an element starts"""
        pass

    def end_element(self, name: str):
        """Called when an element ends"""
        pass

    def start_element_aspect(self, aspect: str):
        """Called when an element aspect starts (core, original, comment)"""
        pass

    def end_element_aspect(self, aspect: str):
        """Called when an element aspect ends"""
        pass


class KleioParser:
    """
    Parser for Kleio notation following the EBNF grammar
    """

    # Default token definitions
    TOKENS = {
        'GROUP_MARKER': '$',
        'ELEMENT_ASSIGNMENT': '=',
        'ELEMENT_SEPARATOR': '/',
        'ORIGINAL_MARKER': '%',
        'COMMENT_MARKER': '#',
        'MULTIPLE_VALUE_SEPARATOR': '|',
        'STRING_DELIMITER': '"',
        'MULTILINE_STRING_DELIMITER': '"""'
    }

    def __init__(self, handler: KleioParserHandler):
        self.handler = handler
        self.tokens = self.TOKENS.copy()

    def configure_tokens(self, token_config: dict):
        """Configure special characters used in parsing"""
        self.tokens.update(token_config)

    def parse(self, text: str):
        """Parse the entire Kleio document"""
        lines = text.splitlines()
        for line in lines:
            if line.strip():
                self._parse_line(line)

    def _get_indent_level(self, line: str) -> int:
        """Calculate the indentation level of a line"""
        indent = 0
        for char in line:
            if char == ' ':
                indent += 1
            elif char == '\t':
                indent += 4  # Assuming tab = 4 spaces
            else:
                break
        return indent

    def _parse_line(self, line: str):
        """Parse a single line"""
        stripped_line = line.strip()
        if not stripped_line:
            return

        indent_level = self._get_indent_level(line)

        # Check if this is a group declaration
        if self.tokens['GROUP_MARKER'] in stripped_line:
            self._parse_group(stripped_line, indent_level)
        elif self.tokens['ELEMENT_SEPARATOR'] in stripped_line and self.tokens['ELEMENT_ASSIGNMENT'] in stripped_line:
            # This is an element on its own line
            self._parse_standalone_element(stripped_line)

    def _parse_group(self, line: str, level: int):
        """Parse a group declaration"""
        # Split by GROUP_MARKER to get group name
        parts = line.split(self.tokens['GROUP_MARKER'], 1)
        group_name_part = parts[0]

        # Notify start of group
        self.handler.start_group(group_name_part, level)

        # Process elements if any
        if len(parts) > 1 and parts[1]:
            elements_part = parts[1]
            self._parse_elements(elements_part)

        # Notify end of group
        self.handler.end_group(group_name_part, level)

    def _parse_standalone_element(self, line: str):
        """Parse an element that appears on its own line"""
        # Remove leading separator if present
        if line.startswith(self.tokens['ELEMENT_SEPARATOR']):
            line = line[1:]

        self._parse_elements(line)

    def _parse_elements(self, elements_str: str):
        """Parse elements from a string"""
        # Split by ELEMENT_SEPARATOR
        element_parts = elements_str.split(self.tokens['ELEMENT_SEPARATOR'])

        for element_part in element_parts:
            if not element_part.strip():
                continue

            if self.tokens['ELEMENT_ASSIGNMENT'] in element_part:
                self._parse_named_element(element_part)
            else:
                # This is a positional element - treat as value only
                self._parse_positional_element(element_part)

    def _parse_named_element(self, element_str: str):
        """Parse a named element with name=value format"""
        if self.tokens['ELEMENT_ASSIGNMENT'] not in element_str:
            # Handle case where assignment is missing, treat as positional
            self._parse_positional_element(element_str)
            return
        name, value_str = element_str.split(self.tokens['ELEMENT_ASSIGNMENT'], 1)
        name = name.strip()

        self.handler.start_element(name)
        self._parse_element_value(value_str)
        self.handler.end_element(name)

    def _parse_positional_element(self, value_str: str):
        """Parse a positional element (value only)"""
        # Treat positional elements as having a special name
        self.handler.start_element("_positional")
        self._parse_element_value(value_str)
        self.handler.end_element("_positional")

    def _parse_element_value(self, value_str: str):
        """Parse an element value with possible aspects"""
        # Check for aspects
        has_original = self.tokens['ORIGINAL_MARKER'] in value_str
        has_comment = self.tokens['COMMENT_MARKER'] in value_str

        if has_original and has_comment:
            # Has all three aspects: core%original#comment
            if self.tokens['ORIGINAL_MARKER'] not in value_str or self.tokens['COMMENT_MARKER'] not in value_str:
                # Fallback if markers are malformed
                self.handler.start_element_aspect("core")
                self._emit_text(value_str)
                self.handler.end_element_aspect("core")
                return
            core_part, rest = value_str.split(self.tokens['ORIGINAL_MARKER'], 1)
            original_part, comment_part = rest.split(self.tokens['COMMENT_MARKER'], 1)

            self.handler.start_element_aspect("core")
            self._emit_text(core_part)
            self.handler.end_element_aspect("core")

            self.handler.start_element_aspect("original")
            self._emit_text(original_part)
            self.handler.end_element_aspect("original")

            self.handler.start_element_aspect("comment")
            self._emit_text(comment_part)
            self.handler.end_element_aspect("comment")

        elif has_original:
            # Has core and original: core%original
            if self.tokens['ORIGINAL_MARKER'] not in value_str:
                self.handler.start_element_aspect("core")
                self._emit_text(value_str)
                self.handler.end_element_aspect("core")
                return
            core_part, original_part = value_str.split(self.tokens['ORIGINAL_MARKER'], 1)

            self.handler.start_element_aspect("core")
            self._emit_text(core_part)
            self.handler.end_element_aspect("core")

            self.handler.start_element_aspect("original")
            self._emit_text(original_part)
            self.handler.end_element_aspect("original")

        elif has_comment:
            # Has core and comment: core#comment
            if self.tokens['COMMENT_MARKER'] not in value_str:
                self.handler.start_element_aspect("core")
                self._emit_text(value_str)
                self.handler.end_element_aspect("core")
                return
            core_part, comment_part = value_str.split(self.tokens['COMMENT_MARKER'], 1)

            self.handler.start_element_aspect("core")
            self._emit_text(core_part)
            self.handler.end_element_aspect("core")

            self.handler.start_element_aspect("comment")
            self._emit_text(comment_part)
            self.handler.end_element_aspect("comment")

        else:
            # Only core aspect
            self.handler.start_element_aspect("core")
            self._emit_text(value_str)
            self.handler.end_element_aspect("core")

    def _emit_text(self, text: str):
        """Emit text content - in a real implementation, this would process the actual text"""
        # For now, we just pass through - in a real implementation you might
        # want to handle string delimiters, normalize whitespace, etc.
        pass


# Example implementation of the handler
class ExampleKleioHandler(KleioParserHandler):
    """Example handler that prints parsing events"""

    def start_group(self, name: str, level: int):
        indent = "  " * level
        print(f"{indent}Start Group: {name}")

    def end_group(self, name: str, level: int):
        indent = "  " * level
        print(f"{indent}End Group: {name}")

    def start_element(self, name: str):
        print(f"  Start Element: {name}")

    def end_element(self, name: str):
        print(f"  End Element: {name}")

    def start_element_aspect(self, aspect: str):
        print(f"    Start Aspect: {aspect}")

    def end_element_aspect(self, aspect: str):
        print(f"    End Aspect: {aspect}")


# Usage example:
if __name__ == "__main__":
    # Example usage
    handler = ExampleKleioHandler()
    parser = KleioParser(handler)

    sample_kleio = '''
        b$b1788.1118/22/1/1788/vigario luis barreto de figueiredo castilho e o reverendo cosme dias ribeiro de coimbra

            n$bernardo/m/obs=diz a margem casais, fl.10v/id=b1788.1118-per1
               ls$datanasc/17880111
    '''

    parser.parse(sample_kleio)