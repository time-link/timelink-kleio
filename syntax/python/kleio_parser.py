# pylint: disable=W0107  # Warning: Unnecessary pass statement
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

    def characters(self, data: str):
        """Called with character data for the current aspect"""
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
        self.pos = 0
        self.text = ""
        self.current_line = 1
        self.current_col = 1

    def configure_tokens(self, token_config: dict):
        """Configure special characters used in parsing"""
        self.tokens.update(token_config)

    def parse(self, text: str):
        """Parse the entire Kleio document"""
        self.text = text
        self.pos = 0
        self.current_line = 1
        self.current_col = 1

        while self.pos < len(self.text):
            self._skip_whitespace_and_newlines()
            if self.pos >= len(self.text):
                break

            indent_level = self._get_current_indent_level()
            self._parse_line_content(indent_level)

    def _peek(self, length: int = 1) -> str:
        """Peek ahead at the next character(s) without consuming"""
        return self.text[self.pos:self.pos + length]

    def _consume(self, length: int = 1) -> str:
        """Consume and return the next character(s)"""
        result = self.text[self.pos:self.pos + length]
        for char in result:
            if char == '\n':
                self.current_line += 1
                self.current_col = 1
            else:
                self.current_col += 1
        self.pos += length
        return result

    def _skip_whitespace(self):
        """Skip whitespace but not newlines"""
        while self.pos < len(self.text) and self.text[self.pos] in ' \t':
            self._consume()

    def _skip_whitespace_and_newlines(self):
        """Skip whitespace including newlines"""
        while self.pos < len(self.text) and self.text[self.pos] in ' \t\n\r':
            self._consume()

    def _get_current_indent_level(self) -> int:
        """Calculate the indentation level at current position"""
        indent = 0
        temp_pos = self.pos
        while temp_pos < len(self.text) and self.text[temp_pos] in ' \t':
            if self.text[temp_pos] == ' ':
                indent += 1
            elif self.text[temp_pos] == '\t':
                indent += 4  # Assuming tab = 4 spaces
            temp_pos += 1
        return indent

    def _parse_line_content(self, indent_level: int):
        """Parse content at the current position"""
        # Skip leading whitespace on this line
        self._skip_whitespace()

        if self.pos >= len(self.text):
            return

        # Read until end of line to determine line type
        line_start = self.pos
        line_content = self._read_until_newline_or_eof()

        # Reset position to parse properly
        self.pos = line_start

        # Check if this is a group declaration
        if self.tokens['GROUP_MARKER'] in line_content:
            self._parse_group(indent_level)
        elif self.tokens['ELEMENT_SEPARATOR'] in line_content:
            # This is an element on its own line
            self._parse_standalone_element()
        else:
            # Skip unknown content
            self._skip_to_newline()

    def _read_until_newline_or_eof(self) -> str:
        """Read from current position until newline or EOF without consuming"""
        temp_pos = self.pos
        result = []
        while temp_pos < len(self.text) and self.text[temp_pos] not in '\n\r':
            result.append(self.text[temp_pos])
            temp_pos += 1
        return ''.join(result)

    def _skip_to_newline(self):
        """Skip to the next newline"""
        while self.pos < len(self.text) and self.text[self.pos] not in '\n\r':
            self._consume()

    def _parse_group(self, level: int):
        """Parse a group declaration"""
        # Read group name until GROUP_MARKER
        group_name = self._read_identifier()

        # Notify start of group
        self.handler.start_group(group_name, level)

        # Check for GROUP_MARKER
        if self._peek() == self.tokens['GROUP_MARKER']:
            self._consume()  # consume the $

            # Parse elements on the same line if any
            if self.pos < len(self.text) and self._peek() not in '\n\r':
                self._parse_elements_until_newline()

        # Skip to next line
        self._skip_to_newline()

        # Notify end of group
        self.handler.end_group(group_name, level)

    def _parse_standalone_element(self):
        """Parse an element that appears on its own line"""
        # Skip leading separator if present
        if self._peek() == self.tokens['ELEMENT_SEPARATOR']:
            self._consume()

        self._parse_elements_until_newline()

    def _parse_elements_until_newline(self):
        """Parse elements until end of line"""
        while self.pos < len(self.text) and self._peek() not in '\n\r':
            self._skip_whitespace()

            if self.pos >= len(self.text) or self._peek() in '\n\r':
                break

            # Skip element separator if present
            if self._peek() == self.tokens['ELEMENT_SEPARATOR']:
                self._consume()
                self._skip_whitespace()

            if self.pos >= len(self.text) or self._peek() in '\n\r':
                break

            # Check if this is a named element or positional
            if self._has_assignment_ahead():
                self._parse_named_element()
            else:
                self._parse_positional_element()

    def _has_assignment_ahead(self) -> bool:
        """Check if there's an assignment operator before the next separator or newline"""
        temp_pos = self.pos
        while temp_pos < len(self.text):
            char = self.text[temp_pos]
            if char == self.tokens['ELEMENT_ASSIGNMENT']:
                return True
            elif char in '\n\r' + self.tokens['ELEMENT_SEPARATOR']:
                return False
            elif char == self.tokens['STRING_DELIMITER']:
                # Skip quoted strings
                if self.text[temp_pos:temp_pos+3] == self.tokens['MULTILINE_STRING_DELIMITER']:
                    temp_pos += 3
                    # Find closing triple quote
                    while temp_pos < len(self.text) - 2:
                        if self.text[temp_pos:temp_pos+3] == self.tokens['MULTILINE_STRING_DELIMITER']:
                            temp_pos += 3
                            break
                        temp_pos += 1
                else:
                    temp_pos += 1
                    # Skip to closing quote
                    while temp_pos < len(self.text) and self.text[temp_pos] != self.tokens['STRING_DELIMITER']:
                        if self.text[temp_pos] == '\\':
                            temp_pos += 2  # Skip escaped character
                        else:
                            temp_pos += 1
                    if temp_pos < len(self.text):
                        temp_pos += 1
            else:
                temp_pos += 1
        return False

    def _parse_named_element(self):
        """Parse a named element with name=value format"""
        # Read element name
        name = self._read_identifier()

        self._skip_whitespace()

        # Expect assignment operator
        if self._peek() != self.tokens['ELEMENT_ASSIGNMENT']:
            return

        self._consume()  # consume '='
        self._skip_whitespace()

        self.handler.start_element(name)
        self._parse_element_value()
        self.handler.end_element(name)

    def _parse_positional_element(self):
        """Parse a positional element (value only)"""
        # Treat positional elements as having a special name
        self.handler.start_element("_positional")
        self._parse_element_value()
        self.handler.end_element("_positional")

    def _parse_element_value(self):
        """Parse an element value with possible aspects"""
        # Parse core aspect
        self.handler.start_element_aspect("core")
        core_value = self._read_value(stop_at_aspect_markers=True)
        if core_value:
            self.handler.characters(core_value)
        self.handler.end_element_aspect("core")

        # Check for original aspect
        if self.pos < len(self.text) and self._peek() == self.tokens['ORIGINAL_MARKER']:
            self._consume()  # consume '%'
            self.handler.start_element_aspect("original")
            original_value = self._read_value(stop_at_aspect_markers=True)
            if original_value:
                self.handler.characters(original_value)
            self.handler.end_element_aspect("original")

        # Check for comment aspect
        if self.pos < len(self.text) and self._peek() == self.tokens['COMMENT_MARKER']:
            self._consume()  # consume '#'
            self.handler.start_element_aspect("comment")
            comment_value = self._read_value(stop_at_aspect_markers=False)
            if comment_value:
                self.handler.characters(comment_value)
            self.handler.end_element_aspect("comment")

    def _read_identifier(self) -> str:
        """Read an identifier (group or element name)"""
        result = []
        while self.pos < len(self.text):
            char = self._peek()
            if char.isalnum() or char in '-_':
                result.append(self._consume())
            else:
                break
        return ''.join(result)

    def _read_value(self, stop_at_aspect_markers: bool = False) -> str:
        """Read a value (simple, quoted, or multiline string)"""
        self._skip_whitespace()

        if self.pos >= len(self.text):
            return ""

        # Check for multiline string delimiter
        if self._peek(3) == self.tokens['MULTILINE_STRING_DELIMITER']:
            return self._read_multiline_string()

        # Check for regular quoted string
        if self._peek() == self.tokens['STRING_DELIMITER']:
            return self._read_quoted_string()

        # Read simple value
        return self._read_simple_value(stop_at_aspect_markers)

    def _read_multiline_string(self) -> str:
        """Read a multiline string enclosed in triple quotes"""
        # Consume opening triple quotes
        self._consume(3)

        result = []
        while self.pos < len(self.text) - 2:
            # Check for closing triple quotes
            if self._peek(3) == self.tokens['MULTILINE_STRING_DELIMITER']:
                self._consume(3)  # consume closing triple quotes
                break
            result.append(self._consume())

        return ''.join(result)

    def _read_quoted_string(self) -> str:
        """Read a quoted string"""
        # Consume opening quote
        self._consume()

        result = []
        while self.pos < len(self.text):
            char = self._peek()
            if char == '\\':
                # Escaped character
                self._consume()  # consume backslash
                if self.pos < len(self.text):
                    result.append(self._consume())  # consume escaped char
            elif char == self.tokens['STRING_DELIMITER']:
                self._consume()  # consume closing quote
                break
            else:
                result.append(self._consume())

        return ''.join(result)

    def _read_simple_value(self, stop_at_aspect_markers: bool) -> str:
        """Read a simple unquoted value"""
        result = []
        while self.pos < len(self.text):
            char = self._peek()

            # Stop at structural markers
            if char in '\n\r' + self.tokens['ELEMENT_SEPARATOR']:
                break

            # Stop at aspect markers if requested
            if stop_at_aspect_markers and char in (self.tokens['ORIGINAL_MARKER'] + self.tokens['COMMENT_MARKER']):
                break

            result.append(self._consume())

        return ''.join(result).strip()


# Example implementation of the handler
class ExampleKleioHandler(KleioParserHandler):
    """Example handler that collects group data in structured dictionaries"""

    def __init__(self):
        self.current_group_name = None
        self.current_group_data = None
        self.current_element_name = None
        self.current_element_aspects = None
        self.current_aspect_name = None
        self.current_aspect_value = None

    def start_group(self, name: str, level: int):
        """Start a new group and initialize its data dictionary"""
        self.current_group_name = name
        self.current_group_data = {}
        indent = "  " * level
        print(f"{indent}Start Group: {name}")

    def end_group(self, name: str, level: int):
        """End group and output its collected data"""
        indent = "  " * level
        print(f"{indent}End Group: {name}")
        print(f"{indent}Group Data:")
        for elem_name, elem_values in self.current_group_data.items():
            print(f"{indent}  {elem_name}: {elem_values}")
        print()

    def start_element(self, name: str):
        """Start a new element and initialize its aspects storage"""
        self.current_element_name = name
        self.current_element_aspects = {'core': '', 'original': '', 'comment': ''}
        print(f"  Start Element: {name}")

    def end_element(self, name: str):
        """End element and store it in the group data"""
        print(f"  End Element: {name}")

        # Create tuple of (core, original, comment)
        aspect_tuple = (
            self.current_element_aspects.get('core', ''),
            self.current_element_aspects.get('original', ''),
            self.current_element_aspects.get('comment', '')
        )

        # Add to group data - elements can have multiple values, so use a list
        if self.current_element_name not in self.current_group_data:
            self.current_group_data[self.current_element_name] = []

        self.current_group_data[self.current_element_name].append(aspect_tuple)

    def start_element_aspect(self, aspect: str):
        """Start an aspect and prepare to collect its value"""
        self.current_aspect_name = aspect
        self.current_aspect_value = []
        print(f"    Start Aspect: {aspect}")

    def end_element_aspect(self, aspect: str):
        """End aspect and store its collected value"""
        value = ''.join(self.current_aspect_value)
        self.current_element_aspects[aspect] = value
        print(f"    End Aspect: {aspect} = '{value}'")

    def characters(self, data: str):
        """Collect character data for the current aspect"""
        if self.current_aspect_value is not None:
            self.current_aspect_value.append(data)


# Usage example:
if __name__ == "__main__":
    # Example usage
    ehandler = ExampleKleioHandler()
    parser = KleioParser(ehandler)

    sample_kleio = '''
        texto$"""Aos 22 dias de Janeiro de 1788,
             baptiz o vigário Luís Barreto de Figuiredo Castilho
             """
        b$b1788.1118/22/1/1788/vigario luis barreto de figueiredo castilho e o reverendo cosme dias ribeiro de coimbra

            n$bernardo/m/obs=diz a margem casais, fl.10v/id=b1788.1118-per1
               ls$datanasc/17880111
    '''

    parser.parse(sample_kleio)
