"""Tests for the Kleio parser modules.

Tests the lexer and syntax parser functionality.
"""
import pytest

from kleio.parser.lexer import Token, tokenize_data, get_tokens
from kleio.parser.syntax import parse_line, QuoteState
from kleio.parser.models import (
    NewGroup, NewElement, EndElement, NewEntry, NewAspect, StoreCore, Aspect
)


class TestLexer:
    """Tests for the lexical tokenizer."""
    
    def test_tokenize_simple_name(self):
        """Test tokenizing a simple name."""
        tokens = tokenize_data("acto")
        assert len(tokens) == 1
        assert tokens[0].type == 'names'
        assert tokens[0].value == 'acto'
    
    def test_tokenize_group_line(self):
        """Test tokenizing a group line with group marker."""
        tokens = tokenize_data("acto$joao")
        assert tokens[0].type == 'names'
        assert tokens[0].value == 'acto'
        assert tokens[1].type == 'dataflag'
        assert tokens[1].value == 1  # dollar
        assert tokens[2].type == 'names'
        assert tokens[2].value == 'joao'
    
    def test_tokenize_with_slashes(self):
        """Test tokenizing with slash separators."""
        tokens = tokenize_data("acto$joao/maria")
        assert tokens[3].type == 'dataflag'
        assert tokens[3].value == 2  # slash
    
    def test_tokenize_element_assignment(self):
        """Test tokenizing element=assignment."""
        tokens = tokenize_data("nome=joao")
        assert tokens[0].type == 'names'
        assert tokens[0].value == 'nome'
        assert tokens[1].type == 'dataflag'
        assert tokens[1].value == 3  # equal
    
    def test_tokenize_aspects(self):
        """Test tokenizing aspect markers."""
        tokens = tokenize_data("joao%original#comment")
        assert tokens[1].type == 'dataflag'
        assert tokens[1].value == 5  # percent
        assert tokens[3].type == 'dataflag'
        assert tokens[3].value == 4  # cardinal
    
    def test_tokenize_entry_separator(self):
        """Test tokenizing entry separator."""
        tokens = tokenize_data("joao;manuel")
        assert tokens[1].type == 'dataflag'
        assert tokens[1].value == 8  # semicolon
    
    def test_tokenize_number(self):
        """Test tokenizing numeric values."""
        tokens = tokenize_data("1234")
        assert tokens[0].type == 'number'
        assert tokens[0].value == '1234'
    
    def test_tokenize_decimal(self):
        """Test tokenizing decimal numbers."""
        tokens = tokenize_data("3.14")
        assert tokens[0].type == 'number'
        assert tokens[0].value == '3.14'
    
    def test_tokenize_fill(self):
        """Test tokenizing whitespace fill."""
        tokens = tokenize_data("  hello")
        assert tokens[0].type == 'fill'
        assert tokens[0].value == '  '
    
    def test_tokenize_dquote(self):
        """Test tokenizing double quote."""
        tokens = tokenize_data('"hello"')
        assert tokens[0].type == 'dquote'
        assert tokens[1].type == 'names'
        assert tokens[2].type == 'dquote'
    
    def test_tokenize_tquote(self):
        """Test tokenizing triple quote."""
        tokens = tokenize_data('"""hello"""')
        assert tokens[0].type == 'tquote'
        assert tokens[0].value == '"""'


class TestSyntaxSimple:
    """Tests for basic syntax parsing."""
    
    def test_empty_line(self):
        """Test parsing an empty line."""
        state = QuoteState()
        tokens = []
        actions = parse_line(tokens, state)
        assert actions == []
    
    def test_simple_group_line(self):
        """Test parsing a simple group line.
        
        Input: acto$joao/maria/1234
        Expected: NewGroup("acto"), StoreCore("joao"), EndElement, 
                  StoreCore("maria"), EndElement, StoreCore("1234")
        """
        state = QuoteState()
        tokens = tokenize_data("acto$joao/maria/1234")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 6
        assert isinstance(actions[0], NewGroup)
        assert actions[0].name == "acto"
        assert isinstance(actions[1], StoreCore)
        assert actions[1].value == "joao"
        assert isinstance(actions[2], EndElement)
        assert isinstance(actions[3], StoreCore)
        assert actions[3].value == "maria"
        assert isinstance(actions[4], EndElement)
        assert isinstance(actions[5], StoreCore)
        assert actions[5].value == "1234"
    
    def test_element_assignment(self):
        """Test parsing element assignment.
        
        Input: nome=joao
        Expected: NewElement("nome"), StoreCore("joao")
        """
        state = QuoteState()
        tokens = tokenize_data("nome=joao")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 2
        assert isinstance(actions[0], NewElement)
        assert actions[0].name == "nome"
        assert isinstance(actions[1], StoreCore)
        assert actions[1].value == "joao"
    
    def test_aspect_switching(self):
        """Test parsing aspect switching.
        
        Input: joao%original#comment
        Expected: StoreCore("joao"), NewAspect(ORIGINAL), StoreCore("original"), 
                  NewAspect(COMMENT), StoreCore("comment")
        """
        state = QuoteState()
        tokens = tokenize_data("joao%original#comment")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 5
        assert isinstance(actions[0], StoreCore)
        assert actions[0].value == "joao"
        assert isinstance(actions[1], NewAspect)
        assert actions[1].aspect == Aspect.ORIGINAL
        assert isinstance(actions[2], StoreCore)
        assert actions[2].value == "original"
        assert isinstance(actions[3], NewAspect)
        assert actions[3].aspect == Aspect.COMMENT
        assert isinstance(actions[4], StoreCore)
        assert actions[4].value == "comment"
    
    def test_entry_separator(self):
        """Test parsing entry separator.
        
        Input: joao;manuel
        Expected: StoreCore("joao"), NewEntry, StoreCore("manuel")
        """
        state = QuoteState()
        tokens = tokenize_data("joao;manuel")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 3
        assert isinstance(actions[0], StoreCore)
        assert actions[0].value == "joao"
        assert isinstance(actions[1], NewEntry)
        assert isinstance(actions[2], StoreCore)
        assert actions[2].value == "manuel"
    
    def test_fill_becomes_space(self):
        """Test that fill sequences become single space."""
        state = QuoteState()
        tokens = tokenize_data("  hello  world")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 4
        assert isinstance(actions[0], StoreCore)
        assert actions[0].value == " "
        assert isinstance(actions[1], StoreCore)
        assert actions[1].value == "hello"
        assert isinstance(actions[2], StoreCore)
        assert actions[2].value == " "
        assert isinstance(actions[3], StoreCore)
        assert actions[3].value == "world"
    
    def test_continuation_line(self):
        """Test parsing a continuation line (no group marker)."""
        state = QuoteState()
        tokens = tokenize_data("joao/maria")
        actions = parse_line(tokens, state)
        
        # Should not have NewGroup
        assert not any(isinstance(a, NewGroup) for a in actions)
        assert len(actions) == 3
        assert isinstance(actions[0], StoreCore)
        assert actions[0].value == "joao"
        assert isinstance(actions[1], EndElement)
        assert isinstance(actions[2], StoreCore)
        assert actions[2].value == "maria"
    
    def test_group_with_fill_prefix(self):
        """Test parsing group with fill prefix (indented).
        
        The fill before a group is consumed but not stored.
        """
        state = QuoteState()
        tokens = tokenize_data("  acto$joao")
        actions = parse_line(tokens, state)
        
        # Fill before group is consumed but not stored
        assert len(actions) == 2
        assert isinstance(actions[0], NewGroup)
        assert actions[0].name == "acto"
        assert isinstance(actions[1], StoreCore)
        assert actions[1].value == "joao"


class TestSyntaxQuotes:
    """Tests for quote handling in syntax parsing."""
    
    def test_double_quote_enter_exit(self):
        """Test entering and exiting double-quote mode."""
        state = QuoteState()
        tokens = tokenize_data('"hello"')
        actions = parse_line(tokens, state)
        
        assert len(actions) == 3
        assert isinstance(actions[0], StoreCore)
        assert actions[0].value == '"'
        assert isinstance(actions[1], StoreCore)
        assert actions[1].value == "hello"
        assert isinstance(actions[2], StoreCore)
        assert actions[2].value == '"'
        assert not state.double_quote_on  # Should be off after
    
    def test_double_quote_across_lines(self):
        """Test double-quote mode persists across lines."""
        state = QuoteState()
        
        # First line: open quote
        tokens1 = tokenize_data('"hello')
        actions1 = parse_line(tokens1, state)
        
        assert len(actions1) == 2
        assert actions1[0].value == '"'
        assert actions1[1].value == "hello"
        assert state.double_quote_on  # Should still be on
        
        # Second line: continue and close
        tokens2 = tokenize_data('world"')
        actions2 = parse_line(tokens2, state)
        
        assert len(actions2) == 2
        assert actions2[0].value == "world"
        assert actions2[1].value == '"'
        assert not state.double_quote_on  # Should be off now
    
    def test_double_quote_skips_returns(self):
        """Test that returns are skipped inside double quotes."""
        state = QuoteState()
        state.double_quote_on = True
        
        tokens = tokenize_data("hello\r")
        actions = parse_line(tokens, state)
        
        # Should only have StoreCore("hello"), return is skipped
        assert len(actions) == 1
        assert actions[0].value == "hello"
    
    def test_double_quote_fill_becomes_space(self):
        """Test that fill becomes space inside double quotes."""
        state = QuoteState()
        state.double_quote_on = True
        
        tokens = tokenize_data("  ")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 1
        assert actions[0].value == " "
    
    def test_triple_quote_enter_exit(self):
        """Test entering and exiting triple-quote mode."""
        state = QuoteState()
        tokens = tokenize_data('"""hello"""')
        actions = parse_line(tokens, state)
        
        assert len(actions) == 3
        assert isinstance(actions[0], StoreCore)
        assert actions[0].value == '"""'
        assert isinstance(actions[1], StoreCore)
        assert actions[1].value == "hello"
        assert isinstance(actions[2], StoreCore)
        assert actions[2].value == '"""'
        assert not state.triple_quote_on
    
    def test_triple_quote_across_lines(self):
        """Test triple-quote mode persists across lines."""
        state = QuoteState()
        
        # First line: open triple quote
        tokens1 = tokenize_data('"""hello')
        actions1 = parse_line(tokens1, state)
        
        assert len(actions1) == 2
        assert actions1[0].value == '"""'
        assert actions1[1].value == "hello"
        assert state.triple_quote_on
        
        # Second line: content
        tokens2 = tokenize_data("world")
        actions2 = parse_line(tokens2, state)
        
        assert len(actions2) == 1
        assert actions2[0].value == "world"
        assert state.triple_quote_on
        
        # Third line: close
        tokens3 = tokenize_data('"""')
        actions3 = parse_line(tokens3, state)
        
        assert len(actions3) == 1
        assert actions3[0].value == '"""'
        assert not state.triple_quote_on
    
    def test_triple_quote_everything_is_storecore(self):
        """Test that everything is StoreCore inside triple quotes."""
        state = QuoteState()
        state.triple_quote_on = True
        
        # Even dataflags should be stored as characters
        tokens = tokenize_data("$/#%")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 4
        for action in actions:
            assert isinstance(action, StoreCore)
        assert actions[0].value == "$"
        assert actions[1].value == "/"
        assert actions[2].value == "#"
        assert actions[3].value == "%"
    
    def test_triple_quote_stores_returns(self):
        """Test that returns are stored inside triple quotes."""
        state = QuoteState()
        state.triple_quote_on = True
        
        tokens = tokenize_data("hello\r")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 2
        assert actions[0].value == "hello"
        assert actions[1].value == "\r"


class TestSyntaxBackslash:
    """Tests for backslash escaping."""
    
    def test_backslash_escape_dataflag(self):
        """Test backslash escaping a dataflag character."""
        state = QuoteState()
        # \$ should store $ literally
        tokens = tokenize_data("\\$")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 1
        assert isinstance(actions[0], StoreCore)
        assert actions[0].value == "$"
    
    def test_backslash_escape_slash(self):
        """Test backslash escaping a slash."""
        state = QuoteState()
        tokens = tokenize_data("\\/")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 1
        assert actions[0].value == "/"
    
    def test_backslash_escape_name(self):
        """Test backslash escaping a name character."""
        state = QuoteState()
        tokens = tokenize_data("\\a")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 1
        assert actions[0].value == "a"


class TestSyntaxEdgeCases:
    """Tests for edge cases."""
    
    def test_name_not_element_without_equal(self):
        """Test that a name without = is StoreCore, not NewElement."""
        state = QuoteState()
        tokens = tokenize_data("nome")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 1
        assert isinstance(actions[0], StoreCore)
        assert actions[0].value == "nome"
    
    def test_complex_group_line(self):
        """Test a complex group line with multiple elements."""
        state = QuoteState()
        # acto$a1/01/01/1700/date=mydate/typ=test
        tokens = tokenize_data("acto$a1/01/01/1700/date=mydate/typ=test")
        actions = parse_line(tokens, state)
        
        assert isinstance(actions[0], NewGroup)
        assert actions[0].name == "acto"
        # Should have StoreCore, EndElement patterns for each element
        assert any(isinstance(a, NewElement) for a in actions)
    
    def test_returns_ignored_in_normal_mode(self):
        """Test that returns are ignored in normal mode."""
        state = QuoteState()
        tokens = tokenize_data("hello\r")
        actions = parse_line(tokens, state)
        
        # Should only have StoreCore("hello")
        assert len(actions) == 1
        assert actions[0].value == "hello"
    
    def test_multiple_entry_separators(self):
        """Test multiple entry separators."""
        state = QuoteState()
        tokens = tokenize_data("a;b;c")
        actions = parse_line(tokens, state)
        
        assert len(actions) == 5
        assert isinstance(actions[0], StoreCore)
        assert actions[0].value == "a"
        assert isinstance(actions[1], NewEntry)
        assert isinstance(actions[2], StoreCore)
        assert actions[2].value == "b"
        assert isinstance(actions[3], NewEntry)
        assert isinstance(actions[4], StoreCore)
        assert actions[4].value == "c"


class TestIntegration:
    """Integration tests combining lexer and parser."""
    
    def test_full_group_parsing(self):
        """Test parsing a complete group definition."""
        state = QuoteState()
        text = 'acto$a1/01/01/1700/date=mydate/typ=test'
        tokens = tokenize_data(text)
        actions = parse_line(tokens, state)
        
        # Verify structure
        assert isinstance(actions[0], NewGroup)
        assert actions[0].name == "acto"
        
        # Find NewElement actions
        new_elements = [a for a in actions if isinstance(a, NewElement)]
        assert len(new_elements) == 2
        assert new_elements[0].name == "date"
        assert new_elements[1].name == "typ"
    
    def test_pseudo_number_handling(self):
        """Test handling of pseudo-numbers like 5.10.1765."""
        state = QuoteState()
        text = 'date$string=5.10.1765'
        tokens = tokenize_data(text)
        actions = parse_line(tokens, state)
        
        # Should parse correctly
        assert any(isinstance(a, NewGroup) for a in actions)
        assert any(isinstance(a, NewElement) for a in actions)
    
    def test_quoted_string_in_data(self):
        """Test handling quoted strings in data mode."""
        state = QuoteState()
        text = 'acto$asf.4#"http://timelink.uc.pt"/24/5/1958'
        tokens = tokenize_data(text)
        actions = parse_line(tokens, state)
        
        # Should have NewGroup and handle the quoted content
        assert isinstance(actions[0], NewGroup)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
