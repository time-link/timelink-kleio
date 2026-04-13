"""Tests for the Kleio lexer/tokenizer.

Ported from lexical.pl tests (lines 367-452).
"""
import pytest

from kleio.parser.lexer import (
    Token,
    chartype,
    data_flag,
    data_flag_char,
    tokenize_data,
    tokenize_cmd,
    get_tokens,
)


class TestChartype:
    """Tests for character classification."""
    
    def test_chartype_space(self):
        assert chartype(' ') == 'space'
    
    def test_chartype_tab(self):
        assert chartype('\t') == 'tab'
    
    def test_chartype_lower(self):
        assert chartype('a') == 'lower'
        assert chartype('z') == 'lower'
    
    def test_chartype_upper(self):
        assert chartype('A') == 'upper'
        assert chartype('Z') == 'upper'
    
    def test_chartype_digit(self):
        assert chartype('0') == 'digit'
        assert chartype('9') == 'digit'
    
    def test_chartype_newline(self):
        assert chartype('\n') == 'return'
        assert chartype('\r') == 'return'
    
    def test_chartype_special_chars(self):
        assert chartype('!') == 'exclamation'
        assert chartype('"') == 'doblequote'
        assert chartype('#') == 'cardinal'
        assert chartype('$') == 'dollar'
        assert chartype('%') == 'percent'
        assert chartype('&') == 'and'
        assert chartype("'") == 'singlequote'
        assert chartype('(') == 'openround'
        assert chartype(')') == 'closeround'
        assert chartype('*') == 'asterix'
        assert chartype('+') == 'plus'
        assert chartype(',') == 'comma'
        assert chartype('-') == 'minus'
        assert chartype('.') == 'point'
        assert chartype('/') == 'slash'
        assert chartype(':') == 'colon'
        assert chartype(';') == 'semicolon'
        assert chartype('<') == 'less'
        assert chartype('=') == 'equal'
        assert chartype('>') == 'greater'
        assert chartype('?') == 'question'
        assert chartype('@') == 'at'
        assert chartype('[') == 'squareopen'
        assert chartype('\\') == 'backslash'
        assert chartype(']') == 'squareclose'
        assert chartype('^') == 'circunflex'
        assert chartype('_') == 'underscore'
        assert chartype('|') == 'pipe'
    
    def test_chartype_other(self):
        # Characters not in the map get their own char as type
        # Non-ASCII letters are classified by their Unicode properties
        assert chartype('é') == 'lower'  # é is a lowercase letter
        assert chartype('É') == 'upper'  # É is an uppercase letter
        assert chartype('€') == '€'      # Euro sign has no special classification


class TestDataFlags:
    """Tests for data flag functions."""
    
    def test_data_flag_dollar(self):
        assert data_flag(1) == 'dollar'
    
    def test_data_flag_slash(self):
        assert data_flag(2) == 'slash'
    
    def test_data_flag_equal(self):
        assert data_flag(3) == 'equal'
    
    def test_data_flag_cardinal(self):
        assert data_flag(4) == 'cardinal'
    
    def test_data_flag_percent(self):
        assert data_flag(5) == 'percent'
    
    def test_data_flag_less(self):
        assert data_flag(6) == 'less'
    
    def test_data_flag_greater(self):
        assert data_flag(7) == 'greater'
    
    def test_data_flag_colon(self):
        assert data_flag(9) == 'colon'
    
    def test_data_flag_backslash(self):
        assert data_flag(10) == 'backslash'
    
    def test_data_flag_8_default(self):
        assert data_flag(8) == 'semicolon'
    
    def test_data_flag_8_custom(self):
        assert data_flag(8, 'pipe') == 'pipe'
    
    def test_data_flag_invalid(self):
        assert data_flag(99) is None
    
    def test_data_flag_char(self):
        assert data_flag_char(1) == '$'
        assert data_flag_char(2) == '/'
        assert data_flag_char(3) == '='
        assert data_flag_char(8) == ';'
        assert data_flag_char(8, 'pipe') == '|'


class TestTokenizeDataQuotes:
    """Test from lexical.pl line 371: get_tokens_dat_quotes."""
    
    def test_tokenize_data_quotes(self):
        """Tokenize: acto$asf.4#"htpp://timelink.uc.pt?"/24/5/1958/obs=url"""
        text = 'acto$asf.4#"htpp://timelink.uc.pt?"/24/5/1958/obs=url\r'
        tokens = tokenize_data(text)
        
        # Expected tokens (note: asf.4 is one name because . is valid in names):
        # names(acto), dataflag(1), names(asf.4), dataflag(4), dquote("), ...
        # Note: '.' is a valid name character, so 'asf.4' is parsed as a name
        
        # Check key tokens
        assert tokens[0] == Token('names', 'acto')
        assert tokens[1] == Token('dataflag', 1)  # $ is dollar (flag 1)
        assert tokens[2] == Token('names', 'asf.4')  # . is valid in names!
        assert tokens[3] == Token('dataflag', 4)  # # is cardinal (flag 4)
        assert tokens[4] == Token('dquote', '"')
        
        # Find the second dquote
        dquote_count = sum(1 for t in tokens if t.type == 'dquote')
        assert dquote_count == 2
        
        # Check for dataflag(3) for = 
        assert Token('dataflag', 3) in tokens
        
        # Verify we have the right structure
        # There should be two dquote tokens (opening and closing)
        dquote_count = sum(1 for t in tokens if t.type == 'dquote')
        assert dquote_count == 2


class TestTokenizeDataEscapedQuotes:
    """Test from lexical.pl line 380: get_tokens_dat_quotes_with_quotes."""
    
    def test_tokenize_data_escaped_quotes(self):
        """Tokenize with escaped quotes inside quoted string."""
        # In Kleio data mode, escaped quotes are handled by the syntax parser,
        # not the lexer. The lexer just sees individual characters.
        # Note: backslash is dataflag 10, so it's tokenized as dataflag(10)
        text = r'acto$asf.4#"htpp://timelink.uc.pt?\"xpto\""/24/5/1958/obs=url\r'
        tokens = tokenize_data(text)
        
        # The backslash and escaped quotes are separate tokens
        # because in data mode, quotes are handled by the syntax parser
        
        # Check we have dquote tokens
        dquote_tokens = [t for t in tokens if t.type == 'dquote']
        assert len(dquote_tokens) >= 2
        
        # Check we have backslash as dataflag(10)
        # In Kleio, backslash is dataflag 10 (escape character)
        backslash_dataflag = [t for t in tokens if t.type == 'dataflag' and t.value == 10]
        assert len(backslash_dataflag) >= 1


class TestTokenizeDataDanglingQuote:
    """Test from lexical.pl line 389: get_tokens_dat_quotes_dangling."""
    
    def test_tokenize_data_dangling_quote(self):
        """Tokenize with unclosed quote (should not crash)."""
        text = 'acto$asf.4/obs="url\r'
        tokens = tokenize_data(text)
        
        # Should have one dquote (opening) but no closing
        dquote_tokens = [t for t in tokens if t.type == 'dquote']
        assert len(dquote_tokens) == 1
        
        # Check other tokens exist
        assert Token('names', 'acto') in tokens
        assert Token('names', 'obs') in tokens


class TestDataFlag8Default:
    """Test from lexical.pl line 398: get_token_data_flag_8."""
    
    def test_data_flag_8_default(self):
        """Tokenize joaquim;manuel;costa with default semicolon separator."""
        text = 'joaquim;manuel;costa'
        tokens = tokenize_data(text)  # default separator is ';'
        
        # Semicolons should be dataflag(8)
        dataflag_8_tokens = [t for t in tokens if t.type == 'dataflag' and t.value == 8]
        assert len(dataflag_8_tokens) == 2
        
        # Check names
        assert Token('names', 'joaquim') in tokens
        assert Token('names', 'manuel') in tokens
        assert Token('names', 'costa') in tokens


class TestDataFlag8Pipe:
    """Test from lexical.pl line 409: get_token_data_flag_8_pipe."""
    
    def test_data_flag_8_pipe(self):
        """Tokenize joaquim|manuel|costa with pipe as separator."""
        text = 'joaquim|manuel|costa'
        tokens = tokenize_data(text, entry_separator='|')
        
        # Pipes should be dataflag(8)
        dataflag_8_tokens = [t for t in tokens if t.type == 'dataflag' and t.value == 8]
        assert len(dataflag_8_tokens) == 2
        
        # Check names
        assert Token('names', 'joaquim') in tokens
        assert Token('names', 'manuel') in tokens
        assert Token('names', 'costa') in tokens


class TestDataFlag8NotSemicolon:
    """Test from lexical.pl line 420: get_token_data_flag_8_not_semi_colon."""
    
    def test_data_flag_8_not_semicolon(self):
        """When separator is pipe, semicolons should NOT be dataflag(8)."""
        text = 'joaquim;manuel;costa'
        tokens = tokenize_data(text, entry_separator='|')
        
        # Semicolons should NOT be dataflag(8) - they should be semicolon type
        dataflag_8_tokens = [t for t in tokens if t.type == 'dataflag' and t.value == 8]
        assert len(dataflag_8_tokens) == 0
        
        # Semicolons should be regular semicolon tokens
        semicolon_tokens = [t for t in tokens if t.type == 'semicolon']
        assert len(semicolon_tokens) == 2


class TestTripleQuote:
    """Test from lexical.pl line 442: triple_quote."""
    
    def test_triple_quote(self):
        """Tokenize triple quoted string."""
        text = '""" \r one line \rtwo lines\r\r"""'
        tokens = tokenize_data(text)
        
        # Should have two tquote tokens (opening and closing)
        tquote_tokens = [t for t in tokens if t.type == 'tquote']
        assert len(tquote_tokens) == 2
        assert tquote_tokens[0] == Token('tquote', '"""')
        assert tquote_tokens[1] == Token('tquote', '"""')
    
    def test_triple_quote_unix_newlines(self):
        """Tokenize triple quoted string with Unix newlines."""
        text = '"""\none line\ntwo lines\n\n"""'
        tokens = tokenize_data(text)
        
        # Should have two tquote tokens
        tquote_tokens = [t for t in tokens if t.type == 'tquote']
        assert len(tquote_tokens) == 2


class TestTokenizeCmd:
    """Tests for command mode tokenization."""
    
    def test_tokenize_cmd_simple(self):
        """Tokenize a simple command line."""
        text = 'element name value'
        tokens = tokenize_cmd(text)
        
        assert Token('fill', ' ') in tokens
        assert Token('name', 'element') in tokens
        assert Token('name', 'name') in tokens
        assert Token('name', 'value') in tokens
    
    def test_tokenize_cmd_with_string(self):
        """Tokenize command with quoted string."""
        text = 'element "quoted value"'
        tokens = tokenize_cmd(text)
        
        assert Token('name', 'element') in tokens
        assert Token('string', '"quoted value"') in tokens
    
    def test_tokenize_cmd_with_slash(self):
        """Tokenize command with slash (dataflag 2)."""
        text = 'element/value'
        tokens = tokenize_cmd(text)
        
        assert Token('name', 'element') in tokens
        assert Token('dataflag', 2) in tokens  # slash
        assert Token('name', 'value') in tokens
    
    def test_tokenize_cmd_escaped_string(self):
        """Tokenize command with escaped quotes in string."""
        text = r'element "value with \"escaped\" quotes"'
        tokens = tokenize_cmd(text)
        
        string_tokens = [t for t in tokens if t.type == 'string']
        assert len(string_tokens) == 1
        # The escaped quotes should be preserved
        assert '"' in string_tokens[0].value
    
    def test_tokenize_cmd_number(self):
        """Tokenize command with number."""
        text = 'count 42'
        tokens = tokenize_cmd(text)
        
        assert Token('name', 'count') in tokens
        assert Token('number', '42') in tokens


class TestGetTokens:
    """Tests for the get_tokens convenience function."""
    
    def test_get_tokens_dat_mode(self):
        text = 'name$value'
        tokens = get_tokens('dat', text)
        
        assert Token('names', 'name') in tokens
        assert Token('dataflag', 1) in tokens  # dollar
        assert Token('names', 'value') in tokens
    
    def test_get_tokens_cmd_mode(self):
        text = 'name value'
        tokens = get_tokens('cmd', text)
        
        assert Token('name', 'name') in tokens
        assert Token('name', 'value') in tokens
    
    def test_get_tokens_invalid_mode(self):
        with pytest.raises(ValueError):
            get_tokens('invalid', 'text')


class TestNames:
    """Tests for name matching rules."""
    
    def test_names_simple(self):
        tokens = tokenize_data('abc')
        assert tokens[0] == Token('names', 'abc')
    
    def test_names_with_digits(self):
        tokens = tokenize_data('abc123')
        assert tokens[0] == Token('names', 'abc123')
    
    def test_names_with_point(self):
        tokens = tokenize_data('abc.def')
        assert tokens[0] == Token('names', 'abc.def')
    
    def test_names_with_minus(self):
        tokens = tokenize_data('abc-def')
        assert tokens[0] == Token('names', 'abc-def')
    
    def test_names_with_underscore(self):
        tokens = tokenize_data('abc_def')
        assert tokens[0] == Token('names', 'abc_def')
    
    def test_names_mixed(self):
        tokens = tokenize_data('my_variable-name.1')
        assert tokens[0] == Token('names', 'my_variable-name.1')
    
    def test_names_start_with_upper(self):
        tokens = tokenize_data('Abc')
        assert tokens[0] == Token('names', 'Abc')


class TestNumbers:
    """Tests for number matching rules."""
    
    def test_number_integer(self):
        tokens = tokenize_data('123')
        assert tokens[0] == Token('number', '123')
    
    def test_number_decimal(self):
        tokens = tokenize_data('3.14')
        assert tokens[0] == Token('number', '3.14')
    
    def test_number_trailing_decimal(self):
        tokens = tokenize_data('5.')
        assert tokens[0] == Token('number', '5.')
    
    def test_number_multiple_digits(self):
        tokens = tokenize_data('1234567890')
        assert tokens[0] == Token('number', '1234567890')


class TestFill:
    """Tests for fill (whitespace) matching."""
    
    def test_fill_spaces(self):
        tokens = tokenize_data('   ')
        assert tokens[0] == Token('fill', '   ')
    
    def test_fill_tabs(self):
        tokens = tokenize_data('\t\t')
        assert tokens[0] == Token('fill', '\t\t')
    
    def test_fill_mixed(self):
        tokens = tokenize_data(' \t ')
        assert tokens[0] == Token('fill', ' \t ')
