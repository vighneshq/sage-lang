import pytest

from sage.tokens.tokens import Token, TokenType
from sage.lexer.lexer import Lexer
from sage.error.syntax_error import LexerError


class TestLexer:

    def _check_output(self, expected_output_list, source):
        lexer = Lexer(source)
        assert not lexer.had_error

        for expected_output in expected_output_list:
            next_token = lexer.get_next_token()

            assert expected_output["lexeme"] == next_token.value
            assert expected_output["token_type"] == next_token.token_type

        next_token = lexer.get_next_token()
        assert "$" == next_token.value
        assert TokenType.EOF == next_token.token_type

    def _check_lexer_error(self, source):
        lexer = Lexer(source)

        with pytest.raises(LexerError):
            while True:
                next_token = lexer.get_next_token()

                if next_token.token_type == TokenType.EOF:
                    break

    def test_terminated_comment(self):
        source = """ -- This is a single line comment.
        identifier
        !- This is a multi-line
        comment. -!"""

        lexer = Lexer(source)

        self._check_output(
            [{"lexeme": "identifier", "token_type": TokenType.IDENT}],
            source)

    def test_unterminated_comment(self):

        source = """!- This multi-line
        comment will not be
        terminated"""

        self._check_lexer_error(source)

    def test_operators(self):

        source = """+ - * / % ** = ( ) & | ~ ^ << >> == != > >= < <="""

        expected_output_list = [
            {"lexeme": lexeme, "token_type": TokenType(lexeme)} for lexeme
            in source.split()
        ]

        self._check_output(expected_output_list, source)

    def test_literals(self):

        source = """'a' 10 020 3.14 0.5 013.3 "Hello, World!" """

        expected_output_list = [
            {"lexeme": "a", "token_type": TokenType.CHAR_LIT},
            {"lexeme": "10", "token_type": TokenType.INT_LIT},
            {"lexeme": "020", "token_type": TokenType.INT_LIT},
            {"lexeme": "3.14", "token_type": TokenType.REAL_LIT},
            {"lexeme": "0.5", "token_type": TokenType.REAL_LIT},
            {"lexeme": "013.3", "token_type": TokenType.REAL_LIT},
            {"lexeme": "Hello, World!", "token_type": TokenType.STRING_LIT},
        ]

        self._check_output(expected_output_list, source)

    def test_unterminated_literals(self):

        source = """'a x = y"""
        self._check_lexer_error(source)

        source = """ This "string is not terminated """
        self._check_lexer_error(source)

    def test_reserved_words(self):
        source = """true false Bool Char Int Real String and or not if else
        while break continue return function let"""

        expected_output_list = [
            {"lexeme": lexeme, "token_type": TokenType(lexeme)} for lexeme
            in source.split()
        ]

        self._check_output(expected_output_list, source)

    def test_identifiers(self):
        source = """x y MAX avg_of_5 temp1 Parser"""

        expected_output_list = [
            {"lexeme": lexeme, "token_type": TokenType.IDENT} for lexeme
            in source.split()
        ]

        self._check_output(expected_output_list, source)

    def test_separators(self):
        source = """{ } . , ; : ->"""

        expected_output_list = [
            {"lexeme": lexeme, "token_type": TokenType(lexeme)} for lexeme
            in source.split()
        ]

        self._check_output(expected_output_list, source)

    def test_unknown_character_error(self):
        source = "!"
        self._check_lexer_error(source)

    def test_general(self):
        source = """
        function gcd(a: Int, b: Int) {
            if b == 0 {
                return a;
            }

            return gcd(b, a%b);
        }

        function main() {
            let d: Int = gcd(10, 5);
            print(d);
        }
        """

        lexer = Lexer(source)

        while not lexer._is_at_end():
            lexer.get_next_token()

        assert not lexer.had_error
