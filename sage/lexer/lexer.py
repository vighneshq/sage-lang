from sage.tokens.tokens import TokenType, Token, RESERVED_WORDS
from sage.error.lexer_error import LexerError


class Lexer:
    """ A class representing the Lexical analyser.

    The Lexer transforms the input character stream of the source
    file into a stream of tokens, which are then fed to the parser.

    Attributes:
        _source (str): Ascii-formatted string representing the source
            file contents.
        _start (int): Integer representing position of a lexeme's
            starting character [index for _source].
        _curr (int): Integer representing the Lexer's current position
            in the source file [index for _source].
        _line_no (int): Line number on which the lexeme occurs in the
            source file.
        _col_no (int): Column number of the lexeme's starting character
            on the line where it occurs.
    """

    def __init__(self, source):
        """ Refer to the class's docstring for more information."""

        self._source = source
        self._start = 0
        self._curr = 0
        self._line_no = 1
        self._col_no = 0

    def _is_at_end(self):
        """ Check if the Lexer has reached the end of the source file.

        Returns:
            bool: Boolean indicating whether the Lexer has reached
                the end.
        """

        return self._curr >= len(self._source)

    def _peek(self):
        """ Return the character at the Lexer's current position.

        Returns the null-character if at end.
        """

        if self._is_at_end():
            return '\0'
        return self._source[self._curr]

    def _peek_next(self):
        """ Return the character after the Lexer's current position.

        Returns the null-character if there is no such character.
        """

        if self._curr + 1 >= len(self._source):
            return '\0'
        return self._source[self._curr + 1]

    def _advance(self):
        """ Increment the Lexer's position, and return the
            previous character.

        Returns:
            string: Character at the Lexer's position before
                incrementing.
        """

        curr_char = self._source[self._curr]
        self._curr += 1

        if curr_char == '\n':
            self._line_no += 1
            self._col_no = 0
        else:
            self._col_no += 1

        return curr_char

    def _advance_if_match(self, expected):
        """ Increment the Lexer's position only if current character
        matches the expeted.

        Args:
            expected (str): Non-null character to be matched against
                the current.

        Returns:
            bool: Indicating whether the Lexer has advanced or not.
        """

        if self._peek() == expected:
            self._advance()
            return True
        return False

    def _get_lexeme(self):
        """ Return the currently scanned lexeme. """

        return self._source[self._start:self._curr]

    def _make_token(self, token_type, value=None):
        """ Construct and return a token using the specified attributes.

        If no value is specified, it uses the lexeme scanned as
        the value.

        Args:
            token_type (TokenType): Token-class of the token to be
                created.
            value (Object): Value of the token (read Token's docstring
                for more information).

        Returns:
            Token: Token-object representing the lexeme that was
                tokenized.
        """

        if value is None:
            value = self._get_lexeme()

        length = self._curr - self._start
        col_no = self._col_no - length + 1

        return Token(token_type, value, self._line_no, col_no)

    def _error(self, msg):
        """ Raise an exception while scanning.

        Raises:
            Lexical error found while scanning.
        """

        formatted_msg = "Line {line_no}, Column {col_no} - {msg}.".format(
            line_no=self._line_no,
            col_no=self._col_no,
            msg=msg
        )

        raise LexerError(formatted_msg)

    def _handle_single_comment(self):
        """ Skip single-line comments from the source file. """

        while not self._is_at_end() and self._peek() != "\n":
            self._advance()

    def _handle_multi_comment(self):
        """ Skip multi-line comments from the source file. """

        while True:

            if self._peek() == '-' and self._peek_next() == '!':
                self._advance()
                self._advance()

                return

            if self._is_at_end():
                self._error("Non-terminating multi-line comment")

            self._advance()

    def _skip_formatting_and_comments(self):
        """ Skip spaces, tabs, carriage returns, newlines, & comments."""

        while not self._is_at_end():
            curr_char = self._peek()

            if curr_char.isspace():
                self._advance()
                continue

            if curr_char == '-':
                if self._peek_next() == '-':
                    self._handle_single_comment()
                    continue

            if curr_char == '!':
                if self._peek_next() == '-':
                    self._handle_multi_comment()
                    continue

            return

    def _handle_identifier(self):
        """ Tokenize the identifier/reserved word, and return its type.

        Returns:
            Token: Token-object representing the identifier that was
                tokenized.
        """
        while self._peek().isalnum() or self._peek() == "_":
            self._advance()

        value = self._get_lexeme()
        token_type = RESERVED_WORDS.get(value, TokenType.IDENT)

        if token_type == TokenType.TRUE:
            value = True
        elif token_type == TokenType.FALSE:
            value = False

        return self._make_token(token_type, value)

    def _handle_number(self):
        """ Tokenize a numeric literal.

        Returns:
            Token: Token-object representing the number that was
                tokenized.
        """

        while self._peek().isdigit():
            self._advance()

        if self._peek() != '.' or not self._peek_next().isdigit():
            value = int(self._get_lexeme())

            return self._make_token(TokenType.INT_LIT, value)

        self._advance()

        while self._peek().isdigit():
            self._advance()

        value = float(self._get_lexeme())

        return self._make_token(TokenType.REAL_LIT, value)

    def _handle_character(self):
        """ Tokenize a character literal.

        Returns:
            Token: Token-object representing the character that was
                tokenized.
        """

        # TODO - Handle escape sequences
        self._advance()
        if self._peek() != '\'':
            self._error("Non-terminating character literal")

        self._advance()
        value = self._get_lexeme()[1:-1]

        return self._make_token(TokenType.CHAR_LIT, value)


    def _handle_string(self):
        """ Tokenize a string literal.

        Returns:
            Token: Token-object representing the string that was
                tokenized.
        """

        # TODO - Handle escape sequences
        while not self._is_at_end() and self._peek() != '"':
            self._advance()

        if self._peek() != '"':
            self._error("Non-terminating string literal")

        self._advance()
        value = self._get_lexeme()[1:-1]

        return self._make_token(TokenType.STRING_LIT, value)


    def get_next_token(self):
        """ Tokenize the next lexeme.

        Skips the formatting, and returns a token representing the
        next lexeme (or EOF).

        Returns:
            Token: Token-object representing the lexeme that was
                tokenized.
        """

        self._skip_formatting_and_comments()

        self._start = self._curr

        if self._is_at_end():
            return self._make_token(TokenType.EOF, "$")

        curr_char = self._advance()

        if curr_char.isalpha():
            return self._handle_identifier()

        if curr_char.isdigit():
            return self._handle_number()

        # Multi-character tokens except identifiers, and literals
        if curr_char == '*':
            if self._advance_if_match('*'):
                return self._make_token(TokenType.EXP)

            return self._make_token(TokenType.MUL)

        if curr_char == '=':
            if self._advance_if_match('='):
                return self._make_token(TokenType.EQUAL)

            return self._make_token(TokenType.ASSIGN)

        if curr_char == '>':
            if self._advance_if_match('>'):
                return self._make_token(TokenType.BIT_RIGHT)

            if self._advance_if_match('='):
                return self._make_token(TokenType.GREATER_EQUAL)

            return make_token(TokenType.GREATER)

        if curr_char == '<':
            if self._advance_if_match('<'):
                return self._make_token(TokenType.BIT_LEFT)

            if self._advance_if_match('='):
                return self._make_token(TokenType.LESSER_EQUAL)

            return make_token(TokenType.LESSER)

        if curr_char == '!':
            if self._advance_if_match('='):
                return self._make_token(TokenType.NOT_EQUAL)

        if curr_char == '\'':
            return self._handle_character()

        if curr_char == '"':
            return self._handle_string()

        # For single-character tokens, try initializing the enum
        # using the lexeme
        try:
            token_type = TokenType(curr_char)
        except ValueError:
            self._error("Unexpected character")
        else:
            return self._make_token(token_type)

