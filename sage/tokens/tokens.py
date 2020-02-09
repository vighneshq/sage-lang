from enum import Enum


class TokenType(Enum):
    """ Enumerated class representing token-classes of our language."""

    # Arithmetic Operators
    ADD             = "+"
    SUB             = "-"
    MUL             = "*"
    DIV             = "/"
    MOD             = "%"
    EXP             = "**"
    ASSIGN          = "="
    LPAREN          = "("
    RPAREN          = ")"
    # Bitwise Operators
    BIT_AND         = "&"
    BIT_OR          = "|"
    BIT_NOT         = "~"
    BIT_XOR         = "^"
    BIT_LEFT        = "<<"
    BIT_RIGHT       = ">>"
    # Conditional Operators
    EQUAL           = "=="
    NOT_EQUAL       = "!="
    GREATER         = ">"
    GREATER_EQUAL   = ">="
    LESSER          = "<"
    LESSER_EQUAL    = "<="
    # Literals
    CHAR_LIT        = "char_lit"
    INT_LIT         = "int_lit"
    REAL_LIT        = "real_lit"
    STRING_LIT      = "string_lit"
    # Reserved Words
    TRUE            = "true"
    FALSE           = "false"
    BOOL            = "Bool"
    CHAR            = "Char"
    INT             = "Int"
    REAL            = "Real"
    STRING          = "String"
    AND             = "and"
    OR              = "or"
    NOT             = "not"
    IF              = "if"
    ELSE            = "else"
    WHILE           = "while"
    BREAK           = "break"
    CONTINUE        = "continue"
    RETURN          = "return"
    # Misc
    LBRACE          = "{"
    RBRACE          = "}"
    DOT             = "."
    COMMA           = ","
    SEMI_COLON      = ";"
    IDENT           = "identifier"
    EOF             = "EOF"


def _build_reserved_words():
    """ Returns a dictionary of reserved words.

    Lexemes are the keys of the dicionary, and the
    corresponding token-classes are values.

    Example -
    {
        "true": TokenType.TRUE,
        "false": TokenType.FALSE,
            .
            .
            .
    }
    """

    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.TRUE)
    end_index = tt_list.index(TokenType.LBRACE)

    reserved_words = {
        token_type.value: token_type
        for token_type in tt_list[start_index:end_index]
    }

    return reserved_words


RESERVED_WORDS = _build_reserved_words()


class Token:
    """ A class used to represent the tokens emitted by the Lexer.

    The Lexer takes an input-stream of characters and tokenizes them
    according to the language's lexical specification. These tokens
    then form the input to the Parser.

    Attributes:
        token_type (TokenType): Enum representing the token's token
            class.
        value (Object): Stores a token's literal value for literals
            in the language. For other token-classes, stores the
            token's lexeme.
        line_no (int): Line number on which the token occurs in the
            source file.
        col_no (int): Column number of the token's starting character
            on the line where it occurs.
    """

    def __init__(self, token_type, value=None, line_no=None, col_no=None):
        """ Refer to the class's docstring for more information."""

        self.token_type = token_type
        self.value = value
        self.line_no = line_no
        self.col_no = col_no

    def __repr__(self):
        """ Return a readable representation of the token.

        Used for debbuging mostly.

        Returns:
            string: A readable representation of the token.
            Example:

            [TokenType.ADD, "+", Line 10, Col 5],
            [TokenType.INT_LIT, 123, Line 13, Col 25],
            [TokenType.IDENT, "average", Line 20, Col 10]
        """

        return "[{token_type}, {value}, Line {line_no}, Col {col_no}]".format(
            token_type=self.token_type,
            value=self.value,
            line_no=self.line_no,
            col_no=self.col_no
        )

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()
