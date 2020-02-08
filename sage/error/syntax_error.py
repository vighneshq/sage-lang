class LexerError(Exception):
    """ Custom exception to be raised when there is an error in
    the source-code's lexical syntax.

    Attributes:
        msg (string): String containing information about the error such as
            the cause, line number, and column number of occurence.
    """

    def __init__(self, msg):
        self.msg = f"{self.__class__.__name__}: {msg}"


class ParserError(Exception):
    """ Custom exception to be raised when there is an error in
    the source-code's concrete syntax.

    Attributes:
        msg (string): String containing information about the error such as
            the cause, token of occurence.
    """

    def __init__(self, msg):
        self.msg = f"{self.__class__.__name__}: {msg}"

