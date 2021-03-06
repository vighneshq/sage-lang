class SemanticError(Exception):
    """ Custom exception to be raised when there is an error in
    the semantic analysis phase.

    Attributes:
        msg (string): String containing information about the error such as
            the cause, line number, and column number of occurence.
    """

    def __init__(self, msg):
        self.msg = f"{self.__class__.__name__}: {msg}"
