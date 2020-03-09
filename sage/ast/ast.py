class AST:
    """ Base class for abstract syntax tree node. """

    pass


class Program:
    """ Root node of the AST, representing the entire program.

    Attributes:
        stmts ([Stmt]): list of statements in the source program
    """

    def __init__(self, stmts):
        self.stmts = stmts
