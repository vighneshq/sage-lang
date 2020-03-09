from abc import ABC, abstractmethod


class AST(ABC):
    """ Abstract base class for abstract syntax tree node. """

    @abstractmethod
    def accept(self, visitor):
        """ Accept the visitor and run the right method. """
        pass


class Program(AST):
    """ Root node of the AST, representing the entire program.

    Attributes:
        stmts ([Stmt]): list of statements in the source program
    """

    def __init__(self, stmts):
        self.stmts = stmts

    def accept(self, visitor):
        """ Accept the visitor and run the right method. """

        visitor.visit_block_stmt(self)
