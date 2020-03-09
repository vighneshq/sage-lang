from abc import abstractmethod

from sage.ast.ast import AST
from sage.util.util import unpack_list


class Expr(AST):
    """ Abstract base class to represent expression nodes in the ast. """

    def __init__(self, data_type=None):
        self.data_type = data_type

    @abstractmethod
    def __repr__(self):
        """ Return a string representation to be used while debugging. """
        pass


class BinaryExpr(Expr):
    """ Concrete-class representing binary expression nodes
    in the ast.

    Attributes:
        left (Expr): Left operand of the expression.
        token (Token): Token representing the operator of the expression.
        right (Expr): Right operand of the expression.
    """

    def __init__(self, left, token, right, data_type=None):
        super().__init__(data_type)
        self.left = left
        self.token = token
        self.right = right

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_binary_expr(self)

    def __repr__(self):
        """ Returns string representation to be used while debugging. """

        return f"({self.left} {self.token.value} {self.right})"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()


class UnaryExpr(Expr):
    """ Concrete-class representing unary expression nodes
    in the ast.

    Attributes:
        token (Token): Token representing the operator of the expression.
        right (Expr): Right operand of the expression.
    """

    def __init__(self, token, right, data_type=None):
        super().__init__(data_type)
        self.token = token
        self.right = right

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_unary_expr(self)

    def __repr__(self):
        """ Returns string representation to be used while debugging. """

        return f"({self.token.value} {self.right})"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()


class GroupedExpr(Expr):
    """ Concrete-class representing grouped (parenthesized)
    expression nodes in the ast.

    Attributes:
        token (Token): Token representing the left parenthesis.
        expr (Expr): Expression
    """

    def __init__(self, token, expr, data_type=None):
        super().__init__(data_type)
        self.token = token
        self.expr = expr

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_grouped_expr(self)

    def __repr__(self):
        """ Returns string representation to be used while debugging. """
        return f"{self.expr}"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()


class LiteralExpr(Expr):
    """ Concrete-class representing literal expression nodes
    in the AST.

    Attributes:
        token (Token): Token representing the literal lexeme.
        value (Object): Literal value of the node (ex - True or 3.141)
    """

    def __init__(self, token, value, data_type=None):
        super().__init__(data_type)
        self.token = token
        self.value = value

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_literal_expr(self)

    def __repr__(self):
        """ Returns string representation to be used while debugging. """

        return f"{self.value}"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()


class VariableExpr(Expr):
    """ Concrete-class representing variable (identifier) nodes
    in the AST.

    Attributes:
        token (Token): Token representing the variable lexeme.
        value (Object): Identifier's lexeme.
        data_type (Token): Identifier's data type.
    """

    def __init__(self, token, value, data_type=None):
        super().__init__(data_type)
        self.token = token
        self.value = value

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_variable_expr(self)

    def __repr__(self):
        """ Returns string representation to be used while debugging. """

        return f"{self.value}"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()


class CallExpr(Expr):
    """ Concrete-class representing call expression nodes
    in the AST.

    Attributes:
        callee (Expr): VariableExpr node reprensenting the callee.
        token (Token): Token representing the left parenthesis from the
            function call.
        args ([Expr]): List of expressions passed as arguments to the function.
    """

    def __init__(self, callee, token, args, data_type=None):
        super().__init__(data_type)
        self.callee = callee
        self.token = token
        self.args = args

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_call_expr(self)

    def __repr__(self):
        """ Returns string representation to be used while debugging. """

        return f"{self.callee}({unpack_list(self.args)})"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()
