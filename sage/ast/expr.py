from sage.ast.ast import AST
from sage.util.util import unpack_list


class BinaryExpr(AST):
    """ Concrete-class representing binary expression nodes
    in the ast.

    Attributes:
        left (AST): Left operand of the expression.
        token (Token): Token representing the operator of the expression.
        right (AST): Right operand of the expression.
    """

    def __init__(self, left, token, right):
        self.left = left
        self.token = token
        self.right = right

    def __repr__(self):
        """ Returns string representation to be used while debugging. """

        return f"({self.left} {self.token.value} {self.right})"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()


class UnaryExpr(AST):
    """ Concrete-class representing unary expression nodes
    in the ast.

    Attributes:
        token (Token): Token representing the operator of the expression.
        right (AST): Right operand of the expression.
    """

    def __init__(self, token, right):
        self.token = token
        self.right = right

    def __repr__(self):
        """ Returns string representation to be used while debugging. """

        return f"({self.token.value} {self.right})"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()


class GroupedExpr(AST):
    """ Concrete-class representing grouped (parenthesized)
    expression nodes in the ast.

    Attributes:
        token (Token): Token representing the left parenthesis.
        expr (AST): Expression
    """

    def __init__(self, token, expr):
        self.token = token
        self.expr = expr

    def __repr__(self):
        """ Returns string representation to be used while debugging. """
        return f"{self.expr}"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()


class LiteralExpr(AST):
    """ Concrete-class representing literal expression nodes
    in the AST.

    Attributes:
        token (Token): Token representing the literal lexeme.
        value (Object): Literal value of the node (ex - True or 3.141)
    """

    def __init__(self, token, value):
        self.token = token
        self.value = value

    def __repr__(self):
        """ Returns string representation to be used while debugging. """

        return f"{self.value}"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()


class VariableExpr(AST):
    """ Concrete-class representing variable (identifier) nodes
    in the AST.

    Attributes:
        token (Token): Token representing the variable lexeme.
        value (Object): Identifier's lexeme.
        data_type (Token): Identifier's data type.
    """

    def __init__(self, token, value, data_type=None):
        self.token = token
        self.value = value
        self.data_type = data_type

    def __repr__(self):
        """ Returns string representation to be used while debugging. """

        return f"{self.value}"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()


class CallExpr(AST):
    """ Concrete-class representing call expression nodes
    in the AST.

    Attributes:
        callee (Expr): VariableExpr node reprensenting the callee.
        token (Token): Token representing the left parenthesis from the
            function call.
        args ([Expr]): List of expressions passed as arguments to the function.
    """

    def __init__(self, callee, token, args):
        self.callee = callee
        self.token = token
        self.args = args

    def __repr__(self):
        """ Returns string representation to be used while debugging. """

        return f"{self.callee}({unpack_list(self.args)})"

    def __str__(self):
        """ For sake of completeness, purpose same as __repr__."""

        return self.__repr__()
