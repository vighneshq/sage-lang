from sage.ast.ast import AST


class JumpStmt(AST):
    """ Concrete-class representing break, continue, nodes in the ast.

    Attributes:
        token (Token): Token representing the 'break', 'continue'.
    """

    def __init__(self, token):
        self.token = token


class WhileStmt(AST):
    """ Concrete-class representing while statement nodes
    in the ast.

    Attributes:
        token (Token): Token representing the 'while'.
        cond (Expr): Boolean expression for the while.
        body ([Stmt]) : List of statements in the body
    """

    def __init__(self, token, cond, body):
        self.token = token
        self.cond = cond
        self.body = body


class IfStmt(AST):
    """ Concrete-class representing if statement nodes
    in the ast.

    Attributes:
        token (Token): Token represnting the 'if'.
        cond (Expr): Boolean expression for if.
        then ([Stmt]): Then-block of the if statement, exectued when condition
            is true.
        els ([Stmt]): Else-block of the if statement if any, executed when
            condition is false.
    """

    def __init__(self, token, cond, then, els):
        self.token = token
        self.cond = cond
        self.then = then
        self.els = els


class ExprStmt(AST):
    """ Concrete-class representing expression statement nodes in the ast.

    Expression statements include function calls.

    Attributes:
        expr (Expr): expression part of the statement.
    """

    def __init__(self, expr):
        self.expr = expr

