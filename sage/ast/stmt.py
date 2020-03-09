from sage.ast.ast import AST


class JumpStmt(AST):
    """ Concrete-class representing break, continue statement nodes in
    the ast.

    Attributes:
        token (Token): Token representing the "break", "continue".
    """

    def __init__(self, token):
        self.token = token


class ReturnStmt(AST):
    """ Concrete-class representing return statement nodes in the ast.

    Attributes:
        token (Token): Token representing the "return".
        expr (Expr): Value to be returned if any.
    """

    def __init__(self, token, expr):
        self.token = token
        self.expr = expr


class WhileStmt(AST):
    """ Concrete-class representing while statement nodes
    in the ast.

    Attributes:
        token (Token): Token representing the "while".
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
        token (Token): Token representing the "if".
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


class LetStmt(AST):
    """ Concrete-class representing variable declaration nodes in the ast.

    Attributes:
        token (Token): token representing the "let"
        var_list ([{str: Token}]): list of variables declared
    """

    def __init__(self, token, var_list):
        self.token = token
        self.var_list = var_list


class FunctionStmt(AST):
    """ Concrete-class representing function declaration nodes in the ast.

    Attributes:
        token (Token): token representing the function name.
        param_types ([{str: Token}]):formal paramseters of the function.
        body ([Stmt]): list of statements in the function body.
        ret_type (Token): return type of the function (optional).
    """

    def __init__(self, token, params, ret_type, body):
        self.token = token
        self.params = params
        self.body = body
        self.ret_type = ret_type
