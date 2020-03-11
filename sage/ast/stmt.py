from sage.ast.ast import AST


class JumpStmt(AST):
    """ Concrete-class representing break, continue statement nodes in
    the ast.

    Attributes:
        token (Token): Token representing the "break", "continue".
    """

    def __init__(self, token):
        self.token = token

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_jump_stmt(self)


class ReturnStmt(AST):
    """ Concrete-class representing return statement nodes in the ast.

    Attributes:
        token (Token): Token representing the "return".
        expr (Expr): Value to be returned if any.
    """

    def __init__(self, token, expr):
        self.token = token
        self.expr = expr

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_return_stmt(self)


class BlockStmt(AST):
    """ Concrete-class representing block nodes in the ast.

    Each block represents a scope in the source code. Blocks are created by
    If-Stmts, While-Stmts, Function Bodies.

    Attributes:
        token (Token): token representing the left curly brace.
        stmts ([stmt]): List of statements in the block.
    """

    def __init__(self, token, stmts):
        self.token = token
        self.stmts = stmts

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_block_stmt(self)


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

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_while_stmt(self)


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

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_if_stmt(self)


class ExprStmt(AST):
    """ Concrete-class representing expression statement nodes in the ast.

    Expression statements include function calls.

    Attributes:
        token (Token): token of the expression node.
        expr (Expr): expression part of the statement.
    """

    def __init__(self, token, expr):
        self.token = token
        self.expr = expr

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_expr_stmt(self)


class LetStmt(AST):
    """ Concrete-class representing variable declaration nodes in the ast.

    Attributes:
        token (Token): token representing the "let"
        variables ([{str: Token}]): list of variables declared
    """

    def __init__(self, token, variables):
        self.token = token
        self.variables = variables

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_let_stmt(self)


class VarDecl(AST):
    """ Concrete-class representing variable declaration nodes in the ast.

    Attributes:
        token (Token): token representing the "let".
        data_type (Token): data-type of the variable.
        init (Expr): intializing expressiong.
    """

    def __init__(self, token, data_type=None, init=None):
        self.token = token
        self.data_type = data_type
        self.init = init

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_var_decl(self)


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

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        visitor.visit_function_stmt(self)


class TypeStmt:
    """ Concrete-class representing type statements nodes in the ast.

    Attributes:
        token (Token): token representing the word "type".
        new_type (Token): new type being declared
        old_type (Token): old type being aliased
    """

    def __init__(self, token, new_type, old_type):
        self.token = token
        self.new_type = new_type
        self.old_type = old_type

    def accept(self, visitor):
        """ Accept the visitor and run the right method.

        Args:
            visitor (Visitor): that wants to perform a certain action.
        """

        pass
        # TODO
        # visitor.visit_var_decl(self)
