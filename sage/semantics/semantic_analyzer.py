from sage.tokens.tokens import TokenType
from sage.ast.stmt import FunctionStmt
from sage.error.semantic_error import SemanticError
from sage.util.util import display_error, Visitor
from sage.semantics.symbols import (TypeSymbol, VariableSymbol,
                                    FunctionSymbol, SymbolTable)


class SemanticAnalyzer(Visitor):
    """ Performs static semantic checks on the ast returned by the
    parser.

    It works in 2 passes. In the first pass, top level function declarations
    are collected (to ensure that functions don't have to be declared before
    use, like C).

    In the second pass, everything else is checked.
    Attributes:
        _curr_scope (SymbolTable): SymbolTable representing current lexical
            scope.
        _scopes ({AST: SymbolTable}): dictionary mapping scoping AST nodes
            scopes to their corresponding symbol tables.
        _inside_loop (bool): boolean indication if we are inside a loop-block
        _inside_function (bool): boolean indication if we are inside a
            function-block
    """

    def __init__(self, **kwargs):
        self._curr_scope = SymbolTable()
        self._scopes = {}
        self._inside_loop = False
        self._inside_function = False
        self.had_error = False
        self._debug = kwargs.get("debug", False)

    def _error(self, token, msg):
        """ Raises error found while parsing. """

        self.had_error = True

        line_info = f"Line {token.line_no}, Column {token.col_no}"
        if token.token_type != TokenType.EOF:
            line_info = f"{line_info} near [Lexeme - {token.value}] -"
        else:
            line_info = f"{line_info} -"

        formatted_msg = f"SemanticError: {line_info} {msg}."

        display_error(formatted_msg)
        if self._debug:
            raise SemanticError(formatted_msg)

    def check(self, prog):
        """ Entry method where the semantic analysis begins. """

        self._visit(prog)

    def _visit(self, node):
        """ AST node being currently visited.

        Instead of using isinstance checks to figure out which node to accept,
        we just use the node to call an accept method. Inside the accept
        method, for each class X, we implement a visit_x method.

        Args:
            node (AST): node to visit in the AST.
        """

        node.accept(self)

    def visit_block_stmt(self, node):
        """ Visit a block statement, and analyse it.

        We declare a new nested scope for the block to check in. In the first
        pass, we collect the function declarations in the block, and then
        we analyze other statements recursively.

        Arg:
            node (AST): node to visit in the AST.
        """

        curr_scope = self._curr_scope
        new_scope = SymbolTable(curr_scope)
        self._curr_scope = new_scope

        # Collect all the function in the list of scopes
        # This is done so that functions can be called without them
        # being declared above or with prototypes (like C requires).
        for stmt in node.stmts:
            if isinstance(stmt, FunctionStmt):
                function_name = stmt.token.value
                if self._curr_scope.lookup(
                        function_name, curr_scope_only=True) is not None:
                    self._error(
                        stmt.token,
                        f"Function '{function_name}' declared more than once")
                    continue

                function_scope = SymbolTable(self._curr_scope)
                for param in stmt.params:
                    param_symbol = VariableSymbol(
                        param.value,
                        param.data_type.value)
                    function_scope.insert(param_symbol)

                ret_type = None
                if stmt.ret_type is not None:
                    ret_type = stmt.ret_type.value

                function_symbol = FunctionSymbol(
                    stmt.token.value,
                    stmt.params,
                    ret_type)

                self._curr_scope.insert(function_symbol)
                self._scopes[stmt.token.value] = function_scope

        for stmt in node.stmts:
            self._visit(stmt)

        self._curr_scope = curr_scope

    def visit_jump_stmt(self, node):
        """ Visit a jump statement node, and analyse it.

        Check if jump is inside a loop.
        Arg:
            node (AST): node to visit in the AST.
        """

        if not self._inside_loop:
            self._error(node.token, f"Use of '{node.token.value}' outside loop")

    def visit_return_stmt(self, node):
        """ Visit a return statement node, and analyse it.

        Check if return is inside a function.
        Arg:
            node (AST): node to visit in the AST.
        """

        if not self._inside_function:
            self._error(node.token, f"Use of '{node.token.value}' outside function")

        if node.expr is not None:
            self._visit(node.expr)

    def visit_while_stmt(self, node):
        """ Visit a while statement, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        self._visit(node.cond)
        self._inside_loop = True
        self._visit(node.body)
        self._inside_loop = False

    def visit_if_stmt(self, node):
        """ Visit an if statement node, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        self._visit(node.cond)
        self._visit(node.then)
        if node.els is not None:
            self._visit(node.els)

    def visit_expr_stmt(self, node):
        """ Visit an expression statement node, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        self._visit(node.expr)

    def visit_let_stmt(self, node):
        """ Visit a let statement node, and analyse it.

        Check variables are not declared before, Add the declared variables in
        the symbol table.
        Arg:
            node (AST): node to visit in the AST.
        """

        for var in node.var_list:
            if self._curr_scope.lookup(
                    var["name"].value, curr_scope_only=True) is not None:
                self._error(
                    var["name"],
                    "Variable '{}' declared more than once in same scope".format(
                        var["name"].value))

            if var["init"] is not None:
                self._visit(var["init"])

            variable_symbol = VariableSymbol(var["name"].value, var["type"].value)
            self._curr_scope.insert(variable_symbol)

    def visit_function_stmt(self, node):
        """ Visit a function statement, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        curr_scope = self._curr_scope
        self._curr_scope = self._scopes[node.token.value]
        self._inside_function = True
        self._visit(node.body)
        self._inside_function = False
        self._curr_scope = curr_scope

    def visit_binary_expr(self, node):
        """ Visit a binary expression, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        self._visit(node.left)
        self._visit(node.right)

    def visit_unary_expr(self, node):
        """ Visit unary expr, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        self._visit(node.right)

    def visit_grouped_expr(self, node):
        """ Visit a grouped expression, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        self._visit(node.expr)

    def visit_literal_expr(self, node):
        """ Visit literal expr, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        pass

    def visit_variable_expr(self, node):
        """ Visit a variable expression, and analyse it.

        Check if the variable belongs to the current scope.

        Arg:
            node (AST): node to visit in the AST.
        """

        if self._curr_scope.lookup(node.token.value) is None:
            self._error(node.token, f"Undeclared variable '{node.value}'")

    def visit_call_expr(self, node):
        """ Visit function call expr, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        function_symbol = self._curr_scope.lookup(node.token.value)

        if function_symbol is None:
            self._error(
                node.token, f"Undeclared function '{node.token.value}'")

        no_of_args = len(node.args)
        if len(node.args) != len(function_symbol.params):
            self._error(
                node.token,
                f"Expected {len(function_symbol.params)} argument(s) to "
                + f"function, got {len(node.args)}")

        curr_scope = self._curr_scope
        for arg in node.args:
            self._visit(arg)
