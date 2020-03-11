from sage.tokens.tokens import TokenType
from sage.ast.stmt import FunctionStmt
from sage.ast.expr import VariableExpr
from sage.error.semantic_error import SemanticError
from sage.util.util import display_error, Visitor
from sage.semantics.symbols import (TypeSymbol, VariableSymbol,
                                    FunctionSymbol, SymbolTable)
from sage.typing import primitives


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
        _curr_function_symbol (bool): boolean indication if we are inside a
            function-block
    """

    def __init__(self):
        self._curr_scope = SymbolTable()
        self._inside_loop = False
        self._curr_function_symbol = None
        self.had_error = False

    def _error(self, token, msg):
        """ Raises error found while parsing.

        Raises -
            SemanticError
        """

        self.had_error = True

        line_info = f"Line {token.line_no}, Column {token.col_no}"
        if token.token_type != TokenType.EOF:
            line_info = f"{line_info} near [Lexeme - {token.value}] -"
        else:
            line_info = f"{line_info} -"

        formatted_msg = f"{line_info} {msg}."
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
            try:
                if isinstance(stmt, FunctionStmt):
                    function_name = stmt.token.value
                    if self._curr_scope.lookup(
                            function_name, curr_scope_only=True) is not None:

                        self._error(
                            stmt.token,
                            "Function '{}' declared more than once".format(
                                function_name
                            ))

                        continue

                    function_scope = SymbolTable(self._curr_scope)
                    params = []
                    for param in stmt.params:
                        param_symbol = VariableSymbol(
                            param.value,
                            param.data_type.value)
                        params.append(param_symbol)
                        function_scope.insert(param_symbol)

                    ret_type = None
                    if stmt.ret_type is not None:
                        ret_type = stmt.ret_type.value

                    function_symbol = FunctionSymbol(
                        stmt.token.value,
                        params,
                        ret_type,
                        function_scope)

                    self._curr_scope.insert(function_symbol)

            except SemanticError as e:
                display_error(e.msg)

        for stmt in node.stmts:
            try:
                self._visit(stmt)
            except SemanticError as e:
                display_error(e.msg)

        self._curr_scope = curr_scope

    def visit_jump_stmt(self, node):
        """ Visit a jump statement node, and analyse it.

        Check if jump is inside a loop.
        Arg:
            node (AST): node to visit in the AST.
        """

        if not self._inside_loop:
            self._error(node.token, "Use of '{}' outside loop".format(
                node.token.value
            ))

    def visit_return_stmt(self, node):
        """ Visit a return statement node, and analyse it.

        Check if return is inside a function.
        Arg:
            node (AST): node to visit in the AST.
        """

        if self._curr_function_symbol is None:
            self._error(node.token, "Use of '{}' outside function".format(
                node.token.value
            ))

        if node.expr is not None:
            self._visit(node.expr)

            if node.expr.data_type != self._curr_function_symbol.ret_type:
                self._error(node.token, f"TODO - Incorrect return type")

    def visit_while_stmt(self, node):
        """ Visit a while statement, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        self._visit(node.cond)
        if node.cond.data_type != "Bool":
            self._error(node.token, "Condition should evaluate to Bool")
        self._inside_loop = True
        self._visit(node.body)
        self._inside_loop = False

    def visit_if_stmt(self, node):
        """ Visit an if statement node, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        self._visit(node.cond)
        if node.cond.data_type != "Bool":
            self._error(node.token, "Condition should evaluate to Bool")

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

        for var in node.variables:
            self._visit(var)

    def visit_var_decl(self, node):
        """ Visit a variable declaration node, and analyse it.

        Check variables are not declared before, Add the declared variables in
        the symbol table.
        Arg:
            node (AST): node to visit in the AST.
        """

        if self._curr_scope.lookup(
                node.token.value, curr_scope_only=True) is not None:

            self._error(
                node.token,
                "Variable '{}' declared more than once in same scope".format(
                    node.token.value))

        if node.data_type is not None:
            data_type_symbol = self._curr_scope.lookup(node.data_type.value)
            if not isinstance(data_type_symbol, TypeSymbol):
                self._error(
                    node.data_type,
                    "Unknown data type '{}'".format(node.data_type.value))

        if node.init is not None:
            self._visit(node.init)
            if node.data_type is not None:
                if (node.data_type.value == "Real" and
                        node.init.data_type == "Int"):
                    pass
                elif node.data_type.value != node.init.data_type:
                    self._error(
                        node.token, "TODO - Assign requires compatible types")
            else:
                node.data_type = node.init.data_type

        variable_symbol = VariableSymbol(
            node.token.value,
            node.data_type.value)
        self._curr_scope.insert(variable_symbol)

    def visit_function_stmt(self, node):
        """ Visit a function statement, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        curr_scope = self._curr_scope
        self._curr_function_symbol = self._curr_scope.lookup(node.token.value)
        self._curr_scope = self._curr_function_symbol.scope
        self._visit(node.body)
        self._curr_function_symbol = None
        self._curr_scope = curr_scope

    # def visit_type_stmt(self, node):
    #       TODO
    #     """ Visit a type statement, and analyse it.

    #     Arg:
    #         node (AST): node to visit in the AST.
    #     """

    def visit_binary_expr(self, node):
        """ Visit a binary expression, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        operation_type = node.token.token_type
        if operation_type == TokenType.ASSIGN:
            if not isinstance(node.left, VariableExpr):
                self._error(node.token, "TODO - LHS is not a variable")

            self._visit(node.right)
            right_type = node.right.data_type
            self._visit(node.left)
            left_type = node.left.data_type

            if left_type == "Real":
                if right_type != "Int" and right_type != "Real":
                    self._error(
                        node.token, "TODO - Assign requires compatible types")
                else:
                    node.data_type = "Real"
            elif left_type != right_type:
                self._error(
                        node.token, "TODO - Assign requires compatible types")
            else:
                node.data_type = left_type

            return

        self._visit(node.left)
        left_type = node.left.data_type
        self._visit(node.right)
        right_type = node.right.data_type

        if operation_type == TokenType.ADD:

            if left_type == "String" or right_type == "String":
                node.data_type = "String"
            elif left_type != "Int" and left_type != "Real":
                self._error(node.token, "TODO - Need Int or Real or String")
            elif right_type != "Int" and right_type != "Real":
                self._error(node.token, "TODO - Need Int or Real or String")
            elif left_type == "Real" or right_type == "Real":
                node.data_type = "Real"
            else:
                node.data_type = "Int"

        elif (operation_type == TokenType.SUB or
                operation_type == TokenType.MUL or
                operation_type == TokenType.DIV or
                operation_type == TokenType.EXP):

            if left_type != "Int" and left_type != "Real":
                self._error(node.token, "TODO - Need Int or Real or String")
            elif right_type != "Int" and right_type != "Real":
                self._error(node.token, "TODO - Need Int or Real or String")
            elif left_type == "Real" or right_type == "Real":
                node.data_type = "Real"
            else:
                node.data_type = "Int"

        elif (operation_type == TokenType.MOD or
                operation_type == TokenType.BIT_AND or
                operation_type == TokenType.BIT_OR or
                operation_type == TokenType.BIT_XOR or
                operation_type == TokenType.BIT_LEFT or
                operation_type == TokenType.BIT_RIGHT):

            if left_type != "Int" or right_type != "Int":
                self._error(node.token, "TODO - Need Int")
            else:
                node.data_type = "Int"

        elif operation_type == TokenType.AND or operation_type == TokenType.OR:
            if left_type != "Bool" or right_type != "Bool":
                self._error(
                    node.token, "TODO - Logical Operations on non-booleans.")
            else:
                node.data_type = "Bool"

        else:
            # Conditional Operator - ==, !=, >, >=, <, <=
            if (operation_type != TokenType.EQUAL and
                    operation_type != TokenType.NOT_EQUAL):

                if left_type == "Bool" or right_type == "Bool":
                    self._error(node.token, "TODO - Booleans are not ordered")

            if left_type != right_type:
                self._error(node.token, "TODO - Compare requires same types")
            else:
                node.data_type = "Bool"

    def visit_unary_expr(self, node):
        """ Visit unary expr, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        operation_type = node.token.token_type

        self._visit(node.right)
        operand_type = node.right.data_type

        if operation_type == TokenType.SUB:
            if operand_type != "Int" and operand_type != "Real":
                self._error(node.token, "TODO - Need Int or Real")

        if operation_type == TokenType.BIT_NOT:
            if operand_type != "Int":
                self._error(node.token, "TODO - Need Int")

        if operation_type == TokenType.NOT:
            if operand_type != "Bool":
                self._error(node.token, "TODO - Need Boolean")

        node.data_type = operand_type

    def visit_grouped_expr(self, node):
        """ Visit a grouped expression, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        self._visit(node.expr)
        node.type = node.expr.data_type

    def visit_literal_expr(self, node):
        """ Visit literal expr, and analyse it.

        Arg:
            node (AST): node to visit in the AST.
        """

        node.data_type = primitives.get_literal_type[node.token.token_type]

    def visit_variable_expr(self, node):
        """ Visit a variable expression, and analyse it.

        Check if the variable belongs to the current scope.

        Arg:
            node (AST): node to visit in the AST.
        """

        var_symbol = self._curr_scope.lookup(node.token.value)
        if var_symbol is None:
            self._error(node.token, f"Undeclared variable '{node.value}'")

        node.data_type = var_symbol.data_type

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

        function_scope = function_symbol.scope

        for idx, arg in enumerate(node.args):
            self._visit(arg)

            if function_symbol.params[idx].data_type != arg.data_type:
                self._error(
                    node.token, f"TODO - Arg[{idx+1}] is of wrong data-type.")

        node.data_type = function_symbol.ret_type
