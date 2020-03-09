from sage.tokens.tokens import TokenType, Token, RESERVED_WORDS
from sage.lexer.lexer import Lexer
from sage.ast.ast import AST, Program
from sage.ast.expr import (BinaryExpr, UnaryExpr, LiteralExpr,
                           GroupedExpr, VariableExpr, CallExpr)
from sage.ast.stmt import (WhileStmt, JumpStmt, ReturnStmt, IfStmt,
                           ExprStmt, LetStmt, FunctionStmt)
from sage.error.syntax_error import LexerError, ParserError
from sage.util.util import display_error


class Parser:
    """ A class representing the Parser.

    The Parser converts the input token stream from the Lexer
    into an abstract syntax tree using a top-down parsing algorithm.

    Attributes:
        _lexer (Lexer): Lexical Analyser.
        _prev_token (Token): Previous token in the token stream.
        _curr_token (Token): Current token that the Parser is on.
        _peek_token (Token): Next token in the token stream.
    """

    def __init__(self, lexer):
        self._lexer = lexer
        self._prev_token = None
        self._curr_token = None

        # Initialize current token
        self._advance()

    def _advance(self):
        """ Get the next token in the token stream.

        Returns:
            Token: Token-object representing the lexeme that was
                tokenized.
        """

        self._prev_token = self._curr_token
        self._curr_token = self._lexer.get_next_token()

        return self._prev_token

    def _is_at_end(self):
        """ Returns whether the Parser has reached the end of token stream. """

        if self._curr_token.token_type == TokenType.EOF:
            return True
        return False

    def _check(self, *token_type_list):
        """ Return a boolean if the current token-type is equal to
        the possible options

        Args:
            token_type_list (str): Variable number of token-types to be matched
                against.

        Returns:
            bool: Indicating whether one of them matches the current token.
        """

        for token_type in token_type_list:
            if self._curr_token.token_type == token_type:
                return True

        return False

    def _panic(self):
        """ Restores the state of the parser so that it can continue
        parsing and check the remainder of the program for more errors.

        The Parser advances until it reaches the end or a token-class
        that can possibly begin a new statement.
        """

        self._advance()
        while not self._is_at_end():

            if self._prev_token.token_type == TokenType.SEMI_COLON:
                return

            if self._check(
                    TokenType.WHILE, TokenType.BREAK,
                    TokenType.CONTINUE, TokenType.FUNCTION):

                return
            self._advance()

    def _error(self, msg):
        """ Raises error found while parsing.

        Raises:
            ParserError Exception.
        """

        curr_token = self._curr_token
        line_info = f"Line {curr_token.line_no}, Column {curr_token.col_no}"
        if curr_token.token_type != TokenType.EOF:
            line_info = f"{line_info} near [Lexeme - {curr_token.value}] -"
        else:
            line_info = f"{line_info} -"

        formatted_msg = f"{line_info} {msg}."

        raise ParserError(formatted_msg)

    def _force_advance(self, msg, *expected_type_list):
        """ Makes the parser err if the token class is not equal to expected.

        Advances if the token classes match.

        Args:
            expected_type_list (TokenType): variable number of tokentypes
                required for the current token.
            msg (string): Error message to be displayed if the token class does
                not match.
        """

        for expected_type in expected_type_list:
            if self._curr_token.token_type == expected_type:
                return self._advance()

        self._error(msg)

    def _parse_primary_expr(self):
        """ Parses primary expression.

        For now primary expression includes literals, parenthesized expression,
        and identifiers.

        Corresponding grammar rule for parsing.
            primary_expr := literal_expr | variable_expr | grouped_expr
            literal_expr := literal
            variable_expr := identifier
            grouped_expr := "(" expr ")"
        """

        if self._check(TokenType.IDENT):
            curr_token = self._advance()

            return VariableExpr(curr_token, curr_token.value)

        if self._check(
                TokenType.TRUE, TokenType.FALSE,
                TokenType.CHAR_LIT, TokenType.INT_LIT,
                TokenType.REAL_LIT, TokenType.STRING_LIT):

            curr_token = self._advance()

            return LiteralExpr(curr_token, curr_token.value)

        if self._check(TokenType.LPAREN):
            tok = self._advance()
            expr = self._parse_expr()

            self._force_advance("Unbalanced parenthesis", TokenType.RPAREN)

            return GroupedExpr(tok, expr)

        self._error("Unexpected token")

    def _parse_function_call(self, callee):
        """ Parses a function call expression.

        Corresponding grammar rule for parsing.
            function_call_expr := primary_expr "(" arg_list ")"
            arg_list := { expr }
        """

        paren_token = self._advance()
        args = []

        while not self._check(TokenType.RPAREN):
            args.append(self._parse_expr())

            if not self._check(TokenType.COMMA):
                break
            self._advance()

        self._force_advance("Expect ')' after function arguments", TokenType.RPAREN)

        return CallExpr(callee, paren_token, args)

    def _parse_call_expr(self):
        """ Parses call-expressions such as function-calls.

        Corresponding grammar rule for parsing.
            call_expr := primary_expr | function_call_expr

        # TODO - Add other types of calls (or nested calls) when more
            data-structures/features are added.
        """

        expr = self._parse_primary_expr()

        if self._check(TokenType.LPAREN):
            expr = self._parse_function_call(expr)

        return expr

    def _parse_exponent_expr(self):
        """ Parses exponentiation expression.

        Corresponding grammar rule for parsing.
            exp_expr := call_expr [ "**" exp_expr ]
        """

        expr = self._parse_call_expr()
        if self._check(TokenType.EXP):
            op = self._advance()

            right = self._parse_exponent_expr()
            expr = BinaryExpr(expr, op, right)

        return expr

    def _parse_unary_expr(self):
        """ Parse unary expression.

        Corresponding grammar rule for parsing.
            unary_op := "-" | "~"
            unary_expr := unary_op unary_expr | exp_expr
        """

        if self._check(TokenType.SUB, TokenType.BIT_NOT):
            op = self._advance()

            right = self._parse_unary_expr()

            return UnaryExpr(op, right)

        return self._parse_exponent_expr()

    def _parse_mul_expr(self):
        """ Parses binary expressions with operators having
        same precedence as multiplication.

        Corresponding grammar rule for parsing.
            mul_op := "*" | "/" | "%" | ">>" | "<<"
            mul_expr := unary_expr { mul_op unary_expr }
        Operators - *, /, %
        """

        expr = self._parse_unary_expr()
        while self._check(
                TokenType.MUL, TokenType.DIV, TokenType.MOD,
                TokenType.BIT_LEFT, TokenType.BIT_RIGHT):

            op = self._advance()

            right = self._parse_unary_expr()
            expr = BinaryExpr(expr, op, right)

        return expr

    def _parse_add_expr(self):
        """ Parses binary expressions with operators having
        same precedence as addition.

        Corresponding grammar rule for parsing.
            add_op := "+" | "-" | "|" | "^"
            add_expr := mul_expr { add_op mul_expr }
        """

        expr = self._parse_mul_expr()
        while self._check(
                TokenType.ADD, TokenType.SUB,
                TokenType.BIT_OR, TokenType.BIT_XOR):

            op = self._advance()

            right = self._parse_mul_expr()
            expr = BinaryExpr(expr, op, right)

        return expr

    def _parse_conditional_expr(self):
        """ Parses a conditional expression.

        Corresponding grammar rule for parsing.
            cond_op := "==" | "!=" | ">" | ">=" | "<" | "<="
            cond_expr := add_expr { cond_op add_expr }
        """

        expr = self._parse_add_expr()
        while self._check(
                TokenType.EQUAL, TokenType.NOT_EQUAL,
                TokenType.GREATER, TokenType.GREATER_EQUAL,
                TokenType.LESSER, TokenType.LESSER_EQUAL):

            op = self._advance()

            right = self._parse_add_expr()
            expr = BinaryExpr(expr, op, right)

        return expr

    def _parse_not_expr(self):
        """ Parses a unary not expression.

        Corresponding grammar rule for parsing.
            not_expr := "not" not_expr | cond_expr
        """

        if self._check(TokenType.NOT):
            op = self._advance()

            right = self._parse_not_expr()
            return UnaryExpr(op, right)

        return self._parse_conditional_expr()

    def _parse_and_expr(self):
        """ Parses a logical and expression.

        Corresponding grammar rule for parsing.
            and_expr := not_expr { "and"  not_expr }
        """

        expr = self._parse_not_expr()
        while self._check(TokenType.AND):
            op = self._advance()

            right = self._parse_not_expr()
            expr = BinaryExpr(expr, op, right)

        return expr

    def _parse_or_expr(self):
        """ Parses a logical or expression.

        Corresponding grammar rule for parsing.
            or_expr := and_expr { "or"  and_expr }
        """

        expr = self._parse_and_expr()
        while self._check(TokenType.OR):
            op = self._advance()

            right = self._parse_and_expr()
            expr = BinaryExpr(expr, op, right)

        return expr

    def _parse_expr(self):
        """ Parses an expression and returns the corresponding ast node.

        Corresponding grammar rule for parsing.
            expr := or_expr

        Returns:
            AST: Root node of the expression
        """

        return self._parse_or_expr()

    def _parse_jump_stmt(self):
        """ Parses a continue, break statement.

        Corresponding grammar rule for parsing.
            jump_stmt := break_stmt | continue_stmt
            break_stmt := "break" ";"
            continue_stmt := "continue" ";"
        """

        jump_token = self._advance()
        self._force_advance("Expect ';' after {}".format(jump_token.value), TokenType.SEMI_COLON)

        return JumpStmt(jump_token)

    def _parse_return_stmt(self):
        """ Parses a return statement.

        Corresponding grammar rule for parsing.
            return_stmt := "return" [ expr ] ";"
        """

        return_token = self._advance()

        expr = None
        if not self._check(TokenType.SEMI_COLON):
            expr = self._parse_expr()

        self._force_advance("Expect ';' after return statement", TokenType.SEMI_COLON)

        return ReturnStmt(return_token, expr)

    def _parse_while_stmt(self):
        """ Parses a while statement.

        Corresponding grammar rule for parsing.
            while_stmt := "while" expr "{" stmt_list  "}"
            stmt_list := { stmt }
        """

        while_token = self._advance()
        cond = self._parse_expr()
        body = []

        self._force_advance("Expect '{' after while condition", TokenType.LBRACE)

        while not self._is_at_end() and not self._check(TokenType.RBRACE):
            body.append(self._parse_stmt())

        self._force_advance("Expected '}' to finish while block", TokenType.RBRACE)

        return WhileStmt(while_token, cond, body)

    def _parse_if_stmt(self):
        """ Parses an if statement.

        Corresponding grammar rule for parsing.
            # TODO - Add rule here.
        """

        if_token = self._advance()
        cond = self._parse_expr()

        then = []
        els = []

        self._force_advance("Expect '{' after if condition", TokenType.LBRACE)
        while not self._is_at_end() and not self._check(TokenType.RBRACE):
            then.append(self._parse_stmt())

        self._force_advance("Expect '}' after if block", TokenType.RBRACE)

        if self._check(TokenType.ELSE):
            self._advance()

            if self._check(TokenType.IF):
                els.append(self._parse_stmt())
                return IfStmt(if_token, cond, then, els)

            self._force_advance("Expect '{' after else", TokenType.LBRACE)

            while not self._is_at_end() and not self._check(TokenType.RBRACE):
                els.append(self._parse_stmt())

            self._force_advance("Expect '}' after else block", TokenType.RBRACE)

        return IfStmt(if_token, cond, then, els)

    def _parse_let_stmt(self):
        """ Parse function for variable declarations.

        Corresponding grammar rule for parsing.
            let_stmt := "let" var_list ";"
            var_list := identifier [ identifier ] [ = expr ] { "," var_list }
        """

        let_token = self._advance()
        var_list = []

        while not self._check(TokenType.SEMI_COLON):
            var_name = self._force_advance("Expect variable name", TokenType.IDENT)
            var_type = None
            var_init = None

            if not self._check(TokenType.ASSIGN):
                self._force_advance("Expect colon before data type", TokenType.COLON)
                var_type = self._force_advance(
                    "Expect variable's data type",
                    TokenType.BOOL, TokenType.CHAR, TokenType.INT,
                    TokenType.REAL, TokenType.STRING,
                    TokenType.IDENT)

            if self._check(TokenType.ASSIGN):
                self._advance()
                var_init = self._parse_expr()

            var = {"type": var_type, "name": var_name, "init": var_init}
            var_list.append(var)

            if not self._check(TokenType.COMMA):
                break
            self._advance()

        self._force_advance("Expect ';' after let statement", TokenType.SEMI_COLON)
        return LetStmt(let_token, var_list)

    def _parse_function_stmt(self):
        """ Parse function declarations.

        Corresponding grammar rule for parsing.
            function_stmt := "function" identifier "(" [ param_list ] ")"
                [ "->" identifier ] "{" stmt_list "}"
            param_list := param { "," param }
            param := identifer identifier
            stmt_list := { stmt }
        """

        self._advance()
        name_token = self._force_advance("Expect function name", TokenType.IDENT)

        params = []
        self._force_advance("Expect '(' before function parameters", TokenType.LPAREN)
        while not self._check(TokenType.RPAREN):
            param_name = self._force_advance("Expect parameter name", TokenType.IDENT)
            self._force_advance("Expect colon before data type", TokenType.COLON)
            param_type = self._force_advance(
                "Expect parameter's data type",
                TokenType.BOOL, TokenType.CHAR, TokenType.INT, TokenType.REAL,
                TokenType.STRING, TokenType.IDENT)

            param = {"type": param_type, "name": param_name}
            params.append(param)

            if not self._check(TokenType.COMMA):
                break
            self._advance()

        self._force_advance("Expect ')' after function parameters", TokenType.RPAREN)

        ret_type = None
        if self._check(TokenType.ARROW):
            self._advance()
            ret_type = self._force_advance(
                "Expect parameter's data type",
                TokenType.BOOL, TokenType.CHAR, TokenType.INT, TokenType.REAL,
                TokenType.STRING, TokenType.IDENT)

        body = []
        self._force_advance("Expect '{' before function body", TokenType.LBRACE)
        while not self._is_at_end() and not self._check(TokenType.RBRACE):
            body.append(self._parse_stmt())

        self._force_advance("Expect '}' after function body", TokenType.RBRACE)

        return FunctionStmt(name_token, params, ret_type, body)

    def _parse_expr_stmt(self):
        """ Parses expression statements such as function calls.

        More expression statements will probably be added.

        Corresponding grammar rule for parsing.
            # TODO - Add rule here.
        """

        expr = self._parse_expr()
        self._force_advance("Expect ';' after expression", TokenType.SEMI_COLON)

        return ExprStmt(expr)

    def _parse_stmt(self):
        """ Parses a statement and returns the corresponding ast node.

        Corresponding grammar rule for parsing.
            stmt := while_stmt | if_stmt | jump_stmt | return_stmt
                | expr_stmt | let_stmt

        Returns:
            AST: Root node of the expression
        """

        if self._check(TokenType.WHILE):
            return self._parse_while_stmt()
        if self._check(TokenType.IF):
            return self._parse_if_stmt()
        if self._check(TokenType.LET):
            return self._parse_let_stmt()
        if self._check(TokenType.RETURN):
            return self._parse_return_stmt()
        if self._check(TokenType.BREAK, TokenType.CONTINUE):
            return self._parse_jump_stmt()
        return self._parse_expr_stmt()

    def _parse_top_level_stmt(self):
        """ Parses a top-level statement.

        Corresponding grammar rule for parsing.
            top_level_stmt := function_stmt | let_stmt
        """
        
        if self._check(TokenType.FUNCTION):
            return self._parse_function_stmt()
        if self._check(TokenType.LET):
            return self._parse_let_stmt()

        self._error("Unknown top-level statement.")

    def parse(self):
        """ Root function where the token stream begins to get parsed.

        Parsing is done using a simple LL(1) recursive descent parsing
        algorithm.

        Corresponding grammar rule for parsing.
            prog := { top_level_stmt }

        Returns:
            AST: Root node of the abstract syntax tree.
        """

        stmts = []
        while not self._check(TokenType.EOF):
            try:
                stmts.append(self._parse_top_level_stmt())
            except (LexerError, ParserError) as e:
                display_error(e.msg)
                self._panic()

        return Program(stmts)
