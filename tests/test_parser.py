import pytest

from sage.tokens.tokens import TokenType
from sage.ast.stmt import (JumpStmt, ReturnStmt, BlockStmt, WhileStmt, IfStmt,
                           ExprStmt, LetStmt, FunctionStmt)
from sage.ast.expr import (LiteralExpr, UnaryExpr, BinaryExpr, GroupedExpr,
                           VariableExpr, CallExpr)
from sage.lexer.lexer import Lexer
from sage.parser.parser import Parser


class TestParser:

    def test_jump_stmt(self):

        source = """
        function main() {
            continue;
            break;
        }
        """

        exprs = source.split("\n")[2:-2]
        exprs = [expr for expr in exprs]

        expected_jumps = []
        for expr in exprs:
            lexeme = expr[:-1].strip()
            expected_jumps.append(lexeme)

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == len(expected_jumps)

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, JumpStmt)
            assert expected_jumps[idx] == stmt.token.value

    def test_return_stmt(self):

        source = """
        function main() {
            return 5;
            return 420.69;
        }
        """

        exprs = source.split("\n")[2:-2]
        exprs = [expr for expr in exprs]

        expected_returns = []
        for expr in exprs:
            token, expr = expr[:-1].split()
            expected_returns.append({
                "token": token,
                "expr": expr
            })

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == len(expected_returns)

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, ReturnStmt)
            assert expected_returns[idx]["token"] == stmt.token.value
            self._test_ast_node(
                stmt.expr,
                expected_returns[idx]["expr"],
                LiteralExpr)

    def test_while_stmt(self):

        source = """
        function main() {
            while x < 5 {
                x + y;
            }
        }
        """

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == 1

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, WhileStmt)
            assert TokenType.WHILE == stmt.token.token_type

            self._test_binary(stmt.cond, {
                "left": "x",
                "op": "<",
                "right": "5"
            })

            assert isinstance(stmt.body, BlockStmt)
            assert len(stmt.body.stmts) == 1
            assert isinstance(stmt.body.stmts[0], ExprStmt)

            self._test_binary(stmt.body.stmts[0].expr, {
                "left": "x",
                "op": "+",
                "right": "y"
            })

    def test_matched_if(self):

        source = """
        function main() {
            if a > b {
                return a;
            }
            else {
                return b;
            }
        }
        """

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == 1

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, IfStmt)
            assert TokenType.IF == stmt.token.token_type

            self._test_binary(stmt.cond, {
                "left": "a",
                "op": ">",
                "right": "b"
            })

            assert len(stmt.then.stmts) == 1
            assert isinstance(stmt.then.stmts[0], ReturnStmt)

            assert "return" == stmt.then.stmts[0].token.value
            assert TokenType.RETURN == stmt.then.stmts[0].token.token_type
            self._test_ast_node(stmt.then.stmts[0].expr, "a")

            assert stmt.els is not None
            assert isinstance(stmt.els, BlockStmt)
            assert len(stmt.els.stmts) == 1
            assert isinstance(stmt.els.stmts[0], ReturnStmt)

            assert "return" == stmt.els.stmts[0].token.value
            assert TokenType.RETURN == stmt.els.stmts[0].token.token_type
            self._test_ast_node(stmt.els.stmts[0].expr, "b")

    def test_unmatched_if(self):

        source = """
        function main() {
            if a > b {
                return a;
            }
        }
        """

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == 1

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, IfStmt)
            assert TokenType.IF == stmt.token.token_type

            self._test_binary(stmt.cond, {
                "left": "a",
                "op": ">",
                "right": "b"
            })

            assert len(stmt.then.stmts) == 1
            assert isinstance(stmt.then.stmts[0], ReturnStmt)

            assert "return" == stmt.then.stmts[0].token.value
            assert TokenType.RETURN == stmt.then.stmts[0].token.token_type
            self._test_ast_node(stmt.then.stmts[0].expr, "a")

            assert stmt.els is None

    def test_nested_if(self):

        source = """
        function main() {
            if a > b {
                if c != 0 {
                    return c;
                }
                else {
                    return c + 1;
                }
            }
        }
        """

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == 1

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, IfStmt)
            assert TokenType.IF == stmt.token.token_type

            self._test_binary(stmt.cond, {
                "left": "a",
                "op": ">",
                "right": "b"
            }, VariableExpr)

            assert len(stmt.then.stmts) == 1
            assert isinstance(stmt.then.stmts[0], IfStmt)
            assert stmt.els is None

            nested_if = stmt.then.stmts[0]

            assert "if" == nested_if.token.value
            assert TokenType.IF == nested_if.token.token_type
            assert nested_if.els is not None

            self._test_binary(nested_if.cond, {
                "left": "c",
                "op": "!=",
                "right": "0"
            })

    def test_let_with_type_without_init(self):

        source = """
        let x: Int, y : Int;
        """

        stmts = source.split("\n")[1:-1]
        stmts = [stmt.strip()[3:-1] for stmt in stmts]

        expected_decls = []
        for stmt in stmts:

            decls = stmt.split(", ")
            for decl in decls:
                name, var_type = decl.split(":")
                expected_decls.append({
                    "name": name.strip(),
                    "type": var_type.strip(),
                })

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], LetStmt)

        var_list = prog.stmts[0].var_list
        assert len(var_list) == 2
        assert len(expected_decls) == len(var_list)

        for idx, var in enumerate(var_list):
            self._test_ast_node(var["name"], expected_decls[idx]["name"])
            self._test_ast_node(var["type"], expected_decls[idx]["type"])

    def test_let_without_type_with_init(self):

        source = """
        let x = 1 + 2, y = a * b;
        """

        stmts = source.split("\n")[1:-1]
        stmts = [stmt.strip()[3:-1] for stmt in stmts]

        expected_decls = []
        for stmt in stmts:

            decls = stmt.split(", ")
            for decl in decls:
                name, expr = decl.split("=")
                left, op, right = expr.split()
                expected_decls.append({
                    "name": name.strip(),
                    "init": {
                        "left": left,
                        "op": op,
                        "right": right
                    }
                })

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], LetStmt)

        var_list = prog.stmts[0].var_list
        assert len(var_list) == 2
        assert len(expected_decls) == len(var_list)

        for idx, var in enumerate(var_list):
            self._test_ast_node(var["name"], expected_decls[idx]["name"])
            self._test_binary(var["init"], expected_decls[idx]["init"])

    def test_let_with_type_with_init(self):

        source = """
        let x : Int = 1 + 2, y : Int= a * b;
        """

        stmts = source.split("\n")[1:-1]
        stmts = [stmt.strip()[3:-1] for stmt in stmts]

        expected_decls = []
        for stmt in stmts:

            decls = stmt.split(", ")
            for decl in decls:
                name, tail = decl.split(":")
                var_type, expr = tail.split("=")
                left, op, right = expr.split()
                expected_decls.append({
                    "name": name.strip(),
                    "type": var_type.strip(),
                    "init": {
                        "left": left,
                        "op": op,
                        "right": right
                    }
                })

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], LetStmt)

        var_list = prog.stmts[0].var_list
        assert len(var_list) == 2
        assert len(expected_decls) == len(var_list)

        for idx, var in enumerate(var_list):
            self._test_ast_node(var["name"], expected_decls[idx]["name"])
            self._test_ast_node(var["type"], expected_decls[idx]["type"])
            self._test_binary(var["init"], expected_decls[idx]["init"])

    def test_function_stmt_with_type(self):

        source = """
        function gcd(a: Int, b: Int) -> Int{
            if b == 0 {
                return a;
            }

            return gcd(b, a%b);
        }
        """

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)
        self._test_ast_node(prog.stmts[0].token, "gcd")

        function = prog.stmts[0]
        assert len(function.params) == 2

        self._test_ast_node(function.params[0].token, "a")
        self._test_ast_node(function.params[0].data_type, "Int")

        self._test_ast_node(function.params[1].token, "b")
        self._test_ast_node(function.params[1].data_type, "Int")

        self._test_ast_node(function.ret_type, "Int")

        assert isinstance(function.body, BlockStmt)
        assert len(function.body.stmts) == 2

        assert isinstance(function.body.stmts[0], IfStmt)
        assert isinstance(function.body.stmts[1], ReturnStmt)

    def _test_ast_node(self, node, lexeme, klass=None):
        if klass is not None:
            assert isinstance(node, klass)
        assert lexeme == node.value

    def test_literal_expr(self):

        source = """
        function main() {
            true;
            false;
            10;
            1.5;
            'a';
            "Goodbye, everybody!";
        }
        """

        exprs = source.split("\n")[2:-2]
        exprs = [expr for expr in exprs]

        expected_literals = []
        for expr in exprs:
            literal = expr[:-1].strip()
            if literal[0] == "'" or literal[0] == '"':
                literal = literal[1:-1]
            expected_literals.append(literal)

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == len(expected_literals)

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, ExprStmt)
            self._test_ast_node(
                stmt.expr,
                expected_literals[idx],
                LiteralExpr)

    def _test_unary(self, node, unary_dict):
        assert isinstance(node, UnaryExpr)

        assert unary_dict["op"] == node.token.value
        assert TokenType(unary_dict["op"]) == node.token.token_type
        self._test_ast_node(
            node.right,
            unary_dict["right"],
            LiteralExpr)

    def test_unary_expr(self):
        source = """
        function main() {
            - 1;
            ~ 0;
            not true;
        }
        """

        exprs = source.split("\n")[2:-2]
        exprs = [expr for expr in exprs]

        expected_unary = []
        for expr in exprs:
            op, right = expr[:-1].split()
            expected_unary.append(
                {"op": op, "right": right},
            )

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == len(expected_unary)

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, ExprStmt)
            self._test_unary(stmt.expr, expected_unary[idx])

    def _test_binary(self, node, binary_dict, klass=None):
        assert isinstance(node, BinaryExpr)

        self._test_ast_node(
            node.left,
            binary_dict["left"],
            klass)
        self._test_ast_node(
            node.right,
            binary_dict["right"],
            klass)

        assert binary_dict["op"] == node.token.value
        assert TokenType(binary_dict["op"]) == node.token.token_type

    def test_binary_expr(self):
        source = """
        function main() {
            1 + 5;
            10 - 10;
            11 * 20;
            1 / 2;
            5 ** 3;
            10 % 2;
            1 = 2;
            1 & 0;
            0 | 1;
            1 ^ 0;
            1 << 5;
            32 >> 5;
            3.14 == 2.178;
            3.14 != 2.178;
            3.14 >  2.178;
            3.14 >= 2.178;
            3.14 <  2.178;
            3.14 <= 2.178;
        }
        """

        exprs = source.split("\n")[2:-2]
        exprs = [expr for expr in exprs]

        expected_binary = []
        for expr in exprs:
            left, op, right = expr[:-1].split()
            expected_binary.append(
                {"left": left, "op": op, "right": right},
            )

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == len(expected_binary)

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, ExprStmt)
            self._test_binary(stmt.expr, expected_binary[idx])

    def test_grouped_expr(self):
        source = """
        function main() {
            (1 + 10);
            (5.4 * 25);
        }
        """

        exprs = source.split("\n")[2:-2]
        exprs = [expr for expr in exprs]

        expected_grouped = []
        for expr in exprs:
            left, op, right = expr[:-1].split()
            left = left.split("(")[1]
            right = right.split(")")[0]

            expected_grouped.append(
                {"left": left, "op": op, "right": right},
            )

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == len(expected_grouped)

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, ExprStmt)
            assert isinstance(stmt.expr, GroupedExpr)
            self._test_binary(stmt.expr.expr, expected_grouped[idx])

    def test_variable_expr(self):

        source = """
        function main() {
            sqrt;
            request_type;
            median;
        }
        """

        exprs = source.split("\n")[2:-2]
        exprs = [expr for expr in exprs]

        expected_variables = []
        for expr in exprs:
            variable = expr[:-1].strip()
            expected_variables.append(variable)

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == len(expected_variables)

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, ExprStmt)
            self._test_ast_node(
                stmt.expr,
                expected_variables[idx],
                VariableExpr)
            assert TokenType.IDENT == stmt.expr.token.token_type

    def _test_call(self, node, call_dict):

        assert isinstance(node, CallExpr)
        assert call_dict["name"] == node.token.value

        assert len(node.args) == len(call_dict["args"])
        for idx, arg in enumerate(call_dict["args"]):
            if arg[0] == '"':
                arg = arg[1:-1]
            self._test_ast_node(node.args[idx], arg)

    def test_call_expr(self):
        source = """
        function main() {
            max(10, 5);
            modifyWindow(x, y, size);
            print("Who goes there?");
        }
        """

        exprs = source.split("\n")[2:-2]
        exprs = [expr for expr in exprs]

        expected_calls = []
        for expr in exprs:
            call = expr[:-2].strip()
            name, args = call.split("(")
            args = args.split(", ")

            expected_calls.append({
                "name": name,
                "args": args,
            })

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()

        assert not parser.had_error

        assert len(prog.stmts) == 1
        assert isinstance(prog.stmts[0], FunctionStmt)

        body = prog.stmts[0].body.stmts
        assert len(body) == len(expected_calls)

        for idx, stmt in enumerate(body):
            assert isinstance(stmt, ExprStmt)
            self._test_call(stmt.expr, expected_calls[idx])

    def test_parser_errors(self):

        sources = [
            """expr_stmt_without_semi_colon()""",
            """ while x < 5
                x = x + 1;
            }
            """,
            """let x;""",

        ]

        for source in sources:
            mod_source = """function main() {\n""" + source + "\n}"
            lexer = Lexer(mod_source)
            parser = Parser(lexer)

            parser.parse()
            assert parser.had_error
