import pytest

from sage.tokens.tokens import TokenType
from sage.ast.stmt import ExprStmt, FunctionStmt
from sage.ast.expr import (LiteralExpr, UnaryExpr, BinaryExpr, GroupedExpr,
                           VariableExpr, CallExpr)
from sage.lexer.lexer import Lexer
from sage.parser.parser import Parser


class TestParser:

    def _test_literal_or_variable(self, node, lexeme, klass=None):
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
            if literal[0] == "'" or literal [0] == '"':
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
            self._test_literal_or_variable(
                stmt.expr,
                expected_literals[idx],
                LiteralExpr)

    def _test_unary(self, node, unary_dict):
        assert isinstance(node, UnaryExpr)

        assert unary_dict["op"] == node.token.value
        assert TokenType(unary_dict["op"]) == node.token.token_type
        self._test_literal_or_variable(
            node.right,
            unary_dict["right"],
            LiteralExpr)

    def test_unary(self):
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

    def _test_binary(self, node, binary_dict):
        assert isinstance(node, BinaryExpr)

        self._test_literal_or_variable(
            node.left,
            binary_dict["left"],
            LiteralExpr)
        assert binary_dict["op"] == node.token.value
        assert TokenType(binary_dict["op"]) == node.token.token_type
        self._test_literal_or_variable(
            node.right,
            binary_dict["right"],
            LiteralExpr)

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
            self._test_literal_or_variable(
                stmt.expr,
                expected_variables[idx],
                VariableExpr)

    def _test_call(self, node, call_dict):

        assert isinstance(node, CallExpr)
        assert call_dict["name"] == node.token.value

        assert len(node.args) == len(call_dict["args"])
        for idx, arg in enumerate(call_dict["args"]):
            if arg[0] == '"':
                arg = arg[1:-1]
            self._test_literal_or_variable(node.args[idx], arg)

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
