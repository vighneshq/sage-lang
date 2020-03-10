import pytest

from sage.lexer.lexer import Lexer
from sage.parser.parser import Parser
from sage.error.semantic_error import SemanticError
from sage.semantics.semantic_analyzer import SemanticAnalyzer
from sage.semantics.symbols import (TypeSymbol, VariableSymbol,
                                    FunctionSymbol, SymbolTable)


class TestSymbolTable:

    def test_lookup_curr_scope(self):

        enclosing_scope = SymbolTable()
        enclosing_scope.insert(VariableSymbol("min", "Real"))

        nested_scope = SymbolTable(enclosing_scope)

        assert enclosing_scope.lookup("min", curr_scope_only=True) \
            is not None
        assert nested_scope.lookup("Int", curr_scope_only=True) is not None
        assert nested_scope.lookup("min", curr_scope_only=True) is None

    def test_nested_lookups(self):

        enclosing_scope = SymbolTable()
        enclosing_scope.insert(VariableSymbol("min", "Real"))

        nested_scope_1 = SymbolTable(enclosing_scope)
        nested_scope_1.insert(VariableSymbol("max", "Real"))

        nested_scope_2 = SymbolTable(nested_scope_1)
        assert nested_scope_2.lookup("max") is not None

        nested_scope_2.insert(TypeSymbol("max"))
        max_symbol = nested_scope_2.lookup("max")
        assert max_symbol is not None
        assert isinstance(max_symbol, TypeSymbol)

    def test_builtins(self):
        scope = SymbolTable()
        builtins = [
            {"name": "Bool", "symbol_type": TypeSymbol},
            {"name": "Char", "symbol_type": TypeSymbol},
            {"name": "Int", "symbol_type": TypeSymbol},
            {"name": "Real", "symbol_type": TypeSymbol},
            {"name": "String", "symbol_type": TypeSymbol},
            {"name": "None", "symbol_type": TypeSymbol},
        ]

        for builtin in builtins:
            builtin_symbol = scope.lookup(
                builtin["name"],
                curr_scope_only=True)
            assert builtin_symbol is not None
            assert isinstance(builtin_symbol, builtin["symbol_type"])


class TestSemanticAnalyzer:

    def _check_error(self, semantic_analyzer, prog, msg):
        with pytest.raises(SemanticError) as exc_info:
            semantic_analyzer.check(prog)

        assert semantic_analyzer.had_error
        assert msg in str(exc_info.value)

    def test_variable_declared_before_use(self):

        source = """
        function main() {
            let x : Int = 0;
            y = x + 5;
        }
        """

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()
        assert not parser.had_error

        semantic_analyzer = SemanticAnalyzer(debug=True)

        self._check_error(semantic_analyzer, prog, "Undeclared variable")

    def test_variable_declared_more_than_once(self):

        source = """
        function main() {
            let x: Int = 0, y:Int;

            let y: Real = x / 2;
        }
        """

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()
        assert not parser.had_error

        semantic_analyzer = SemanticAnalyzer(debug=True)

        self._check_error(
            semantic_analyzer, prog,
            "declared more than once")

    def test_variable_hidden_in_nested_scope(self):

        source = """
        function main() {
            let x: Int = 0, y: Int = 5;

            if y > 10 {
                let x : String = "Hello, World!";
            }
        }
        """

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()
        assert not parser.had_error

        semantic_analyzer = SemanticAnalyzer(debug=True)
        semantic_analyzer.check(prog)

        assert not semantic_analyzer.had_error

    def test_function_declared_after_use(self):

        source = """
        function main() {
            let x: Int = gcd(10, 5);
        }

        function gcd(a: Int, b: Int) -> Int {
            if b == 0 {
                return a;
            }

            return gcd(b, a % b);
        }
        """

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()
        assert not parser.had_error

        semantic_analyzer = SemanticAnalyzer(debug=True)
        semantic_analyzer.check(prog)

        assert not semantic_analyzer.had_error
