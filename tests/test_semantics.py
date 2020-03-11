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
            {"name": "Null", "symbol_type": TypeSymbol},
            {"name": "Void", "symbol_type": TypeSymbol},
        ]

        for builtin in builtins:
            builtin_symbol = scope.lookup(
                builtin["name"],
                curr_scope_only=True)
            assert builtin_symbol is not None
            assert isinstance(builtin_symbol, builtin["symbol_type"])


class TestSemanticAnalyzer:

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

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.check(prog)

        assert semantic_analyzer.had_error

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

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.check(prog)

        assert semantic_analyzer.had_error

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

        semantic_analyzer = SemanticAnalyzer()
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

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.check(prog)

        assert not semantic_analyzer.had_error

    def test_types_are_legal(self):

        source = """
        function main() {
            let x: Node = gcd(10, 5);
        }
        """

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()
        assert not parser.had_error

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.check(prog)

        assert semantic_analyzer.had_error

    def test_assignment_type_is_compatible(self):

        sources = [
            {"source": "let x: Int = 5.1;", "compatible": False},
            {"source": "let x: Real = \"Hello, World.\";",
                "compatible": False},
            {"source": "let c: Char = true;",
                "compatible": False},
            {"source": """ let x: Bool;
                x = 'a'; """,
                "compatible": False},
            {"source": "let x : Real = 1;", "compatible": True},
            {"source": "let x : Real, y: Int; y = x;", "compatible": False},
            {
                "source": """
                    let x:Int = func();
                }
                function func() -> Int {
                    return 3.14;
            """,
                "compatible": False
            }
        ]

        for pair in sources:
            mod_source = "function main() {\n" + pair["source"] + "\n}"
            lexer = Lexer(mod_source)
            parser = Parser(lexer)

            prog = parser.parse()
            assert not parser.had_error

            semantic_analyzer = SemanticAnalyzer()
            semantic_analyzer.check(prog)

            assert not semantic_analyzer.had_error == pair["compatible"]

    def test_operation_is_legal_for_types(self):

        sources = [
            {"source": "'a' + 1;", "legal": False},
            {"source": "\"Hello\" - 1;", "legal": False},
            {"source": "\"Hello\" * 1;", "legal": False},
            {"source": "true / 1;", "legal": False},
            {"source": "5.5 / 10;", "legal": True},
            {"source": "5.5 % 10;", "legal": False},
            {"source": "\"Hello, \" + \"World!\";", "legal": True},
            {"source": "1 & 2;", "legal": True},
            {"source": "1 | 2.5;", "legal": False},
            {"source": "4 ^ 2.5;", "legal": False},
            {"source": "4 << 2.0;", "legal": False},
            {"source": "4 >> 2.0;", "legal": False},
            {"source": "1 == \"1\";", "legal": False},
            {"source": "true != 1;", "legal": False},
            {"source": "true > 1;", "legal": False},
            {"source": "true == false;", "legal": True},
            {"source": "'a' >= 'b'';", "legal": True},
            {"source": "'a' < 'b'';", "legal": True},
            {"source": "'a' <= 'b'';", "legal": True},
            {"source": "-5.5;", "legal": True},
            {"source": "-'a';", "legal": False},
            {"source": "~10;", "legal": True},
            {"source": "~3.14;", "legal": False},
            {"source": "not 1.4;", "legal": False},
            {"source": "not true;", "legal": True},
            {"source": "false and true;", "legal": True},
            {"source": "123 or true;", "legal": False},
        ]

        for pair in sources:
            mod_source = "function main() {\n" + pair["source"] + "\n}"
            lexer = Lexer(mod_source)
            parser = Parser(lexer)

            prog = parser.parse()
            assert not parser.had_error

            semantic_analyzer = SemanticAnalyzer()
            semantic_analyzer.check(prog)

            assert not semantic_analyzer.had_error == pair["legal"]

    def test_function_returns_the_declared_type(self):

        source = """
            function main() {
            let x: Int = gcd(10, 5);
        }

        function gcd(a: Int, b: Int) -> Int {
            if b == 0 {
                return a;
            }

            return "Hello, World!;
        }
        """

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()
        assert not parser.had_error

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.check(prog)

        assert semantic_analyzer.had_error

    def test_function_parameter_types_are_correct(self):

        sources = [{
                "source": """
            function main() {
            let x: Int = gcd(10, 5);
            }

            function gcd(a: Int, b: Int) -> Int {
                if b == 0 {
                    return a;
                }

                return gcd(b, a % b);
            }
            """,
                "correct": True
            },
            {
                "source": """
            function main() {
            let x: Int = gcd(10.5, 5);
            }

            function gcd(a: Int, b: Int) -> Int {
                if b == 0 {
                    return a;
                }

                return gcd(b, a % b);
            }
            """,
                "correct": False,
            }
            ]

        for pair in sources:
            lexer = Lexer(pair["source"])
            parser = Parser(lexer)

            prog = parser.parse()
            assert not parser.had_error

            semantic_analyzer = SemanticAnalyzer()
            semantic_analyzer.check(prog)

            assert not semantic_analyzer.had_error == pair["correct"]
