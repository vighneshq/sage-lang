import argparse

from sage.tokens.tokens import TokenType, Token
from sage.lexer.lexer import Lexer
from sage.parser.parser import Parser
from sage.semantics.semantic_analyzer import SemanticAnalyzer

if __name__ == "__main__":

    src = '''
    function count(w: Int, v: Int)
    {
        return w + v;
    }

    let z : Int;
    function main()
    {
        let x:Int, y:Int;
        x = y = z;
        let z: Int;

        while x + 5 < 10 {
            count(5, 10.5, 11);
            y + 10;
            12*x;
            continue;
        }

        break;

        if x < 5 {
            x + 5;
            y * 10;
        } else if b < 10 {
            a + 5;
            b * 10;
        }
        1 + (2+3);
        while x < 10 {
            return 5;
        }
        min(a, b + 10);
    }
    function min(q: Int, p: Int)
    {
        if p < q {
            return p;
        }
        return q;
    }
    '''

    lexer = Lexer(src)
    parser = Parser(lexer)

    prog = parser.parse()
    for stmt in prog.stmts:
        print(stmt.token)

    semantic_checker = SemanticAnalyzer()
    semantic_checker.check(prog)
