import argparse

from sage.tokens.tokens import TokenType, Token
from sage.lexer.lexer import Lexer
from sage.parser.parser import Parser


if __name__ == "__main__":

    src = '''
    while x + 5 < 10 {
        count(5, 10.5);
        y + 10;
        12*x;
    }

    continue;

    if x < 5 {
        x + 5;
        y * 10;
    } else if b < 10 {
        a + 5;
        b * 10;
    }

    while x < 10 {
        return 5;
    }
    count(a, b
    '''

    l = Lexer(src)
    p = Parser(l)

    p.parse()
