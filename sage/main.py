import argparse

from sage.tokens.tokens import TokenType, Token
from sage.lexer.lexer import Lexer


if __name__ == "__main__":

    src = '''x = y + 1\nInt Real\n"Hello \'x\''''

    l = Lexer(src)

    while True:

        try:
            tok = l.get_next_token()
            print(tok)
            if tok.token_type == TokenType.EOF:
                break
        except Exception as e:
            print(e.msg)

