import argparse
import sys

from sage.lexer.lexer import Lexer
from sage.parser.parser import Parser
from sage.semantics.semantic_analyzer import SemanticAnalyzer
from sage.util.util import display_error


if __name__ == "__main__":

    cl_parser = argparse.ArgumentParser()
    cl_parser.add_argument("file", type=str, help=".sg file with source code")

    args = cl_parser.parse_args()

    sage_file = args.file
    if not sage_file.endswith(".sg"):
        display_error("FileError - Expect sage file with .sg extension.")

    try:
        source = None
        with open(sage_file, "r") as source_file:
            source = source_file.read()

        lexer = Lexer(source)
        parser = Parser(lexer)

        prog = parser.parse()
        if parser.had_error:
            sys.exit(1)

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.check(prog)
        if semantic_analyzer.had_error:
            sys.exit(1)

    except FileNotFoundError as e:
        display_error("FileError - Could not find expected file.")
        sys.exit(1)
