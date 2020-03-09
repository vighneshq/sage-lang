from abc import ABC, abstractmethod
import sys

import colorama
from termcolor import cprint


colorama.init(autoreset=True)


def display_error(msg):
    """ Prints the error message to the screen using coloured input.

    The error message is printed in red to standard error.
    """

    cprint(msg, "red", attrs=["dark"], file=sys.stderr)


def unpack_list(li):
    """ Retrurns a space seperated string using the elements
    of a list.

    Seperate function because python f-strings do not allow
    unpacking using the * operator.
    """

    return ", ".join(map(str, li))


class Visitor(ABC):
    """ Abstract base class for a vististor that will traverse the AST
    to perform some action. """

    @abstractmethod
    def visit_jump_stmt(self, node):
        """ Visit jump statement ast node, and perform some task depending
        on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_return_stmt(self, node):
        """ Visit expression statement ast node, and perform some task
        depending on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_block_stmt(self, node):
        """ Visit block statement ast node, and perform some task depending
        on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_function_stmt(self, node):
        """ Visit function statement ast node, and perform some task depending
        on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_while_stmt(self, node):
        """ Visit while statement ast node, and perform some task depending
        on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_if_stmt(self, node):
        """ Visit if statement ast node, and perform some task depending
        on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_let_stmt(self, node):
        """ Visit let statement ast node, and perform some task depending
        on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_expr_stmt(self, node):
        """ Visit expression statement ast node, and perform some task
        depending on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_binary_expr(self, node):
        """ Visit binary expressio ast node, and perform some task depending
        on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_unary_expr(self, node):
        """ Visit unary expression ast node, and perform some task depending
        on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_grouped_expr(self, node):
        """ Visit grouped expression ast node, and perform some task depending
        on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_literal_expr(self, node):
        """ Visit literal expression ast node, and perform some task depending
        on type of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_variable_expr(self, node):
        """ Visit variable ast node, and perform some task depending on type
        of self (type of visitor).
        """
        pass

    @abstractmethod
    def visit_call_expr(self, node):
        """ Visit function call ast node, and perform some task
        depending on type of self (type of visitor).
        """
        pass
