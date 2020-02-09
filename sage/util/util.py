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
