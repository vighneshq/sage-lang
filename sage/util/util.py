import sys

import colorama


colorama.init(autoreset=True)

def display_error(msg):

    print(
        colorama.Style.BRIGHT + colorama.Fore.RED + colorama.Back.WHITE + msg,
        file=sys.stderr)