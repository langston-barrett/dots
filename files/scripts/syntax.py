# https://stackoverflow.com/questions/4284313/how-can-i-check-the-syntax-of-python-script-without-executing-it

import ast, traceback
from sys import argv

with open(argv[0]) as f:
    source = f.read()
exit_code = 0
try:
    ast.parse(source)
except SyntaxError:
    exit_code = 1
    traceback.print_exc()
exit(exit_code)
