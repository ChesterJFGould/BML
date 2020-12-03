# Description
A Pratt Parser based on the design detailed in [this paper](https://tdop.github.io) (Pratt, 1978).
As with most recursive descent parsers each production rule of the grammar is embodied in a function.
It takes in a text encoded token stream from stdin (like the one produces by the lexer) or from the
files passed as arguments and converts it into the AST.

# Result and Exceptions
This code is unacceptably verbose as each function returns a result type instead of just an expression.
I will fix this by refactoring the code to use exceptions instead.
The prime example of this is the function `parse_if`, if exceptions were used it would be about half as long.
