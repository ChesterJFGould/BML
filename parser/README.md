# Description
A Pratt Parser based on the design detailed in [this paper](https://tdop.github.io) (Pratt, 1978).
As with most recursive descent parsers each production rule of the grammar is embodied in a function.
It takes in a text encoded token stream from stdin (like the one produces by the lexer) or from the
files passed as arguments and converts it into the AST.
