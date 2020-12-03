#! /bin/sh

_build/default/lexer/lexer.exe $@ |\
_build/default/parser/parser.exe |\
_build/default/translator/translator.exe
