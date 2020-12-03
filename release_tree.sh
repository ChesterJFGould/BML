#! /bin/sh

./lexer.exe $@ |\
./parser.exe |\
./tree.exe
