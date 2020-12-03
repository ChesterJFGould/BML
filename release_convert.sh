#! /bin/sh

# If we are given an output file, redirect stdout to that.
case $# in
	1)
		./lexer.exe $1 |\
		./parser.exe |\
		./translator.exe
		;;

	2)
		./lexer.exe $1 |\
		./parser.exe |\
		./translator.exe > $2
		;;
esac
