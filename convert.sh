#! /bin/sh

# If we are given an output file, redirect stdout to that.
case $# in
	1)
		_build/default/lexer/lexer.exe $1 |\
		_build/default/parser/parser.exe |\
		_build/default/translator/translator.exe
		;;

	2)
		_build/default/lexer/lexer.exe $1 |\
		_build/default/parser/parser.exe |\
		_build/default/translator/translator.exe > $2
		;;
esac
