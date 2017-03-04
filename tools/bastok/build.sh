#!/bin/bash

lex bastok.l
gcc -o bastok lex.yy.c -lfl
rm -f lex.yy.c
