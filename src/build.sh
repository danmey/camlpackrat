#!/bin/bash

ocamlopt btstream.ml peg.ml pegparser.ml helper.ml mlcodegen.ml main.ml -o ./mlp 
./mlp test.mlp > out.ml
#ocamlopt btstream.ml mlpdef.ml out.ml parse.ml -o test
rm -f *.cm*
rm -f *.o