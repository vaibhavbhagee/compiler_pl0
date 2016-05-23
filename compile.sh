#!/bin/bash

sml parse_tree_gen.sml "lex.txt" "parse_tree.txt" "symtable.txt"
sml as_tree_gen.sml "parse_tree.txt" "as_tree.txt"
sml code_gen.sml "as_tree.txt" "symtable.txt" "ir_code.txt" "code_output.txt"