This is the source code for the compiler of a language PL0 with the
following given EBNF specification:

************* Tokens of Language PL0 ***************

Reserved words. int, bool, tt, ff, if, then, else, while, proc, print, read.
Integer Operators.
Unary. ~
Binary. +, -, *, /, %
Boolean Operators.
Unary. !
Binary. &&,||
Relational Operators. =, <>, <, <=, >, >=
Assignment. :=
Brackets. (, ), {, }
Punctuation. ;, ,
Identifiers. (A-Za-z) (A-Za-z0-9)*
Comment. Any string of printable characters enclosed in (*, *)

************* EBNF of Language PL0 ******************

Program ::= Block .
Block ::= DeclarationSeq CommandSeq .
DeclarationSeq ::= [VarDecls] [ProcDecls] .
VarDecls ::= [IntVarDecls] [BoolVarDecls] .
IntVarDecls ::= int Ident {, Ident}; .
BoolVarDecls ::= bool Ident {, Ident}; .
ProcDecls ::= [ProcDef {;ProcDecls};] .
ProcDef ::= proc Ident Block .
CommandSeq r::= {{Command;}}.
Command r::= AssignmentCmd | CallCmd | ReadCmd | PrintCmd | ConditionalCmd | WhileCmd .
AssignmentCmd ::= Ident := Expression .
CallCmd r::= Ident .
ReadCmd ::= read( Ident ) .
PrintCmd ::=print( Ident ) .
Expression ::= IntExpression | BoolExpression .
ConditionalCmd ::= if BoolExpression then CommandSeq else CommandSeq .
WhileCmd ::= while BoolExpression CommandSeq .