exception IncorrectCommandInput;

val arg = CommandLine.arguments();
fun extract_filenames (a as x::y::tl) = (x,y)
	| extract_filenames _ = raise IncorrectCommandInput;

val src = #1 (extract_filenames arg);
val dst = #2 (extract_filenames arg);

Control.Print.printLength := 100; 

exception InvalidInp;

datatype TOKEN = UNMINUS of int * int
				| BINADD of int * int
				| BINSUB of int * int
				| BINDIV of int * int
				| BINMUL of int * int
				| BINMOD of int * int
				| NEG of int * int
				| AND of int * int
				| OR of int * int
				| ASSIGN of int * int
				| EQ of int * int
				| NE of int * int
				| LT of int * int
				| LTE of int * int
				| GT of int * int
				| GTE of int * int
				| LP of int * int
				| RP of int * int
				| LB of int * int
				| RB of int * int
				| EOS of int * int
				| COMMA of int * int
				| INT of int * int
				| BOOL of int * int
				| IF of int * int
				| THEN of int * int
				| ELSE of int * int
				| WHILE of int * int
				| PROC of int * int
				| PRINT of int * int
				| READ of int * int
				| ERROR of int * int * int
				| INTLIT of int * int * int
				| IDENT of int * int * string
				| BOOLVAL of int * int * bool

fun toString TOKEN =
	case TOKEN of
			  UNMINUS(a,b) => "UNMINUS(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| BINADD(a,b) => "BINADD(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| BINSUB(a,b) => "BINSUB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| BINDIV(a,b) => "BINDIV(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| BINMUL(a,b) => "BINMUL(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| BINMOD(a,b) => "BINMOD(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| NEG(a,b) => "NEG(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| AND(a,b) => "AND(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| OR(a,b) => "OR(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| ASSIGN(a,b) => "ASSIGN(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| EQ(a,b) => "EQ(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| NE(a,b) => "NE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| LT(a,b) => "LT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| LTE(a,b) => "LTE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| GT(a,b) => "GT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| GTE(a,b) => "GTE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| LP(a,b) => "LP(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| RP(a,b) => "RP(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| LB(a,b) => "LB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| RB(a,b) => "RB(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| EOS(a,b) => "EOS(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| COMMA(a,b) => "COMMA(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| INT(a,b) => "INT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| BOOL(a,b) => "BOOL(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| IF(a,b) => "IF(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| THEN(a,b) => "THEN(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| ELSE(a,b) => "ELSE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| WHILE(a,b) => "WHILE(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| PROC(a,b) => "PROC(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| PRINT(a,b) => "PRINT(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| READ(a,b) => "READ(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
			| ERROR(a,b,c)=>"ERROR(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Int.toString c ^")\n"
			| INTLIT(a,b,c)=>"INTLIT(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Int.toString c ^")\n"
			| IDENT(a,b,c)=>"IDENT(" ^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ c ^ ")\n"
			| BOOLVAL(a,b,c)=>"BOOLVAL("^ Int.toString a ^ "," ^ Int.toString b ^ "," ^ Bool.toString c ^")\n"

(*Read from the text file*)
fun getclist (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (c, f') => loop (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loop ([], f))
    end;

val char_list = getclist src;

(*Generate lexemes as list of characters*)
fun gen_lexeme (l:char list) = 
	let fun gen_char_by_char lex_list curr_lex [] row col curr_row curr_col iscomment = if curr_lex = [] then lex_list else (lex_list@[(curr_lex,curr_row,curr_col)])|
			gen_char_by_char lex_list curr_lex (h::t) row col curr_row curr_col iscomment = 
				if iscomment = false then
					if h = #" " orelse h = #"\t" then
						if curr_lex=[] then
							gen_char_by_char lex_list [] t row (col+1) curr_row curr_col iscomment
						else
							gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]) [] t row (col+1) curr_row curr_col iscomment
					else if h = #"\n" then
						if curr_lex=[] then
							gen_char_by_char lex_list [] t (row+1) 1 curr_row curr_col iscomment
						else
							gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]) [] t (row+1) 1 curr_row curr_col iscomment
					else if h = #"+" orelse h = #"~" orelse h = #"-" orelse h = #"/" orelse h = #"*" orelse h = #")" orelse h = #"!" orelse h = #"%" orelse h = #";" orelse h = #"," orelse h = #"{" orelse h = #"}" orelse h = #"=" then
						if curr_lex=[] then
							gen_char_by_char (lex_list@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
						else
							gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
					else if h = #"<" then
						case t of
						[] =>					
							if curr_lex=[] then
								gen_char_by_char (lex_list@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
							else
								gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
						|x::y =>
							if x = #">" then					
								if curr_lex=[] then
									gen_char_by_char (lex_list@[([h]@[x],row,col)]) [] y row (col+2) curr_row curr_col iscomment
								else
									gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h]@[x],row,col)]) [] y row (col+2) curr_row curr_col iscomment
							else if x = #"=" then					
								if curr_lex=[] then
									gen_char_by_char (lex_list@[([h]@[x],row,col)]) [] y row (col+2) curr_row curr_col iscomment
								else
									gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h]@[x],row,col)]) [] y row (col+2) curr_row curr_col iscomment
							else
								if curr_lex=[] then
									gen_char_by_char (lex_list@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
								else
									gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
					else if h = #">" then
						case t of
						[] =>					
							if curr_lex=[] then
								gen_char_by_char (lex_list@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
							else
								gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
						|x::y =>
							if x = #"=" then					
								if curr_lex=[] then
									gen_char_by_char (lex_list@[([h]@[x],row,col)]) [] y row (col+2) curr_row curr_col iscomment
								else
									gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h]@[x],row,col)]) [] y row (col+2) curr_row curr_col iscomment
							else
								if curr_lex=[] then
									gen_char_by_char (lex_list@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
								else
									gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
					else if h = #"&" then
						case t of
						[] =>					
							if curr_lex=[] then
								gen_char_by_char (lex_list@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
							else
								gen_char_by_char (lex_list@[(curr_lex@[h],curr_row,curr_col)]) [] t row (col+1) curr_row curr_col iscomment
						|x::y =>
							if x = #"&" then					
								if curr_lex=[] then
									gen_char_by_char (lex_list@[([h]@[x],row,col)]) [] y row (col+2) row (col+2) iscomment
								else
									gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h]@[x],row,col)]) [] y row (col+2) row (col+2) iscomment
							else
								if curr_lex=[] then
									gen_char_by_char (lex_list) ([h]) t row (col+1) curr_row curr_col iscomment
								else
									gen_char_by_char (lex_list) (curr_lex@[h]) t row (col+1) curr_row curr_col iscomment
					else if h = #"|" then
						case t of
						[] =>					
							if curr_lex=[] then
								gen_char_by_char (lex_list@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
							else
								gen_char_by_char (lex_list@[(curr_lex@[h],curr_row,curr_col)]) [] t row (col+1) curr_row curr_col iscomment
						|x::y =>
							if x = #"|" then					
								if curr_lex=[] then
									gen_char_by_char (lex_list@[([h]@[x],row,col)]) [] y row (col+2) row (col+2) iscomment
								else
									gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h]@[x],row,col)]) [] y row (col+2) row (col+2) iscomment
							else
								if curr_lex=[] then
									gen_char_by_char (lex_list) ([h]) t row (col+1) curr_row curr_col iscomment
								else
									gen_char_by_char (lex_list) (curr_lex@[h]) t row (col+1) curr_row curr_col iscomment
					else if h = #":" then
						case t of
						[] =>					
							if curr_lex=[] then
								gen_char_by_char lex_list [h] t row (col+1) row col iscomment
							else
								gen_char_by_char lex_list (curr_lex@[h]) t row (col+1) curr_row curr_col iscomment
						|x::y =>
							if x = #"=" then					
								if curr_lex=[] then
									gen_char_by_char (lex_list@[([h]@[x],row,col)]) [] y row (col+2) curr_row curr_col iscomment
								else
									gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h]@[x],row,col)]) [] y row (col+2) curr_row curr_col iscomment
							else
								if curr_lex=[] then
									gen_char_by_char lex_list [h] t row (col+1) row col iscomment
								else
									gen_char_by_char lex_list (curr_lex@[h]) t row (col+1) curr_row curr_col iscomment
					else if h = #"(" then
						case t of
						[] =>					
							if curr_lex=[] then
								gen_char_by_char (lex_list@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
							else
								gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
						|x::y =>
							if x = #"*" then					
								if curr_lex=[] then
									gen_char_by_char (lex_list) ([h]@[x]) y row (col+2) row col true
								else
									gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]) ([h]@[x]) y row (col+2) row col true
							else
								if curr_lex=[] then
									gen_char_by_char (lex_list@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
								else
									gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]@[([h],row,col)]) [] t row (col+1) curr_row curr_col iscomment
					else
						if curr_lex=[] then
							gen_char_by_char lex_list [h] t row (col+1) row col iscomment
						else
							gen_char_by_char lex_list (curr_lex@[h]) t row (col+1) curr_row curr_col iscomment
				else
					if h = #"*" then
						case t of
						[] =>
							gen_char_by_char (lex_list@[(curr_lex,curr_row,curr_col)]) [] t row (col+1) curr_row curr_col false
						|x::y =>
							if x = #")" then					
								gen_char_by_char (lex_list) [] y row (col+2) curr_row curr_col false
							else
								gen_char_by_char (lex_list) (curr_lex@[h]) t row (col+1) curr_row curr_col true
					else if h = #"\n" then
						gen_char_by_char (lex_list) (curr_lex@[h]) t (row+1) (col) curr_row curr_col true
					else
						gen_char_by_char (lex_list) (curr_lex@[h]) t row (col+1) curr_row curr_col true

	in gen_char_by_char [] [] l 1 1 1 1 false
	end;

val lex_l = gen_lexeme char_list;

(*Code for the Deterministic Finite Automaton*)

(*Error state acceptance*)
fun error [] viewed row col = SOME (ERROR(row,col,String.size(String.implode(viewed)))) |
	error (h::t) viewed row col = error t (viewed@[h]) row col;

(*"~" state acceptance*)
fun unminus [] viewed row col = SOME (UNMINUS(row,col)) |
	unminus (h::t) viewed row col = error t (viewed@[h]) row col;

(*"+" state acceptance*)
fun binadd [] viewed row col = SOME (BINADD(row,col)) |
	binadd (h::t) viewed row col = error t (viewed@[h]) row col;

(*"-" state acceptance*)
fun binsub [] viewed row col = SOME (BINSUB(row,col)) |
	binsub (h::t) viewed row col = error t (viewed@[h]) row col;

(*"/" state acceptance*)
fun bindiv [] viewed row col = SOME (BINDIV(row,col)) |
	bindiv (h::t) viewed row col = error t (viewed@[h]) row col;

(*"*" state acceptance*)
fun binmul [] viewed row col = SOME (BINMUL(row,col)) |
	binmul (h::t) viewed row col = error t (viewed@[h]) row col;

(*"%" state acceptance*)
fun binmod [] viewed row col = SOME (BINMOD(row,col)) |
	binmod (h::t) viewed row col = error t (viewed@[h]) row col;

(*"!" state acceptance*)
fun neg [] viewed row col = SOME (NEG(row,col)) |
	neg (h::t) viewed row col = error t (viewed@[h]) row col;

(*"=" state acceptance*)
fun eq [] viewed row col = SOME (EQ(row,col)) |
	eq (h::t) viewed row col = error t (viewed@[h]) row col;

fun lp [] viewed row col = SOME (LP(row,col)) |
	lp (h::t) viewed row col = error t (viewed@[h]) row col;

(*")" state acceptance*)
fun rp [] viewed row col = SOME (RP(row,col)) |
	rp (h::t) viewed row col = error t (viewed@[h]) row col;

(*"{" state acceptance*)
fun lb [] viewed row col = SOME (LB(row,col)) |
	lb (h::t) viewed row col = error t (viewed@[h]) row col;

(*"}" state acceptance*)
fun rb [] viewed row col = SOME (RB(row,col)) |
	rb (h::t) viewed row col = error t (viewed@[h]) row col;

(*";" state acceptance*)
fun eos [] viewed row col = SOME (EOS(row,col)) |
	eos (h::t) viewed row col = error t (viewed@[h]) row col;

(*"," state acceptance*)
fun comma [] viewed row col = SOME (COMMA(row,col)) |
	comma (h::t) viewed row col = error t (viewed@[h]) row col;

(*"&&" state acceptance*)
fun and_end [] viewed row col = SOME (AND(row,col)) |
	and_end (h::t) viewed row col = error t (viewed@[h]) row col;

fun and_one [] viewed row col = error [] (viewed) row col |
	and_one (h::t) viewed row col = if h = #"&" then
									 	and_end t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*"||" state acceptance*)
fun or_end [] viewed row col = SOME (OR(row,col)) |
	or_end (h::t) viewed row col = error t (viewed@[h]) row col;

fun or_one [] viewed row col = error [] (viewed) row col |
	or_one (h::t) viewed row col = if h = #"|" then
									 	or_end t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*"ne" state acceptance*)
fun ne_end [] viewed row col = SOME (NE(row,col)) |
	ne_end (h::t) viewed row col = error t (viewed@[h]) row col;

(*"lte" state acceptance*)
fun lte_end [] viewed row col = SOME (LTE(row,col)) |
	lte_end (h::t) viewed row col = error t (viewed@[h]) row col;

(*"lt" state acceptance*)
fun lt_one [] viewed row col = SOME (LT(row,col)) |
	lt_one (h::t) viewed row col = if h = #">" then
									 	ne_end t (viewed@[h]) row col
									 else if h = #"=" then
									 	lte_end t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*"gte" state acceptance*)
fun gte_end [] viewed row col = SOME (GTE(row,col)) |
	gte_end (h::t) viewed row col = error t (viewed@[h]) row col;

(*"gt" state acceptance*)
fun gt_one [] viewed row col = SOME (GT(row,col)) |
	gt_one (h::t) viewed row col = if h = #"=" then
									 	gte_end t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*":=" state acceptance*)
fun assign_end [] viewed row col = SOME (ASSIGN(row,col)) |
	assign_end (h::t) viewed row col = error t (viewed@[h]) row col;

fun assign_one [] viewed row col = error [] (viewed) row col |
	assign_one (h::t) viewed row col = if h = #"=" then
									 	assign_end t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*States to accept or reject identifiers*)
fun identifier [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	identifier (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*States to accept or reject int literals*)
fun ret_int (SOME l) = l
	|ret_int _ = raise InvalidInp;

fun intlit [] viewed row col = SOME (INTLIT(row,col,ret_int(Int.fromString(String.implode(viewed))))) |
	intlit (h::t) viewed row col = if Char.isDigit(h) then
									 	intlit t (viewed@[h]) row col 
									 else
									 	error t (viewed@[h]) row col;

(*States to accept of reject "bool"*)
fun bool_end [] viewed row col = SOME (BOOL(row,col)) |
	bool_end (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun bool_three [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	bool_three (h::t) viewed row col = if h = #"l" then
										bool_end t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun bool_two [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	bool_two (h::t) viewed row col = if h = #"o" then
										bool_three t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun bool_one [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	bool_one (h::t) viewed row col = if h = #"o" then
										bool_two t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*States to accept of reject "else"*)
fun else_end [] viewed row col = 	SOME (ELSE(row,col)) |
	else_end (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun else_three [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	else_three (h::t) viewed row col = if h = #"e" then
										else_end t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun else_two [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	else_two (h::t) viewed row col = if h = #"s" then
										else_three t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun else_one [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	else_one (h::t) viewed row col = if h = #"l" then
										else_two t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*States to accept of reject "ff"*)
fun ff_end [] viewed row col = SOME (BOOLVAL(row,col,false)) |
	ff_end (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun ff_one [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	ff_one (h::t) viewed row col = if h = #"f" then
										ff_end t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*States to accept of reject "if"*)
fun if_end [] viewed row col = 	SOME (IF(row,col)) |
	if_end (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*States to accept of reject "int"*)
fun int_end [] viewed row col = 	SOME (INT(row,col)) |
	int_end (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun int_two [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	int_two (h::t) viewed row col = if h = #"t" then
										int_end t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun i_one [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	i_one (h::t) viewed row col = if h = #"n" then
										int_two t (viewed@[h]) row col
									else if h = #"f" then
										if_end t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*States to accept of reject "proc"*)
fun proc_end [] viewed row col = 	SOME (PROC(row,col)) |
	proc_end (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun proc_three [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	proc_three (h::t) viewed row col = if h = #"c" then
										proc_end t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*States to accept of reject "print"*)
fun print_end [] viewed row col = 	SOME (PRINT(row,col)) |
	print_end (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun print_four [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	print_four (h::t) viewed row col = if h = #"t" then
										print_end t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun print_three [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	print_three (h::t) viewed row col = if h = #"n" then
										print_four t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun r_two [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	r_two (h::t) viewed row col = if h = #"i" then
										print_three t (viewed@[h]) row col
									 else if h = #"o" then
										proc_three t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun p_one [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	p_one (h::t) viewed row col = if h = #"r" then
										r_two t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*States to accept of reject "read"*)
fun read_end [] viewed row col = SOME (READ(row,col)) |
	read_end (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun read_three [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	read_three (h::t) viewed row col = if h = #"d" then
										read_end t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun read_two [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	read_two (h::t) viewed row col = if h = #"a" then
										read_three t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun read_one [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	read_one (h::t) viewed row col = if h = #"e" then
										read_two t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*States to accept of reject "while"*)
fun while_end [] viewed row col = SOME (WHILE(row,col)) |
	while_end (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun while_four [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	while_four (h::t) viewed row col = if h = #"e" then
										while_end t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun while_three [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	while_three (h::t) viewed row col = if h = #"l" then
										while_four t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun while_two [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	while_two (h::t) viewed row col = if h = #"i" then
										while_three t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun while_one [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	while_one (h::t) viewed row col = if h = #"h" then
										while_two t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*States to accept of reject "tt"*)
fun tt_end [] viewed row col = 	SOME (BOOLVAL(row,col,true)) |
	tt_end (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;									 	

(*States to accept of reject "then"*)
fun then_end [] viewed row col = 	SOME (THEN(row,col)) |
	then_end (h::t) viewed row col = if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun then_three [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	then_three (h::t) viewed row col = if h = #"n" then
										then_end t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun then_two [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	then_two (h::t) viewed row col = if h = #"e" then
										then_three t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

fun t_one [] viewed row col = SOME (IDENT(row,col,String.implode(viewed))) |
	t_one (h::t) viewed row col = if h = #"h" then
										then_two t (viewed@[h]) row col
									 else if h = #"t" then
										tt_end t (viewed@[h]) row col
									 else if Char.isAlphaNum(h) then
									 	identifier t (viewed@[h]) row col
									 else
									 	error t (viewed@[h]) row col;

(*Code of start state of DFA*)
fun start ((h::t),row,col) = if h = #"b" then
								bool_one t [h] row col
							else if h = #"e" then
								else_one t [h] row col
							else if h = #"f" then
								ff_one t [h] row col
							else if h = #"i" then
								i_one t [h] row col
							else if h = #"p" then
								p_one t [h] row col
							else if h = #"r" then
								read_one t [h] row col
							else if h = #"t" then
								t_one t [h] row col
							else if h = #"w" then
								while_one t [h] row col
							else if Char.isAlpha(h) then
								identifier t [h] row col
							else if Char.isDigit(h) then
								intlit t [h] row col
							else if h = #"~" then
								unminus t [h] row col
							else if h = #"+" then
								binadd t [h] row col
							else if h = #"-" then
								binsub t [h] row col
							else if h = #"*" then
								binmul t [h] row col
							else if h = #"/" then
								bindiv t [h] row col
							else if h = #"%" then
								binmod t [h] row col
							else if h = #"!" then
								neg t [h] row col
							else if h = #"&" then
								and_one t [h] row col
							else if h = #"|" then
								or_one t [h] row col
							else if h = #"=" then
								eq t [h] row col
							else if h = #":" then
								assign_one t [h] row col
							else if h = #">" then
								gt_one t [h] row col
							else if h = #"<" then
								lt_one t [h] row col
							else if h = #"(" then
								lp t [h] row col
							else if h = #")" then
								rp t [h] row col
							else if h = #"{" then
								lb t [h] row col
							else if h = #"}" then
								rb t [h] row col
							else if h = #";" then
								eos t [h] row col
							else if h = #"," then
								comma t [h] row col
							else
								error t [h] row col;

fun scanner [] = [] |
	scanner (h::t) = (start h)::(scanner t);

val scan = scanner lex_l;

(*Write the output to a text file*)
fun write (filename: string, s) = 
    let 
    	val f =  TextIO.openAppend filename
    in  
    	(TextIO.output (f, s); TextIO.closeOut f) 
    end

exception TokenInputError;

fun writetofile [(SOME (h))] = write (dst, toString h) |
	writetofile ((SOME (h))::t) =  (write (dst, toString h);writetofile t) |
	writetofile _ = raise TokenInputError;
	
val fio = TextIO.openAppend dst;
TextIO.closeOut fio; 
val vl = writetofile scan;
						
OS.Process.exit(OS.Process.success);