val l = CommandLine.arguments();
val inputFile = (hd l);
val parseTreeOutput = (hd (tl l));
val symbolTableOutput = (hd (tl (tl l)));

datatype NODE = NONTERMINAL of string * int
		| INTLITERAL of int * int
		| BOOLLITERAL of string * int
		| MISTAKE of int * string

datatype SYMBOL = KEYWORD of string * string
	| INTSYMBOL of string * string
	| BOOLSYMBOL of string * string
	| PROCSYMBOL of string * string

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
		| CALL of int * int
		| READ of int * int
		| ERROR of int * int * int
		| INTLIT of int * int * int
		| IDENT of int * int * string
		| BOOLVAL of int * int * bool

fun toStringToken TOKEN =
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
			| CALL(a,b) => "CALL(" ^ Int.toString a ^ "," ^ Int.toString b ^ ")\n"
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


fun toString NODE =
case NODE of
NONTERMINAL(a, b) => a
| INTLITERAL(a, b) => (Int.toString(a))
| BOOLLITERAL(a, b) => a
| MISTAKE(a, b) => "ERROR in line " ^ (Int.toString(a)) ^ ": " ^ b

fun toStringSymbol SYMBOL =
(case SYMBOL of
        KEYWORD(a,b) => a ^"\t" ^b ^"\n"
        | INTSYMBOL(a,b) => a ^"\tIDENT\tINT\t" ^b ^ "\n"
        | BOOLSYMBOL(a,b) => a ^"\tIDENT\tBOOL\t" ^b ^ "\n"
        | PROCSYMBOL(a,b) => a ^"\tIDENT\tPROC\t" ^b ^ "\n");

Control.Print.printLength := 100;

(*Read from the text file*)
fun getslist (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (slist,clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (#"\n", f') => if String.implode(List.rev(clist)) = "" then
								loop (slist,[], f')
							else
								loop (slist@[String.implode(List.rev(clist))],[], f')
		  |	SOME (c, f') => loop (slist,c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; slist)
    in  loop ([],[], f)
    end;

val token_list = getslist(inputFile);

(*Function to split a token into name and list of values*)
fun split_token s = 
	let fun split_string l1 l2 [] = l1
			| split_string l1 l2 (x::y) = if x = #"(" orelse x = #")" orelse x = #"," then
										split_string (l1@[String.implode(l2)]) [] y
									else
										split_string l1 (l2@[x]) y	
	in split_string [] [] (String.explode(s))
	end;									

val split_token_list = map split_token token_list;

fun ret_int (SOME l) = l;

fun ret_bool (SOME l) = l;

fun write (filename: string, s) = 
    let 
    	val f =  TextIO.openAppend filename
    in  
    	(TextIO.output (f, s); TextIO.closeOut f) 
    end

val fio = TextIO.openAppend symbolTableOutput;
TextIO.closeOut fio; 

exception InvalidToken;

fun gen_token (x::y) = if x = "UNMINUS" then 
						UNMINUS(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "BINADD" then 
						BINADD(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "BINSUB" then 
						BINSUB(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "BINDIV" then 
						BINDIV(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "BINMUL" then 
						BINMUL(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "BINMOD" then 
						BINMOD(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "NEG" then 
						NEG(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "AND" then 
						AND(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "OR" then 
						OR(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "ASSIGN" then 
						ASSIGN(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "EQ" then 
						EQ(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "NE" then 
						NE(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "LT" then 
						LT(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "LTE" then 
						LTE(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "GT" then 
						GT(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "GTE" then 
						GTE(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "LP" then 
						LP(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "RP" then 
						RP(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "LB" then 
						LB(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "RB" then 
						RB(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "EOS" then 
						EOS(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "COMMA" then 
						COMMA(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "INT" then 
						INT(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "BOOL" then 
						BOOL(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "IF" then 
						IF(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "THEN" then 
						THEN(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "CALL" then 
						CALL(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "ELSE" then 
						ELSE(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "WHILE" then 
						WHILE(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "PROC" then 
						PROC(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "PRINT" then 
						PRINT(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "READ" then 
						READ(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))))
					else if x = "ERROR" then 
						ERROR(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))),ret_int(Int.fromString(hd (tl (tl y)))))
					else if x = "INTLIT" then 
						INTLIT(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))),ret_int(Int.fromString(hd (tl (tl y)))))
					else if x = "IDENT" then 
						IDENT(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))),(hd (tl (tl y))))
					else if x = "BOOLVAL" then 
						BOOLVAL(ret_int(Int.fromString(hd y)),ret_int(Int.fromString(hd (tl y))),ret_bool(Bool.fromString(hd (tl (tl y)))))
					else raise InvalidToken

(*Generate list of tokens*)
val gen_token_list = map gen_token split_token_list;

val keyword_list = [KEYWORD("int","INT"),KEYWORD("bool","BOOL"),KEYWORD("tt","BOOLVAL"),KEYWORD("ff","BOOLVAL"),KEYWORD("if","IF"),KEYWORD("then","THEN"),KEYWORD("else","ELSE"),KEYWORD("while","WHILE"),KEYWORD("proc","PROC"),KEYWORD("print","PRINT"),KEYWORD("read","READ"),KEYWORD("call","CALL")]

fun write_to_symboltable s = write(symbolTableOutput,s);

fun write_keyword_list l = map write_to_symboltable (map toStringSymbol l)

val keywd_list_write = write_keyword_list keyword_list;

exception MatchNotFound;

(*Code of the Parser*)
fun program (l as x::y) level scope= 
	("[\n" ^ (toString (NONTERMINAL("Program",level))) ^ "\n[" ^(#1 (block (x::y) (level+1) scope)) ^ "\n]\n" ^ "\n]") |
	program _ _ _ = raise MatchNotFound

and block (x::y) level scope = 
	let val (str,l) = (decleration_seq (x::y) (level+1) scope)
		val (str_cmd,l_cmd) = (command_seq (l) (level+1))
	in (print ("inside block" ^ Int.toString(level));
		("\n" ^ (toString (NONTERMINAL("Block",level))) ^ "\n[" ^ str ^ " , " ^ str_cmd ^ "\n]",l_cmd)
		)
	end |
	block [] level scope = ("",[])

and decleration_seq (x::y) level scope = 
	let val (str,l) = (var_decls (x::y) (level+1) scope)
		val (str_proc,l_proc) = (proc_decls (l) (level+1) scope)
	in (print ("inside declseq" ^ Int.toString(level));
		("\n" ^ (toString (NONTERMINAL("DeclarationSeq",level))) ^ "\n[" ^ str ^ " , " ^  str_proc ^ "\n]",l_proc)
		)
	end |
	decleration_seq _ _ scope = raise MatchNotFound

and var_decls (x::y) level scope = 
	let val (str,l) = (int_var_decls (x::y) (level+1) scope)
		val (str_bool,l_bool) = (bool_var_decls (l) (level+1) scope)
	in ("\n" ^ (toString (NONTERMINAL("VarDecls",level))) ^ "\n[" ^ str ^ " , " ^ str_bool ^ "\n]",l_bool)
 	end |
 	var_decls _ _ scope = raise MatchNotFound

and int_var_decls (x::y::z) level scope = 
	let val (str,l) = (vardef (y::z) (level+1) scope "int")
	in case x of
		INT(a,b) => ("\n" ^ (toString (NONTERMINAL("IntVarDecls",level))) ^ "\n[INT , " ^ str ^ " ]",l)
		| _ => ("\n" ^ (toString (NONTERMINAL("IntVarDecls",level))) ^ "[EPSILON]" ^ "\n",x::y::z)
	end |
	int_var_decls [] level scope = ("",[]) |
	int_var_decls [x] level scope = 
		case x of
			INT(a,b) => ("\n" ^ (toString (NONTERMINAL("IntVarDecls",level))) ^ "\n[INT , " ^ "Error VarDef not found " ^ " ]\n",[])
			| _ => ("\n" ^ (toString (NONTERMINAL("IntVarDecls",level))) ^ "[EPSILON]" ^ "\n",[x])

and vardef (x::y) level scope sym_type= 
	let val (str,l) = (vardef1 (y) (level+1) scope sym_type)
	in case x of
		IDENT(a,b,c) => if sym_type = "int" then 
							(write(symbolTableOutput,(toStringSymbol (INTSYMBOL(c,scope))));("VarDef[" ^ (toString (NONTERMINAL("Ident",level))) ^ "[" ^ c ^ "] , " ^ str ^ "]",l))
						else
							(write(symbolTableOutput,(toStringSymbol (BOOLSYMBOL(c,scope))));("VarDef[" ^ (toString (NONTERMINAL("Ident",level))) ^ "[" ^ c ^ "] , " ^ str ^ "]",l))
		| _ => ("Error Ident not found",x::y)
	end |
	vardef [] level scope sym_type= ("Error in VarDef",[])

and bool_var_decls (x::y::z) level scope = 
	let val (str,l) = (vardef (y::z) (level+1) scope "bool")
	in case x of
		BOOL(a,b) => ("\n" ^ (toString (NONTERMINAL("BoolVarDecls",level))) ^ "\n[BOOL , " ^ str ^ " ]\n",l)
		| _ => ("\n" ^ (toString (NONTERMINAL("BoolVarDecls",level))) ^ "\n[EPSILON]\n",x::y::z)
	end |
	bool_var_decls [] level scope = ("",[]) |
	bool_var_decls [x] level scope = 
		case x of
			BOOL(a,b) => ("\n" ^ (toString (NONTERMINAL("BoolVarDecls",level))) ^ "\n[BOOL , " ^ "Error VarDef not found" ^ " ]\n",[])
			| _ => ("\n" ^ (toString (NONTERMINAL("BoolVarDecls",level))) ^ "\n[EPSILON]\n",[x])

and vardef1 (x::y) level scope sym_type = 
	(case x of 
			COMMA(e,f) => let val (str,l) = (vardef (y) (level+1) scope sym_type)
					in	((toString (NONTERMINAL("VarDef1",level))) ^ "[" ^ (toString (NONTERMINAL("COMMA",level))) ^ " , " ^ str ^ "]",l)
					end
			| EOS(e,f) =>
				((toString (NONTERMINAL("VarDef1",level))) ^ "[" ^ (toString (NONTERMINAL("EOS",level))) ^ "]",y)
			| _ =>
				((toString (NONTERMINAL("VarDef1",level))) ^ "["  ^ "Error COMMA or EOS not found" ^ "] , ",x::y))
	|vardef1 [] level scope sym_type = ("Error in VarDef1",[])

and proc_decls (x::y::z) level scope = 
	(case x of
		PROC(a,b) =>( case y of
						IDENT(e,f,g) =>let val (str,l) = (block (z) (level+1) (scope ^ ":" ^ g))
								in	(write(symbolTableOutput,(toStringSymbol (PROCSYMBOL(g,scope))));
									(case l of
										(yh::yt) =>
										 	(let val (str_proc,l_proc) = (proc_decls (yt) (level+1) scope)
										 	 in (case yh of 
													EOS(e,f) =>
														("\n" ^ (toString (NONTERMINAL("ProcDecls",level))) ^ "[\n" ^ (toString (NONTERMINAL("PROC",level))) ^ ", " ^ (toString (NONTERMINAL("Ident",level))) ^ " [" ^ g ^ "], " ^ str ^ " , " ^ (toString (NONTERMINAL("EOS",level))) ^ ", \n" ^ str_proc ^ "\n]",l_proc)
													| _ => 
														("\n" ^ (toString (NONTERMINAL("ProcDecls",level))) ^ "[\n" ^ (toString (NONTERMINAL("PROC",level))) ^ ", " ^ (toString (NONTERMINAL("Ident",level))) ^ " [" ^ g ^ "], " ^ str ^ " , \n Error in ProcDecls EOS not found" ^ ", \n" ^ str_proc ^ "\n]",yt))
										 	
										 	 end)
										| [] => (
													("\n" ^ (toString (NONTERMINAL("ProcDecls",level))) ^ "[\n" ^ (toString (NONTERMINAL("PROC",level))) ^ ", " ^ (toString (NONTERMINAL("Ident",level))) ^ " [" ^ g ^ "], " ^ str ^ " , \n Error in ProcDecls EOS not found" ^ "\n]",[]))
										 		)
									)
								end
						| _ => ("\n" ^ (toString (NONTERMINAL("ProcDecls",level))) ^ "[\n" ^ (toString (NONTERMINAL("PROC",level))) ^ " , " ^ ",\n Error in ProcDecls Ident not found" ^ "\n]",y::z))
		| _ => ("\n ProcDecls [EPSILON]",x::y::z))
	|proc_decls (x::[]) level scope = 
			(case x of
				PROC(a,b) => ("\n" ^ (toString (NONTERMINAL("ProcDecls",level))) ^ "[\n" ^ (toString (NONTERMINAL("PROC",level))) ^ ", " ^ ",\n Error in ProcDef Ident not found" ^ "\n]",[])
				| _ => ("\n ProcDecls [EPSILON]",x::[]))
	|proc_decls [] level scope = ("\n ProcDecls [EPSILON]",[])

and command_seq [] level = ("",[]) |
	command_seq (x::y) level = 
	(print ("inside cmd seq" ^ Int.toString(level));
		case x of
		LB(a,b) =>  let val (str,l) = command (y) (level+1)
					in  (case l of
						(xs::ys) => (case xs of
										RB(e,f) => ("\n" ^ (toString (NONTERMINAL("CommandSeq",level))) ^ "[\n" ^ (toString (NONTERMINAL("LB",level))) ^ " , " ^ str ^ " , " ^ (toString (NONTERMINAL("RB",level))) ^ "\n]",ys)
										| _ => (print ("Inside cmd seq: " ^ (toStringToken (xs)) ^ (toStringToken (hd y)));("\n" ^ (toString (NONTERMINAL("CommandSeq",level))) ^ "[\n" ^ (toString (NONTERMINAL("LB",level))) ^ " , " ^ str ^ " , " ^ " Error RB not found" ^ "\n]",xs::ys)) (*return the remaining list......get this doubt cleared*)
									)
						|[] => ("\n" ^ (toString (NONTERMINAL("CommandSeq",level))) ^ "[\n" ^ (toString (NONTERMINAL("LB",level))) ^ " , " ^ str ^ " , " ^ " Error RB not found" ^ "\n]",[]) (*return the remaining list......get this doubt cleared*)
						)
					end
		| _ => ("\n  Error CommandSeq LB not found" ^ "\n",x::y)
	)

and command [] level = ("Error in command",[]) |
	command [x] level = (case x of
		RB(a,b) => ("Command [EPSILON]",[x])
		| _ => ("Error in command",[x])) |
	command (x::y::z) level =
	(print ("Inside commd here: " ^ (toStringToken (x)) ^ (toStringToken (hd z)) ^ Int.toString(level)); 
	case x of
		IDENT(a,b,c) => (case y of
						 ASSIGN(e,f) =>
							let val (str,l) = assignment_command (z) (level+1)
							in  (case l of
								(xs::ys) => (case xs of
												EOS(e,f) => let val (str1,l1) = command (ys) (level+1)
														in (print ("Inside cmd here: " ^ (toStringToken (xs)) ^ (toStringToken (hd z)));("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("AssignmentCmd",level))) ^ "[" ^ "Ident [" ^ c ^ "] , " ^ (toString (NONTERMINAL("ASSIGN",level))) ^ " , " ^ str ^ "] , " ^ (toString (NONTERMINAL("EOS",level))) ^ " , " ^ str1 ^ "\n]\n",l1))
														end
												| _ => let val (str1,l1) = command (xs::ys) (level+1)
														in (print ("Inside cmd: " ^ (toStringToken (xs)) ^ (toStringToken (hd z)));("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("AssignmentCmd",level))) ^ "[" ^ "Ident [" ^ c ^ "] , " ^ (toString (NONTERMINAL("ASSIGN",level))) ^ " , " ^ str ^ "] , " ^ " Error EOS not found" ^ "\n]\n",l1)) (*return the remaining list......get this doubt cleared*)
														end
											)
								|[] => let val (str1,l1) = command ([]) (level+1)
										in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("AssignmentCmd",level))) ^ "\n[ " ^ "Ident [" ^ c ^ "] , " ^ (toString (NONTERMINAL("ASSIGN",level))) ^ " , " ^ str ^ "] , " ^ " Error EOS not found" ^ " , " ^ str1 ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
										end
								)
							end
						 | _ => let val (str1,l1) = command (y::z) (level+1)
						 		in ((toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("AssignmentCmd",level))) ^ "\n[ Ident [" ^ c ^ "] , " ^ "Error ASSIGN not found" ^ "\n]" ^ str1 ^ "\n]",l1)
								end)
		| CALL(a,b) =>	let val (str,l) = call_command (y::z) (level+1) (*TODO:try this out*)
							in  (case l of
								(xs::ys) => (case xs of
												EOS(e,f) => let val (str1,l1) = command (ys) (level+1)
														in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("CallCmd",level))) ^ "\n[\n" ^ (toString (NONTERMINAL("CALL",level))) ^ " , " ^ str ^ "\n] , " ^ (toString (NONTERMINAL("EOS",level))) ^ "\n , " ^ str1 ^ "\n]",l1)
														end	
												| _ => let val (str1,l1) = command (xs::ys) (level+1)
														in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("CallCmd",level))) ^ "[" ^ (toString (NONTERMINAL("CALL",level))) ^ " , " ^ str ^ "\n] , " ^ " Error EOS not found" ^ "\n , " ^ str1 ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
														end
											)
								|[] => let val (str1,l1) = command ([]) (level+1)
										in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("CallCmd",level))) ^ "\n[\n" ^ (toString (NONTERMINAL("CALL",level))) ^ " , " ^ str ^ "\n] , " ^ " Error EOS not found" ^ " , " ^ str1 ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
										end
								)
							end
		| READ(a,b) =>  let val (str,l) = read_command (z) (level+1)
					in case y of
						LP(e,f) =>  (case l of
									(xs::ys::zs) => (case xs of
													RP(e,f) => (case ys of
																EOS(a,b) => let val (str1,l1) = command (zs) (level+1)
																		in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("ReadCmd",level))) ^ "[ READ , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ (toString (NONTERMINAL("RP",level))) ^ "] , " ^ (toString (NONTERMINAL("EOS",level))) ^ " , " ^ str1 ^ "\n]",l1)
																		end
																| _ => let val (str1,l1) = command (ys::zs) (level+1)
																		in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("ReadCmd",level))) ^ "[ READ , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ (toString (NONTERMINAL("RP",level))) ^ "] , Error EOS not found" ^ " , " ^ str1 ^ "\n]",l1)
																		end
																)
													| _ => let val (str1,l1) = command (xs::ys::zs) (level+1)
															in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("ReadCmd",level))) ^ "[ READ , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ " Error RP not found" ^ "]\n , " ^ str1 ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
															end
											)
									| xs::[] => (case xs of
													RP(e,f) => let val (str1,l1) = command ([]) (level+1)
																in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("ReadCmd",level))) ^ "[ READ , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ (toString (NONTERMINAL("RP",level))) ^ ", Error EOS not found" ^ " , " ^ str1 ^ "\n" ^ "\n]",l1)
																end
													| _ => let val (str1,l1) = command ([xs]) (level+1)
															in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("ReadCmd",level))) ^ "[ READ , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ " Error RP not found" ^ "]\n , " ^ str1 ^ "\n" ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
															end
												)
									|[] => let val (str1,l1) = command ([]) (level+1)
											in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("ReadCmd",level))) ^ "[ READ , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ " Error RP not found" ^ "]\n , " ^ str1 ^ "\n" ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
											end
									)
						| _ => ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("ReadCmd",level))) ^ "[ READ ,  Error LP not found" ^ "]\n]\n",y::z) (*ask for requirement of recursive call here*)
					end
		| PRINT(a,b) =>  let val (str,l) = print_command (z) (level+1)
					in case y of
						LP(e,f) =>  (case l of
									(xs::ys::zs) => (case xs of
													RP(e,f) => (case ys of
																EOS(a,b) => let val (str1,l1) = command (zs) (level+1)
																		in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("PrintCmd",level))) ^ "[ PRINT , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ (toString (NONTERMINAL("RP",level))) ^ "] , " ^ (toString (NONTERMINAL("EOS",level))) ^ " , " ^ str1 ^ "\n" ^ "\n]",l1)
																		end
																| _ => let val (str1,l1) = command (ys::zs) (level+1)
																		in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("PrintCmd",level))) ^ "[ PRINT , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ (toString (NONTERMINAL("RP",level))) ^ "] , Error EOS not found" ^ " , " ^ str1 ^ "\n" ^ "\n]",l1)
																		end
																)
													| _ => let val (str1,l1) = command (xs::ys::zs) (level+1)
															in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("PrintCmd",level))) ^ "[ PRINT , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ " Error RP not found" ^ "]\n , " ^ str1 ^ "\n" ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
															end
												)
									| ([xs]) => (case xs of
													RP(e,f) => let val (str1,l1) = command ([]) (level+1)
															in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("PrintCmd",level))) ^ "[ PRINT , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ (toString (NONTERMINAL("RP",level))) ^ "] , Error EOS not found" ^ "" ^ str1 ^ "\n" ^ "\n]",l1)
															end																
													| _ => let val (str1,l1) = command ([xs]) (level+1)
															in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("PrintCmd",level))) ^ "[ PRINT , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ " Error RP not found" ^ "]\n , " ^ str1 ^ "\n" ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
															end
												)
									|[] => let val (str1,l1) = command ([]) (level+1)
											in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("PrintCmd",level))) ^ "[ PRINT , " ^ (toString (NONTERMINAL("LP",level))) ^ " , " ^ str ^ " , " ^ " Error RP not found" ^ "]\n" ^ str1 ^ "\n" ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
											end
									)
						| _ => ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("PrintCmd",level))) ^ "[ PRINT ,  Error LP not found" ^ "]\n]\n",y::z) (*return the remaining list......get this doubt cleared*)
					end
		| IF(a,b) => let val (str,l) = if_command (y::z) (level+1)
					 in (case l of
						(xs::ys) => (case xs of
										EOS(e,f) => let val (str1,l1) = command (ys) (level+1)
												in (("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("ConditionalCmd",level))) ^ "[\n" ^ (toString (NONTERMINAL("IF",level))) ^ ", " ^ str ^ "] , " ^ (toString (NONTERMINAL("EOS",level))) ^ "\n , " ^ str1 ^ "\n" ^ "\n]",l1))
												end
										| _ => let val (str1,l1) = command (xs::ys) (level+1)
												in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("ConditionalCmd",level))) ^ "[\n" ^ (toString (NONTERMINAL("IF",level))) ^ ", " ^ str ^ "] , " ^ " Error EOS not found" ^ "\n , " ^ str1 ^ "\n" ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
												end
									)
						|[] => let val (str1,l1) = command ([]) (level+1)
								in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("ConditionalCmd",level))) ^ "[\n" ^ (toString (NONTERMINAL("IF",level))) ^ " , " ^ str ^ "] , " ^ " Error EOS not found" ^ "\n , " ^ str1 ^ "\n" ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
								end
						)
					 end
		| WHILE(a,b) =>  let val (str,l) = while_command (y::z) (level+1)
					 in (case l of
						(xs::ys) => (case xs of
										EOS(e,f) => let val (str1,l1) = command (ys) (level+1)
												in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("WhileCmd",level))) ^ "[\n" ^ (toString (NONTERMINAL("WHILE",level))) ^ ", " ^ str ^ "]" ^ " , " ^ (toString (NONTERMINAL("EOS",level))) ^ "\n , " ^ str1 ^ "\n" ^ "\n]",l1)
												end
										| _ => let val (str1,l1) = command (xs::ys) (level+1)
												in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("WhileCmd",level))) ^ "[\n" ^ (toString (NONTERMINAL("WHILE",level))) ^ ", " ^ str ^ "] , " ^ " Error EOS not found" ^ "\n , " ^ str1 ^ "\n" ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
												end
									)
						|[] => let val (str1,l1) = command ([]) (level+1)
								in ("\n" ^ (toString (NONTERMINAL("Command",level))) ^ "[\n" ^ (toString (NONTERMINAL("WhileCmd",level))) ^ "[\n" ^ (toString (NONTERMINAL("WHILE",level))) ^ " , " ^ str ^ "] , " ^ " Error EOS not found" ^ "\n , " ^ str1 ^ "\n" ^ "\n]",l1) (*return the remaining list......get this doubt cleared*)
								end
						)
					 end
		| _ => ("Command [EPSILON]",x::y::z)
	)

and assignment_command [] level = ("Error in Assignment cmd. Expression not found",[]) |
	assignment_command (x::y) level = let val (str,l) = expression (x::y) (level+1)
									in (print "inside ass_cmd";(str,l))
									end

and read_command [] level = ("Error in Read command. Identifier not found",[]) |
	read_command (x::y) level = case x of 
									IDENT(a,b,c) => ((toString (NONTERMINAL("Ident",level))) ^ "[" ^ c ^ "]",y)
									| _ => ("Error in Read command. Identifier not found",x::y)

and print_command [] level = ("Error in Print command. Identifier not found",[]) |
	print_command (x::y) level = case x of 
									IDENT(a,b,c) => ((toString (NONTERMINAL("Ident",level))) ^ "[" ^ c ^ "]",y)
									| _ => ("Error in Print command. Identifier not found",x::y)

and call_command [] level = ("Error in Call command. Identifier not found",[]) |
	call_command (x::y) level = case x of 
									IDENT(a,b,c) => ((toString (NONTERMINAL("Ident",level))) ^ "[" ^ c ^ "]",y)
									| _ => ("Error in Call command. Identifier not found",x::y)

and if_command [] level = ("Error in ConditionalCmd. Bool Expression not found",[]) |
	if_command (x::y) level = let val (str,l) = bool_expression (x::y) (level+1)
							in (print "inside if";case l of
								(xs::ys) => ( case xs of 
												THEN(e,f) => let val (str_then,l_then) = then_command (ys) (level+1)
															in (str ^ ", " ^ toString(NONTERMINAL("THEN",level)) ^ ", " ^ str_then, l_then)
															end
												| _ => (str ^ ", Error in ConditionalCmd. THEN not found",xs::ys)
											)
								| [] => (str ^ ", Error in ConditionalCmd. THEN not found",[]))
							end

and then_command [] level = ("Error in ConditionalCmd. CommandSeq not found",[]) |
	then_command (x::y) level = let val (str,l) = command_seq (x::y) (level+1)
							in case l of
								(xs::ys) => ( case xs of 
												ELSE(e,f) => let val (str_else,l_else) = else_command (ys) (level+1)
															in (str ^ ", " ^ toString(NONTERMINAL("ELSE",level)) ^ ", " ^ str_else, l_else)
															end
												| _ => (str ^ ", Error in ConditionalCmd. ELSE not found",xs::ys)
											)
								| [] => (str ^ ", Error in ConditionalCmd. ELSE not found",[])
							end

and else_command [] level = ("Error in ConditionalCmd. CommandSeq not found",[]) |
	else_command (x::y) level = let val (str,l) = command_seq (x::y) (level+1)
							in (str, l)
							end

and while_command [] level = ("Error in WhileCmd. Bool Expression not found",[]) |
	while_command (x::y) level = let val (str,l) = bool_expression (x::y) (level+1)
									 val (str_cmd,l_cmd) = command_seq (l) (level+1)
							in (print "inside while";(str ^ ", " ^ str_cmd, l_cmd))
							end

(*to be updated once EBNF is updated*)
and expression [] level = ("Error in expression",[]) |
	expression (x::y) level = let val (str,l) = bool_expression (x::y) (level+1)
							in ("\n Expression \n[" ^ str ^ "\n]",l)
							end

and bool_expression [] level = ("Error in bool expression",[]) |
	bool_expression (x::y) level = let val (str,l) = bool_f (x::y) (level+1)
										val (str1,l1) = bool_e (l) (level+1)
							in ("\n BoolExpression \n[" ^ str ^ ", " ^ str1 ^ "\n]",l1)
							end

and bool_f [] level = ("Error in bool_f",[]) |
	bool_f (x::y) level = let val (str,l) = bool_g (x::y) (level+1)
								val (str1,l1) = bool_f1 (l) (level+1)
						in ("\n BoolF \n[" ^ str ^ ", " ^ str1 ^ "\n]",l1)
						end

and bool_g [] level = ("Error in bool_g",[]) |
	bool_g (x::y) level = let val (str,l) = bool_h (x::y) (level+1)
								val (str1,l1) = bool_g1 (l) (level+1)
						in ("\n BoolG \n[" ^ str ^ ", " ^ str1 ^ "\n]",l1)
						end

and bool_h [] level = ("Error in bool_h",[]) |
	bool_h (x::y) level = let val (str,l) = bool_i (x::y) (level+1)
								val (str1,l1) = bool_h1 (l) (level+1)
						in ("\n BoolH \n[" ^ str ^ ", " ^ str1 ^ "\n]",l1)
						end

and bool_i [] level = ("Error in bool_i",[]) |
	bool_i (x::y) level = case x of
							NEG(a,b) => let val (str,l) = bool_j (y) (level+1)
										in ("\n BoolI \n[" ^ (toString (NONTERMINAL("NEG",level))) ^ " , " ^ str ^ "\n]",l)
										end
							| _ =>  let val (str,l) = bool_j (x::y) (level+1)
									in ("\n BoolI \n[" ^ str ^ "\n]",l)
									end

and bool_j [] level = ("Error in bool_j",[]) |
	bool_j (x::y) level = case x of
							BOOLVAL(a,b,c) => if c = true then
												("\n BoolJ \n[" ^ "BoolLiteral [" ^ (toString (BOOLLITERAL("tt",level))) ^ "] \n]",y)
											else
												("\n BoolJ \n[" ^ "BoolLiteral [" ^ (toString (BOOLLITERAL("ff",level))) ^ "] \n]",y)
							| _ => let val (str,l) = int_expression (x::y) (level+1)
									in ("\n BoolJ \n[" ^ str ^ "\n]",l)
									end

and bool_e [] level = ("Error in bool_e",[]) |
	bool_e (x::y) level = case x of
							OR(a,b) => let val (str,l) = bool_expression (y) (level+1)
									in ("\n BoolE \n[" ^ (toString (NONTERMINAL("OR",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| _ => ("\n BoolE [EPSILON] \n",x::y)

and bool_f1 [] level = ("Error in bool_f1",[]) |
	bool_f1 (x::y) level = case x of
							AND(a,b) => let val (str,l) = bool_f (y) (level+1)
									in ("\n BoolF1 [" ^ (toString (NONTERMINAL("AND",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| _ => ("\n BoolF1 [EPSILON] \n",x::y)

and bool_g1 [] level = ("Error in bool_g1",[]) |
	bool_g1 (x::y) level = case x of
							NE(a,b) => let val (str,l) = bool_g (y) (level+1)
									in ("\n BoolG1 [" ^ (toString (NONTERMINAL("NE",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| EQ(a,b) => let val (str,l) = bool_g (y) (level+1)
									in ("\n BoolG1 [" ^ (toString (NONTERMINAL("EQ",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| _ => ("\n BoolG1 [EPSILON] \n",x::y)

and bool_h1 [] level = ("Error in bool_h1",[]) |
	bool_h1 (x::y) level = case x of
							LT(a,b) => let val (str,l) = bool_h (y) (level+1)
									in ("\n BoolH1 [" ^ (toString (NONTERMINAL("LT",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| LTE(a,b) => let val (str,l) = bool_h (y) (level+1)
									in ("\n BoolH1 [" ^ (toString (NONTERMINAL("LTE",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| GT(a,b) => let val (str,l) = bool_h (y) (level+1)
									in ("\n BoolH1 [" ^ (toString (NONTERMINAL("GT",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| GTE(a,b) => let val (str,l) = bool_h (y) (level+1)
									in ("\n BoolH1 [" ^ (toString (NONTERMINAL("GTE",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| _ => ("\n BoolH1 [EPSILON] \n",x::y)

(*Int Expression Part*)
and int_expression [] level = ("Error in int expression",[]) |
	int_expression (x::y) level = let val (str,l) = int_t (x::y) (level+1)
										val (str1,l1) = int_e (l) (level+1)
							in ("\n IntExpression [" ^ str ^ ", " ^ str1 ^ "\n]",l1)
							end

and int_t [] level = ("Error in int_t",[]) |
	int_t (x::y) level = let val (str,l) = int_f (x::y) (level+1)
								val (str1,l1) = int_t1 (l) (level+1)
						in ("\n IntT [" ^ str ^ ", " ^ str1 ^ "\n]",l1)
						end

and int_f [] level = ("Error in int_f",[]) |
	int_f (x::y) level = case x of
							UNMINUS(a,b) => let val (str,l) = int_f1 (y) (level+1)
										in ("\n IntF [" ^ (toString (NONTERMINAL("UNMINUS",level))) ^ " , " ^ str ^ "\n]",l)
										end
							| _ =>  let val (str,l) = int_f1 (x::y) (level+1)
									in ("\n IntF [" ^ str ^ "\n]",l)
									end

and int_f1 [] level = ("Error in int_f1",[]) |
	int_f1 (x::y) level = case x of
							INTLIT(a,b,c) => ("\n IntF1 [" ^ "IntLiteral [" ^ Int.toString(c) ^ "] \n]",y)
							| LP(a,b) => let val (str,l) = bool_expression (y) (level+1)
										in case l of
												(xs::ys) => (case xs of 
																RP(e,f) => ("\n IntF1 [\n" ^ (toString (NONTERMINAL("LP",level))) ^ ", " ^ str ^ ", " ^ (toString (NONTERMINAL("RP",level))) ^ "\n]\n",ys)
																| _ => ("\n IntF1 [\n" ^ (toString (NONTERMINAL("LP",level))) ^ ", " ^ str ^ ", " ^ "Error RP not found" ^ "\n]\n",xs::ys)
															)
												| [] => ("\n IntF1 [\n" ^ (toString (NONTERMINAL("LP",level))) ^ ", " ^ str ^ ", " ^ "Error RP not found" ^ "\n]\n",[])
										end
							| IDENT(a,b,c) => ("\n IntF1 \n[" ^ "Ident [" ^ (c) ^ "] \n]",y)
							| _ => ("\n IntF1 \n[ Error in int_f1. Expected Ident, IntLit or BoolExpression \n]",x::y)

and int_e [] level = ("Error in int_e",[]) |
	int_e (x::y) level = case x of
							BINADD(a,b) => let val (str,l) = int_expression (y) (level+1)
									in ("\n IntE \n[" ^ (toString (NONTERMINAL("BINADD",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| BINSUB(a,b) => let val (str,l) = int_expression (y) (level+1)
									in ("\n IntE \n[" ^ (toString (NONTERMINAL("BINSUB",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| _ => ("\n IntE [EPSILON] \n",x::y)

and int_t1 [] level = ("Error in int_t1",[]) |
	int_t1 (x::y) level = case x of
							BINMOD(a,b) => let val (str,l) = int_t (y) (level+1)
									in ("\n IntT1 \n[" ^ (toString (NONTERMINAL("BINMOD",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| BINMUL(a,b) => let val (str,l) = int_t (y) (level+1)
									in ("\n IntT1 \n[" ^ (toString (NONTERMINAL("BINMUL",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| BINDIV(a,b) => let val (str,l) = int_t (y) (level+1)
									in ("\n IntT1 \n[" ^ (toString (NONTERMINAL("BINDIV",level))) ^ ", " ^ str ^ "\n]",l)
									end
							| _ => ("\n IntT1 [EPSILON] \n",x::y)

val parse_tree = program gen_token_list 0 "global";

val fio = TextIO.openAppend parseTreeOutput;
TextIO.closeOut fio; 
val vl = write(parseTreeOutput,parse_tree);

OS.Process.exit(OS.Process.success);
