CM.make "$smlnj-tdp/back-trace.cm";
SMLofNJ.Internals.TDP.mode := true;

val l = CommandLine.arguments();
val inputFile = (hd l);
val abstractSyntaxTreeOutput = (hd (tl l));

Control.Print.printDepth := 1000;
Control.Print.stringDepth := 1000;
Control.Print.printLength := 1000;

datatype treeNode = Empty 
				| Node of string * treeNode list;

fun ret_node_list t = case t of
						Node (a,l) => l
						| Empty => [];

fun ret_str t = case t of
						Node (a,l) => a
						| Empty => "None";

exception empty_list;

fun hd [] = raise empty_list
	| hd (x::y) = x;

fun tl [] = raise empty_list
	| tl (x::y) = y;

(*Read from the text file*)
fun getclist (filename:string) = 
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (clist, f) = 
	    case TextIO.StreamIO.input1 f of
		SOME (#" ", f') => loop (clist, f')
		| SOME (#"\n", f') => loop (clist, f')
		| SOME (#"\r", f') => loop (clist, f') 
		| SOME (c, f') => loop (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loop ([], f))
    end;

val input_parse_tree = getclist(inputFile);

fun conv_to_string_list [] curr_word = []
	| conv_to_string_list (x::y) curr_word = if x = #"[" orelse x = #"]" orelse x = #"," then
												if String.implode(curr_word) = "" then
													[Char.toString(x)]@(conv_to_string_list y [])
												else
													[(String.implode(curr_word))]@[Char.toString(x)]@(conv_to_string_list y [])
											else
												conv_to_string_list y (curr_word@[x])

val parse_tree_str_list = conv_to_string_list input_parse_tree [];

val parse_tree_str_list = List.rev(tl (List.rev(tl parse_tree_str_list)));

exception error_in_tree_construction;

(*Convert input tree to Parse Tree*)
fun make_parse_tree s to_be_sent count [] = if to_be_sent <> [] then
												[Node (s,(make_parse_tree "" [] 0 ((to_be_sent))))]
											else [Node (s,[])]
	| make_parse_tree s to_be_sent count (head::tail) = if count = 1 then
																(if s = "" then
																	((*print ("To be sent: "^(foldr op^ "" to_be_sent)^" "^Int.toString(count)^"\n");*)(if head = "[" then
																		make_parse_tree s (to_be_sent@[head]) (count+1) tail
																	else if head = "]" then
																		make_parse_tree s (to_be_sent) (count-1) tail
																	else if head = "," then
																		make_parse_tree s (to_be_sent@[head]) (count) tail
																	else 
																		make_parse_tree head (to_be_sent@[head]) (count) tail
																	))
																else
																	((*print ("To be sent: "^(foldr op^ "" to_be_sent)^" "^Int.toString(count)^"\n");*)(if head = "[" then
																		make_parse_tree s (to_be_sent@[head]) (count+1) tail
																	else if head = "]" then
																		make_parse_tree s (to_be_sent) (count-1) tail
																	else make_parse_tree s (to_be_sent@[head]) (count) tail
																	))
																)

														else if count = 0 then
																(if s <> "" then
																	((*print ("To be sent: "^(foldr op^ "" to_be_sent)^" "^Int.toString(count)^"\n");*)
																	if to_be_sent = [] andalso tail <> [] then
																		(if head = "[" then
																			make_parse_tree s (to_be_sent) (count+1) tail
																		else if head = "," then
																			[Node (s,(make_parse_tree "" [] 0 ((to_be_sent))))]@(make_parse_tree "" [] 0 tail)
																		else if String.compare(head,"]") <> EQUAL then
																				make_parse_tree s (to_be_sent@[head]) (count) tail
																		else (print head;raise error_in_tree_construction)
																		)
																	else
																		[Node (s,(make_parse_tree "" [] 0 ((to_be_sent))))]@(make_parse_tree "" [] 0 tail)
																	)
																else
																	((*print ("To be sent: "^(foldr op^ "" to_be_sent)^" "^Int.toString(count)^"\n");*)(if head = "[" then
																		if to_be_sent = [] then
																			make_parse_tree s (to_be_sent) (count+1) tail
																		else
																			make_parse_tree s (to_be_sent@[head]) (count+1) tail
																	else if head = "," then
																		[Node (s,(make_parse_tree "" [] 0 ((to_be_sent))))]@(make_parse_tree "" [] 0 tail)
																	else if String.compare(head,"]") <> EQUAL then
																		if to_be_sent = [] then
																			make_parse_tree head (to_be_sent) (count) tail
																		else
																			make_parse_tree s (to_be_sent@[head]) (count) tail
																	else ((*print head;*)raise error_in_tree_construction)
																	))
																)
														else
															((*print ("To be sent: "^(foldl op^ "" to_be_sent)^" "^Int.toString(count)^"\n");*)(
																if head = "[" then
																	make_parse_tree s (to_be_sent@[head]) (count+1) tail
																else if head = "]" then
																	make_parse_tree s (to_be_sent@[head]) (count-1) tail
																else make_parse_tree s (to_be_sent@[head]) (count) tail
															))


val (parse_tree) = make_parse_tree "" [] 0 parse_tree_str_list;

(*conversion to abstract syntax tree*)
fun make_AST (t: treeNode) = 
	case t of
		Node (s,l) => 
			(print (s^"\n");print (foldr op^ "" ((map ret_str l))^"\n");(case s of
				"Program" => make_AST (hd l)
				| "Block" => [Node("Block",foldr op@ [] (map make_AST l))]
				| "DeclarationSeq" => 
						(case l of
							(Node("VarDecls",[(Node("IntVarDecls",[Node("EPSILON",[])])),(Node("BoolVarDecls",[Node("EPSILON",[])]))]))::tail =>
									(
										case tail of
											[Node("ProcDecls",[Node("EPSILON",[])])] => [Node("Declarations",[])]
											| _ => [Node("Declarations",foldr op@ [] (map make_AST tail))]
									)
							| (Node("VarDecls",lst))::tail =>
									(
										case tail of
											[Node("ProcDecls",[Node("EPSILON",[])])] => [Node("Declarations",make_AST (Node("VarDecls",lst)))]
											| _ => [Node("Declarations",foldr op@ [] (map make_AST l))]
									)
						)
				| "VarDecls" =>
						(case l of
							(Node("IntVarDecls",[Node("EPSILON",[])]))::tail =>
									(
										[Node("Variables",foldr op@ [] (map make_AST tail))]
									)
							| (Node("IntVarDecls",lst))::tail =>
									(
										case tail of
											[Node("BoolVarDecls",[Node("EPSILON",[])])] => [Node("Variables",make_AST (Node("IntVarDecls",lst)))]
											| _ => [Node("Variables",foldr op@ [] (map make_AST l))]
									)
						) 
				| "IntVarDecls" => [Node("INT",foldr op@ [] (map make_AST (tl l)))]
				| "BoolVarDecls" => [Node("BOOL",foldr op@ [] (map make_AST (tl l)))]
				| "VarDef" => foldr op@ [] (map make_AST l)
				| "VarDef1" => 
						(
							case l of
								[Node("EOS",[])] => []
								| _ => make_AST (hd (tl l))
						) 
				| "Ident" => l
				| "ProcDecls" => 
						(
							case l of
								[Node("EPSILON",[])] => []
								| _ => [Node("PROC",(make_AST (hd (tl l)))@(make_AST (hd (tl (tl l)))))]@(make_AST (hd (tl (tl (tl l)))))
						)
				| "CommandSeq" => [Node("CommandSequence",make_AST (hd (tl l)))]
				| "Command" => 
						(
							case l of
								[Node("EPSILON",[])] => [] 
								| _ => foldr op@ [] (map make_AST l)
						)
				| "AssignmentCmd" => [Node("Assign",(make_AST (hd l))@(make_AST (hd (tl (tl l)))))]
				| "CallCmd" => [Node("Call",(make_AST (hd (tl l))))]
				| "ReadCmd" => [Node("Read",(make_AST (hd (tl (tl l)))))]
				| "PrintCmd" => [Node("Print",(make_AST (hd (tl (tl l)))))]
				| "ConditionalCmd" => [Node("ConditionalCommand",([Node("If",make_AST (hd (tl l)))])@([Node("Then",(make_AST (hd (tl (tl (tl l))))))])@([Node("Else",(make_AST (hd (tl (tl (tl (tl (tl l))))))))]))]
				| "WhileCmd" => [Node("While",(make_AST (hd (tl l)))@(make_AST (hd (tl (tl l)))))]
				| "Expression" => make_AST (hd l)
				| "BoolExpression" => (*TODO:Start here for expression*)
						(
							case l of
								[(Node("BoolF",bfl)),(Node("BoolE",[Node("EPSILON",[])]))] => make_AST (Node("BoolF",bfl))
								| [(Node("BoolF",bfl)),(Node("BoolE",[Node("OR",orl),Node("BoolExpression",bel)]))] => [Node("OR",(make_AST (Node("BoolF",bfl)))@(make_AST (Node("BoolExpression",bel))))]
						)
				| "BoolF" => 
						(
							case l of
								[(Node("BoolG",bfl)),(Node("BoolF1",[Node("EPSILON",[])]))] => make_AST (Node("BoolG",bfl))
								| [(Node("BoolG",bfl)),(Node("BoolF1",[Node("AND",orl),Node("BoolF",bel)]))] => [Node("AND",(make_AST (Node("BoolG",bfl)))@(make_AST (Node("BoolF",bel))))]
								(*| _ => [Node("AND",(make_AST (hd l))@(make_AST (hd (tl (ret_node_list (hd (tl l)))))))]*)
						)
				| "BoolG" => 
						(
							case l of
								[(Node("BoolH",bfl)),(Node("BoolG1",[Node("EPSILON",[])]))] => make_AST (Node("BoolH",bfl))
								| [(Node("BoolH",bfl)),(Node("BoolG1",bgl))] => 
									(
										case (bgl) of
											[Node(op_type,eql),Node("BoolG",bgl)] => ((*print ("List: "^(ret_str (hd (tl (ret_node_list (hd (tl l)))))));*)
																	[Node(op_type,(make_AST (Node("BoolH",bfl)))@(make_AST (Node("BoolG",bgl))))])
											(*| Node("NE",eql) => (print ("List: "^(ret_str (hd (tl (ret_node_list (hd (tl l)))))));[Node("NE",(make_AST (hd l))@(make_AST (hd (tl (ret_node_list (hd (tl l)))))))])*)
									)
						)
				| "BoolH" => 
						(
							case l of
								[(Node("BoolI",bfl)),(Node("BoolH1",[Node("EPSILON",[])]))] => make_AST (Node("BoolI",bfl))
								| [(Node("BoolI",bfl)),(Node("BoolH1",bgl))] => 
									(
										case (bgl) of
											[Node(op_type,eql),Node("BoolH",bhl)] => ((*print ("List: "^(ret_str (hd (tl (ret_node_list (hd (tl l)))))));*)
																	[Node(op_type,(make_AST (Node("BoolI",bfl)))@(make_AST (Node("BoolH",bhl))))])
									)
						)
				| "BoolI" => 
						(
							case l of
								[(Node("NEG",negl)),(Node("BoolJ",bjl))] => [Node("NEG",make_AST (Node("BoolJ",bjl)))]
								| [(Node("BoolJ",bjl))] => make_AST (Node("BoolJ",bjl))
						)
				| "BoolJ" => 
						(
							case l of
								[(Node("BoolLiteral",bll))] => (print (foldr op^ "" (map ret_str bll));bll)
								| [(Node("IntExpression",iel))] => make_AST (Node("IntExpression",iel))
						)
				| "IntExpression" => (*TODO:Start here for Int expression*)
						(
							case l of
								[(Node("IntT",bfl)),(Node("IntE",[Node("EPSILON",[])]))] => make_AST (Node("IntT",bfl))
								| [(Node("IntT",bfl)),(Node("IntE",[Node(op_type,opl),Node("IntExpression",iel)]))] => ((*print ("List: "^(ret_str (hd (tl (ret_node_list (hd (tl l)))))));*)
																			[Node(op_type,(make_AST (Node("IntT",bfl)))@(make_AST (Node("IntExpression",iel))))])
						)
				| "IntT" =>
						(
							case l of
								[(Node("IntF",bfl)),(Node("IntT1",[Node("EPSILON",[])]))] => make_AST (Node("IntF",bfl))
								| [(Node("IntF",bfl)),(Node("IntT1",[Node(op_type,opl),Node("IntT",iel)]))] => ((*print ("List: "^(ret_str (hd (tl (ret_node_list (hd (tl l)))))));*)
																			[Node(op_type,(make_AST (Node("IntF",bfl)))@(make_AST (Node("IntT",iel))))])
						)
				| "IntF" => 
						(
							case l of
								[(Node("UNMINUS",unml)),(Node("IntF1",bjl))] => [Node("UNMINUS",make_AST (Node("IntF1",bjl)))]
								| [(Node("IntF1",bjl))] => make_AST (Node("IntF1",bjl))
								| _ => (print ("Non-Exhaustive Match: "^(foldr op^ "" (map ret_str l)));[Node(s,[])])
						)
				| "IntF1" => 
						(
							case l of
								[(Node("IntLiteral",bll))] => bll
								| [(Node("Ident",bll))] => make_AST (Node("Ident",bll))
								| _ => (print (foldr op^ "" (map ret_str l));make_AST (hd (tl l)))
						)
				| "EOS" => []
				(*| _ => [Node(s,[])]*)
			)
			)
		| _ => raise error_in_tree_construction

val ast = make_AST (hd parse_tree);

fun print_tree ((Node(x,xs::ys))::[]) = x^"\n[\n"^(print_tree (xs::ys))^"\n]\n"
	| print_tree ((Node(x,xs::ys))::tl1::tl2) =  x^"\n[\n"^(print_tree (xs::ys))^"\n]\n,\n"^(print_tree (tl1::tl2))
	| print_tree ((Node(x,[]))::[]) = x
	| print_tree ((Node(x,[]))::tl1::tl2) =  x^"\n,\n"^(print_tree (tl1::tl2))

val ast_print = print_tree ast;

(*Write to file*)
fun write (filename: string, s) = 
    let 
    	val f =  TextIO.openAppend filename
    in  
    	(TextIO.output (f, s); TextIO.closeOut f) 
    end

val fio = TextIO.openAppend abstractSyntaxTreeOutput;
TextIO.closeOut fio;

val write_to_file = write (abstractSyntaxTreeOutput,ast_print);

val npt = "outputNormalParseTree.txt";

val fio1 = TextIO.openAppend npt;
TextIO.closeOut fio1;

val normal_parse_tree = write (npt,String.implode(input_parse_tree));

OS.Process.exit(OS.Process.success);