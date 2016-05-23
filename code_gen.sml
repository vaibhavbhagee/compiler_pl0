(*use "bigint.sml";*)
(*use "vm.sml";*)

val l = CommandLine.arguments();
val astInput = (hd l);
val symTableInput = (hd (tl l));
val irCodeOutput = (hd (tl (tl l)));
val programOutput = (hd (tl (tl (tl l))));

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
		| SOME (c, f') => loop (c::clist, f')
	      | NONE =>  (TextIO.StreamIO.closeIn; clist)
    in  rev(loop ([], f))
    end;

val input_as_tree = getclist(astInput);

fun conv_to_string_list [] curr_word = []
	| conv_to_string_list (x::y) curr_word = if x = #"[" orelse x = #"]" orelse x = #"," then
												if String.implode(curr_word) = "" then
													[Char.toString(x)]@(conv_to_string_list y [])
												else
													[(String.implode(curr_word))]@[Char.toString(x)]@(conv_to_string_list y [])
											else
												conv_to_string_list y (curr_word@[x])

val as_tree_str_list = conv_to_string_list input_as_tree [];

(*val as_tree_str_list = List.rev(tl (List.rev(tl as_tree_str_list)));*)

exception error_in_tree_construction;

(*Convert input tree to Parse Tree*)
fun make_as_tree s to_be_sent count [] = if to_be_sent <> [] then
												[Node (s,(make_as_tree "" [] 0 ((to_be_sent))))]
											else [Node (s,[])]
	| make_as_tree s to_be_sent count (head::tail) = if count = 1 then
																(if s = "" then
																	((*print ("To be sent: "^(foldr op^ "" to_be_sent)^" "^Int.toString(count)^"\n");*)(if head = "[" then
																		make_as_tree s (to_be_sent@[head]) (count+1) tail
																	else if head = "]" then
																		make_as_tree s (to_be_sent) (count-1) tail
																	else if head = "," then
																		make_as_tree s (to_be_sent@[head]) (count) tail
																	else 
																		make_as_tree head (to_be_sent@[head]) (count) tail
																	))
																else
																	((*print ("To be sent: "^(foldr op^ "" to_be_sent)^" "^Int.toString(count)^"\n");*)(if head = "[" then
																		make_as_tree s (to_be_sent@[head]) (count+1) tail
																	else if head = "]" then
																		make_as_tree s (to_be_sent) (count-1) tail
																	else make_as_tree s (to_be_sent@[head]) (count) tail
																	))
																)

														else if count = 0 then
																(if s <> "" then
																	((*print ("To be sent: "^(foldr op^ "" to_be_sent)^" "^Int.toString(count)^"\n");*)
																	if to_be_sent = [] andalso tail <> [] then
																		(if head = "[" then
																			make_as_tree s (to_be_sent) (count+1) tail
																		else if head = "," then
																			[Node (s,(make_as_tree "" [] 0 ((to_be_sent))))]@(make_as_tree "" [] 0 tail)
																		else if String.compare(head,"]") <> EQUAL then
																				make_as_tree s (to_be_sent@[head]) (count) tail
																		else (print head;raise error_in_tree_construction)
																		)
																	else
																		[Node (s,(make_as_tree "" [] 0 ((to_be_sent))))]@(make_as_tree "" [] 0 tail)
																	)
																else
																	((*print ("To be sent: "^(foldr op^ "" to_be_sent)^" "^Int.toString(count)^"\n");*)(if head = "[" then
																		if to_be_sent = [] then
																			make_as_tree s (to_be_sent) (count+1) tail
																		else
																			make_as_tree s (to_be_sent@[head]) (count+1) tail
																	else if head = "," then
																		[Node (s,(make_as_tree "" [] 0 ((to_be_sent))))]@(make_as_tree "" [] 0 tail)
																	else if String.compare(head,"]") <> EQUAL then
																		if to_be_sent = [] then
																			make_as_tree head (to_be_sent) (count) tail
																		else
																			make_as_tree s (to_be_sent@[head]) (count) tail
																	else ((*print head;*)raise error_in_tree_construction)
																	))
																)
														else
															((*print ("To be sent: "^(foldl op^ "" to_be_sent)^" "^Int.toString(count)^"\n");*)(
																if head = "[" then
																	make_as_tree s (to_be_sent@[head]) (count+1) tail
																else if head = "]" then
																	make_as_tree s (to_be_sent@[head]) (count-1) tail
																else make_as_tree s (to_be_sent@[head]) (count) tail
															))


val as_tree = (hd (make_as_tree "" [] 0 as_tree_str_list));

exception variable_not_found;

fun get_ident_type name [] = (print name;raise variable_not_found)
	| get_ident_type name ((a,b,_)::xs) = if name = a then
											b 
									else
										get_ident_type name xs;


exception type_mismatch;

fun get_type (Node(a,[Node("",[])])) symtable = if a = "tt" orelse a = "ff" then
										"bool"
									else if Int.fromString(a) <> NONE then
										"int"
									else get_ident_type a symtable
	| get_type (Node(a,[])) symtable = if a = "tt" orelse a = "ff" then
										"bool"
									else if Int.fromString(a) <> NONE then
										"int"
									else get_ident_type a symtable 
	| get_type (Node(a,x::xs)) symtable = if a = "UNMINUS" orelse a = "NEG" then
											get_type x symtable
									(*else if a = "tt" orelse a = "ff" then
										"bool"
									else if Int.fromString(a) <> NONE then
										"int"*)									
									else
										(
											let
											 	val left_type = (get_type x symtable)
											 	val right_type = (get_type (hd xs) symtable)
											 in
											 	case a of
											 		"BINADD" => (
																 	if left_type = "int" andalso right_type = "int" then
																 		"int"
																 	else
																 		raise type_mismatch
																)
											 		| "BINSUB" => (
																 	if left_type = "int" andalso right_type = "int" then
																 		"int"
																 	else
																 		raise type_mismatch
																)
											 		| "BINMUL" => (
																 	if left_type = "int" andalso right_type = "int" then
																 		"int"
																 	else
																 		raise type_mismatch
																)
											 		| "BINDIV" => (
																 	if left_type = "int" andalso right_type = "int" then
																 		"int"
																 	else
																 		raise type_mismatch
																)
											 		| "BINMOD" => (
																 	if left_type = "int" andalso right_type = "int" then
																 		"int"
																 	else
																 		raise type_mismatch
																)
											 		| "AND" => (
																 	if left_type = "bool" andalso right_type = "bool" then
																 		"bool"
																 	else
																 		raise type_mismatch
																)
											 		| "OR" => (
																 	if left_type = "bool" andalso right_type = "bool" then
																 		"bool"
																 	else
																 		raise type_mismatch
																)
											 		| "LT" => (
																 	if left_type = right_type then
																 		"bool"
																 	else
																 		raise type_mismatch
																)
											 		| "LTE" => (
																 	if left_type = right_type then
																 		"bool"
																 	else
																 		raise type_mismatch
																)
											 		| "GT" => (
																 	if left_type = right_type then
																 		"bool"
																 	else
																 		raise type_mismatch
																)
											 		| "GTE" => (
																 	if left_type = right_type then
																 		"bool"
																 	else
																 		raise type_mismatch
																)
											 		| "EQ" => (
																 	if left_type = right_type then
																 		"bool"
																 	else
																 		raise type_mismatch
																)
											 		| "NE" => (
																 	if left_type = right_type then
																 		"bool"
																 	else
																 		raise type_mismatch
																)
											 end 
										);

exception incorrect_ast;

fun get_first (a,b,c) = a;

fun get_sec (a,b,c) = b;

fun get_third (a,b,c) = c;

fun check_membership m [] = false
	| check_membership m (x::y) = if (get_first (x)) = m then 
										true
								else if (get_first (x)) = "PROC_CALL" then
									false
								else
									check_membership m y;

exception variable_already_declared;

fun add_int_names [] s_table = s_table
	| add_int_names (nx::ny) s_table = if (check_membership nx s_table) = false then
										add_int_names (ny) ((nx,"int",false)::s_table)
									else 
										raise variable_already_declared;

fun add_bool_names [] s_table = s_table
	| add_bool_names (nx::ny) s_table = if (check_membership nx s_table) = false then
										add_int_names (ny) ((nx,"bool",false)::s_table)
									else 
										raise variable_already_declared;

fun restore_symbol_table [] = []
	| restore_symbol_table (x::y) = if (get_first x) = "PROC_CALL" then
										(x::y) (*To retain the declaration of the PROC*)
									else
										restore_symbol_table y;

exception ite_exp_not_bool;
exception while_exp_not_bool;
exception var_not_assigned_before_use;

fun check_ast_list symtable [] = symtable
	| check_ast_list symtable (x::y) = let
										val output = check_ast symtable x
										(*Add code to undo changes due to previous method*)
									in
										check_ast_list output y
									end

and check_ast symtable (Node(a,l)) = 
		(
			case a of
				"Block" =>  (
								case l of
								(xs::ys) => let
											val output = check_ast symtable xs
										in
											check_ast output (hd ys)
										end
								| [] => raise incorrect_ast
							)
				| "Declarations" => (
										case l of
											((Node("Variables",lst))::tail) => let
																				val output = check_ast symtable (Node("Variables",lst))
																				val lt = check_ast_list output tail
																			in
																				lt
																			end
											| ((Node("PROC",lst))::tail) => let
																			val out_lst = check_ast_list symtable l
																		in
																			out_lst
																		end
											| _ => symtable
									)
				| "Variables" =>    (
										case l of
											([Node("INT",lst),Node("BOOL",lst_bool)]) => 
																let
																	val output = add_int_names (map ret_str lst) symtable
																	val lt = add_bool_names (map ret_str lst_bool) output
																in
																	lt
																end
											| [(Node("INT",lst))] => 
																let
																	val output = add_int_names (map ret_str lst) symtable
																in
																	output
																end
											| [(Node("BOOL",lst))] => 
																let
																	val output = add_bool_names (map ret_str lst) symtable
																in
																	output
																end
									)
				| "PROC" => restore_symbol_table (check_ast (("PROC_CALL",(ret_str (hd l)),false)::symtable) (hd (tl l)))
				| "CommandSequence" => check_ast_list symtable l
				| "ConditionalCommand" => (
											case l of
												[Node("If",lst_if),Node("Then",lst_then),Node("Else",lst_else)] =>
															if (get_type (hd lst_if) symtable) = "bool" then
																let
																	val out_list = List.take ((List.rev (check_ast_list symtable lst_then)),(List.length symtable))
																	val out_list_final = List.take ((List.rev (check_ast_list out_list lst_else)),(List.length out_list))
																in
																	out_list_final
																end
															else
																raise ite_exp_not_bool
										)
				| "Assign" => (
								case l of
									[Node(a,lst_lhs),Node(b,lst_rhs)] =>
											if (get_type (Node(a,lst_lhs)) symtable) = (get_type (Node(b,lst_rhs)) symtable) then
												let
													fun assign_var aa [] = raise variable_not_found
														| assign_var aa (stx::sty) = if (get_first stx) = aa then
																						(get_first stx,get_sec stx,true)::sty
																					else
																						stx::(assign_var aa sty)
												in
													assign_var a symtable
												end
											else raise type_mismatch
							)
				| "Print" => (
								case l of
									[Node(a,lst)] => (*let
														fun get_var_assign aa [] = raise variable_not_found
														| get_var_assign aa (stx::sty) = if (get_first stx) = aa then
																						(get_third stx)
																					else
																						(get_var_assign aa sty)
													in
														if (get_var_assign a symtable) = true then*)
															symtable
														(*else
															raise var_not_assigned_before_use
													end*)
							)
				| "Read" => (
								case l of
									[Node(a,lst)] => let
														val type_a = get_type (Node(a,lst)) symtable
													in
														symtable
													end
							)
				| "Call" => (
								case l of
									[Node(a,lst)] => let
														fun check_proc_membership aa [] = raise variable_not_found
															| check_proc_membership aa (pmx::pmy) = 
																if (get_first pmx) = "PROC_CALL" andalso (get_sec pmx) = aa then
																	true
																else
																	check_proc_membership aa pmy

													in
														if (check_proc_membership a symtable) = true then
															symtable
														else
															raise variable_not_found
													end
							)
				| "While" => (
											case l of
												[Node(k,lst_lhs),Node(ex,lst_rhs)] =>
															if (get_type (Node(k,lst_lhs)) symtable) = "bool" then
																let
																	val out_list = List.take ((List.rev (check_ast symtable (Node(ex,lst_rhs)))),(List.length symtable))
																in
																	out_list
																end
															else
																raise while_exp_not_bool
										)
				| _ => raise error_in_tree_construction
		);

(*TODO: Debug *)
val ret_sym_table = check_ast [] as_tree;

(* Code for Code Generation *)

fun gen_decl_list_int a = ("DECLARE_INT "^a^" _ _");

fun gen_decl_list_bool a = ("DECLARE_BOOL "^a^" _ _");

fun gen_code_exp (Node(a,[Node("",[])])) symtable temp_var_no = 
									if a = "tt" orelse a = "ff" then
										(["DECLARE_BOOL *temp"^(Int.toString(temp_var_no))^" _ _"]@["ASSIGN "^a^" _ *temp"^(Int.toString(temp_var_no))],("*temp"^(Int.toString(temp_var_no))),(temp_var_no+1))
									else if Int.fromString(a) <> NONE then
										(["DECLARE_INT *temp"^(Int.toString(temp_var_no))^" _ _"]@["ASSIGN "^a^" _ *temp"^(Int.toString(temp_var_no))],("*temp"^(Int.toString(temp_var_no))),(temp_var_no+1))
									else ([],a,(temp_var_no)) 
	| gen_code_exp (Node(a,[])) symtable temp_var_no = 
									if a = "tt" orelse a = "ff" then
										(["DECLARE_BOOL *temp"^(Int.toString(temp_var_no))^" _ _"]@["ASSIGN "^a^" _ *temp"^(Int.toString(temp_var_no))],("*temp"^(Int.toString(temp_var_no))),(temp_var_no+1))
									else if Int.fromString(a) <> NONE then
										(["DECLARE_INT *temp"^(Int.toString(temp_var_no))^" _ _"]@["ASSIGN "^a^" _ *temp"^(Int.toString(temp_var_no))],("*temp"^(Int.toString(temp_var_no))),(temp_var_no+1))
									else ([],a,(temp_var_no)) 
	| gen_code_exp (Node(a,x::xs)) symtable temp_var_no = 
									(case a of
										"UNMINUS" => 
											let
												val (code,temp_var_name,temp_var_out) = gen_code_exp x symtable temp_var_no
											in
												(code@["DECLARE_INT *temp"^(Int.toString(temp_var_out))^" _ _"]@["UMINUS"^" "^temp_var_name^" _ *temp"^(Int.toString(temp_var_out))],("*temp"^(Int.toString(temp_var_out))),(temp_var_out+1))
											end

										| "NEG" => 
											let
												val (code,temp_var_name,temp_var_out) = gen_code_exp x symtable temp_var_no
											in
												(print (ret_str x);((code@["DECLARE_BOOL *temp"^(Int.toString(temp_var_out))^" _ _"]@["NOT"^" "^temp_var_name^" _ *temp"^(Int.toString(temp_var_out))]),("*temp"^(Int.toString(temp_var_out))),(temp_var_out+1)))
											end
										
										| "BINADD" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_INT *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["PLUS"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 

										| "BINSUB" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_INT *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["MINUS"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 

										| "BINMUL" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_INT *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["MULT"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 


										| "BINDIV" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_INT *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["DIV"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 

										| "BINMOD" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_INT *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["MOD"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 

										| "GTE" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_BOOL *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["GEQ"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 

										| "GT" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_BOOL *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["GT"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 

										| "LTE" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_BOOL *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["LEQ"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 


										| "LT" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_BOOL *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["LT"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 

										| "NE" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_BOOL *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["NEQ"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 

										| "EQ" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_BOOL *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["EQ"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 

										| "AND" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_BOOL *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["AND"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 

										| "OR" =>
											let
											 	val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp x symtable temp_var_no
											 	val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (hd xs) symtable left_temp_var_out
											 in
											 	(left_code@right_code@["DECLARE_BOOL *temp"^(Int.toString(right_temp_var_out))^" _ _"]@["OR"^" "^left_temp_var_name^" "^right_temp_var_name^" *temp"^(Int.toString(right_temp_var_out))],("*temp"^(Int.toString(right_temp_var_out))),(right_temp_var_out+1))
											 end 
										);

fun check_membership_proc m [] = (print m;raise variable_not_found)
	| check_membership_proc m (x::y) = if (get_sec (x)) = m then 
										(get_third (x))
								else
									check_membership_proc m y;

fun gen_code_proc symtable [] = []
	| gen_code_proc symtable (x::y) = ["DECLARE_PROC "^x^" "^(check_membership_proc x symtable)^" _"]@(gen_code_proc symtable y)

fun gen_code_list symtable [] next_inst_no temp_var_no = ([],symtable,next_inst_no,temp_var_no)
	| gen_code_list symtable (x::y) next_inst_no temp_var_no = 
									let
										val (code,symtable_out,ins_no_out,temp_var_out) = gen_code symtable x next_inst_no temp_var_no
										val (code_out,out_symtable,ins_out,temp_out) = (gen_code_list symtable_out y (*ins_no_out*)(next_inst_no+List.length(code)) temp_var_out)
										(*Add code to undo changes due to previous method*)
										val l_out = code@code_out
									in
										(l_out,out_symtable,(*ins_out*)(next_inst_no+List.length(l_out)),temp_out)
									end

and gen_code symtable (Node(a,l)) next_inst_no temp_var_no = 
		(
			case a of
				"Block" =>  (
								case l of
								(xs::ys) => let
											val (code,symtable_out,ins_no_out,temp_var_out) = gen_code_list symtable (xs::ys) next_inst_no temp_var_no
										in
											(code,symtable_out,(next_inst_no+List.length(code)),temp_var_out)
										end
								| [] => raise incorrect_ast
							)
				| "Declarations" => (
										case l of
											((Node("Variables",lst))::tail) => let
																				(*val (code,symtable_out,ins_no_out,temp_var_out) = gen_code symtable (Node("Variables",lst)) next_inst_no temp_var_no
																				val lt = gen_code_list symtable_out tail ins_no_out temp_var_out*)
																				val name_list = map ret_str (map hd (map ret_node_list tail))
																				val (retvar_code,symtable_out,ins_out,temp_var_out) = gen_code symtable (hd l) (next_inst_no) temp_var_no
																				val (code,ret_symtable_out,ret_ins_out,ret_temp_var_out) = gen_code_list symtable_out tail (next_inst_no+List.length(retvar_code)+List.length(name_list)) temp_var_out
																				val decl_code = gen_code_proc ret_symtable_out name_list
																				val l_out = retvar_code@decl_code@["GOTO _ "^(Int.toString(next_inst_no+List.length(retvar_code)+List.length(decl_code)+List.length(code)))^" _"]@code
																			in
																				(l_out,ret_symtable_out,(next_inst_no+List.length(l_out)),ret_temp_var_out)
																			end
											| ((Node("PROC",lst))::tail) => let
																				val name_list = map ret_str (map hd (map ret_node_list l))
																				val (retvar_code,symtable_out,ins_out,temp_var_out) = ([],symtable,next_inst_no,temp_var_no)
																				val (code,ret_symtable_out,ret_ins_out,ret_temp_var_out) = gen_code_list symtable_out l (next_inst_no+List.length(retvar_code)+List.length(name_list)) temp_var_out
																				val decl_code = gen_code_proc ret_symtable_out name_list
																				val l_out = retvar_code@decl_code@["GOTO _ "^(Int.toString(next_inst_no+List.length(retvar_code)+List.length(decl_code)+List.length(code)))^" _"]@code
																		in
																			(l_out,ret_symtable_out,(next_inst_no+List.length(l_out)),ret_temp_var_out)
																		end
											| _(*Changed here*) => ([(*"GOTO _ "^(Int.toString(next_inst_no+1))^" _"*)],symtable,next_inst_no,temp_var_no)
									)
				| "Variables" =>    (
										case l of
											([Node("INT",lst),Node("BOOL",lst_bool)]) => 
																let
																	val output = map gen_decl_list_int (map ret_str lst)
																	val lt = output@(map gen_decl_list_bool (map ret_str lst_bool))
																in
																	(lt,symtable,(next_inst_no + List.length(lt)),temp_var_no)
																end
											| [(Node("INT",lst))] => 
																let
																	val lt = map gen_decl_list_int (map ret_str lst)
																in
																	(lt,symtable,(next_inst_no + List.length(lt)),temp_var_no)
																end
											| [(Node("BOOL",lst))] => 
																let
																	val lt = map gen_decl_list_bool (map ret_str lst)
																in
																	(lt,symtable,(next_inst_no + List.length(lt)),temp_var_no)
																end
									)
				| "PROC" => let
								val (code,symtable_out,next_ins_out,temp_var_out) = (gen_code ([("PROC_CALL",(ret_str (hd l)),Int.toString(next_inst_no))]@symtable) (hd (tl l)) (next_inst_no+1) temp_var_no)
							in
								(code@["RETURN _ _ _"],symtable_out,(next_inst_no+List.length(code)+1),temp_var_out)
							end
				| "CommandSequence" => 
							let
								val (code,symtable_out,next_ins_out,temp_var_out) = gen_code_list symtable l next_inst_no temp_var_no
							in
								(code,symtable_out,(next_inst_no+List.length(code)),temp_var_out)
							end
				| "ConditionalCommand" => (
											case l of
												[Node("If",lst_if),Node("Then",lst_then),Node("Else",lst_else)] =>
															let
																val (exp_code,exp_var_name,exp_var_out) = gen_code_exp (hd lst_if) symtable temp_var_no
																val (then_code,then_symtable,then_next_ins,then_temp_var_no) = gen_code symtable (hd lst_then) (next_inst_no+(List.length(exp_code))+1) exp_var_out
																val (else_code,else_symtable,else_next_ins,else_temp_var_no) = gen_code then_symtable (hd lst_else) (next_inst_no+List.length(exp_code)+1+List.length(then_code)+1) then_temp_var_no
																val l_out = exp_code@["IF "^exp_var_name^" "^(Int.toString((*then_next_ins*)(*+List.length(then_code)*)next_inst_no+List.length(exp_code)+List.length(then_code)+1(*changed from 2*)))^" _"]@then_code@["GOTO _ "^(Int.toString((*else_next_ins*)next_inst_no+List.length(exp_code)+List.length(then_code)+List.length(else_code)+1(*changed from 2*)))^" _"]@else_code
															in
																(l_out,else_symtable,(*else_next_ins*)(next_inst_no+List.length(l_out)),else_temp_var_no)
															end
										)
				| "Assign" => (
								case l of
									[Node(a,lst_lhs),Node(b,lst_rhs)] =>
										
												let
													val (left_code,left_temp_var_name,left_temp_var_out) = gen_code_exp (Node(a,lst_lhs)) symtable temp_var_no
											 		val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (Node(b,lst_rhs)) symtable left_temp_var_out 
													val l_out = left_code@right_code@["ASSIGN "^right_temp_var_name^" _ "^left_temp_var_name]
												in
													(left_code@right_code@["ASSIGN "^right_temp_var_name^" _ "^left_temp_var_name],symtable,(next_inst_no+List.length(l_out)),right_temp_var_out)
												end
											
							)
				| "Print" => (
								case l of
									[Node(a,lst)] => let
														val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (Node(a,lst)) symtable temp_var_no 
														val l_out = right_code@["PRINT "^right_temp_var_name^" _ _"]
													in
														(l_out,symtable,(next_inst_no+List.length(l_out)),right_temp_var_out)
													end
							)
				| "Read" => (
								case l of
									[Node(a,lst)] => let
														val (right_code,right_temp_var_name,right_temp_var_out) = gen_code_exp (Node(a,lst)) symtable temp_var_no
														val l_out = right_code@["READ _ _ "^right_temp_var_name]
													in
														(l_out,symtable,(next_inst_no+List.length(l_out)),right_temp_var_out)
													end
							)
				| "Call" => (
								case l of
									[Node(a,lst)] => (["CALL "^a^" _ _"],symtable,(next_inst_no+1),temp_var_no)
							)
				| "While" => (
											case l of
												[Node(k,lst_lhs),Node(ex,lst_rhs)] =>
															let
																val (exp_code,exp_var_name,exp_var_out) = gen_code_exp (Node(k,lst_lhs)) symtable temp_var_no
																val (then_code,then_symtable,then_next_ins,then_temp_var_no) = gen_code symtable (Node(ex,lst_rhs)) (next_inst_no+List.length(exp_code)+1) exp_var_out
																(*val (else_code,else_symtable,else_next_ins,else_temp_var_no) = gen_code (hd lst_else) then_symtable (then_next_ins+List.length(then_code)) then_temp_var_out*)
																val l_out = exp_code@["IF "^exp_var_name^" "^(Int.toString((*then_next_ins*)(*+List.length(then_code)*)next_inst_no+List.length(exp_code)+1(*changed from 1*)+List.length(then_code)))^" _"]@then_code@["GOTO _ "^(Int.toString((next_inst_no-1)))^" _"]
															in
																(l_out,then_symtable,(*(then_next_ins+1)*)(next_inst_no+List.length(l_out)),then_temp_var_no)
															end
										)
				| _ => raise error_in_tree_construction
		);

val generated_code = gen_code [] as_tree 1 0;

fun write (filename: string, s) = 
    let val f =  TextIO.openOut filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end;

fun add_cr s = (s^"\n");

val to_print = foldr op^ "" (map add_cr ((#1 generated_code)@["END_OF_CODE _ _ _"]));

val write_file = write (irCodeOutput,to_print);

use "vm.sml";

OS.Process.exit(OS.Process.success);