(*val l = CommandLine.arguments();
val inputFile = (hd l);
val outputFile = (hd (tl l));*)

(*open BigInt;*)
use "bigint.sml";

fun lick (filename:string) =
    let val f = TextIO.getInstream(TextIO.openIn filename)
	fun loop (accum: string list, f) =
	    case (TextIO.StreamIO.inputLine f) of 
	        SOME(chunk, f') => loop ((chunk::accum), f')
	      | NONE => (TextIO.StreamIO.closeIn f; accum)
            (* esac *)
    in  rev(loop ([], f))
    end;

val inp = lick irCodeOutput;

fun process_input s = 
	let fun process (x::xs) = if x = #"\n" orelse x = #"\r" then
						process xs
					else (x::(process xs))
			| process [] = []

	in process (String.explode(s))
	end;

fun process_input_in s = 
	let fun process (x::xs) = if x = #"\n" orelse x = #"\r" then
						process xs
					else (x::(process xs))
			| process [] = []

	in String.implode(process (String.explode(s)))
	end;

fun split_by_space curr_word (x::xs) = if x = #" " then
													[(String.implode(curr_word))]@(split_by_space [] xs)
												else (split_by_space (curr_word@[x]) xs)
	| split_by_space curr_word [] = [String.implode(curr_word)];												

(*RAM generation from input list*)
val ram = map (split_by_space []) (map process_input inp);

exception Not_found;

fun insert_in_symtable (name,value) (x::xs) = 
	(case x of
	(a,inp_val) => if a = name then
					((name,value)::xs)
				else
	(*| _ => *)		(x::(insert_in_symtable (name,value) xs)))
	| insert_in_symtable (name,value) [] = raise Not_found;;

fun ret_val a (xs::xss) = 
	(case xs of
	(a_x,val_a) => if a_x = a then 
						((*print ("value of "^a_x^": "^val_a^"\n");*)val_a)
					else
	(*| _ => *)			ret_val a xss)
	| ret_val a [] = (print a;raise Not_found);

fun ret_val_tuple a (xs::xss) = 
	(case xs of
	(a_x,val_a) =>  if a_x = a then 
						(val_a,xss)
	(*| _ => *)else
					ret_val_tuple a xss)
	| ret_val_tuple a [] = raise Not_found;

fun retint (SOME x) = x;

fun bin2str true = "tt"
	| bin2str false = "ff";

fun str2bool "tt" = true
	| str2bool "ff" = false;

fun str2int "tt" = "1"
	| str2int "ff" = "0"
	| str2int t = t;

exception InvalidInstruction;

fun print_s_table [] = ""
	| print_s_table ((a,b)::ys) = "("^a^","^b^") , "^(print_s_table ys);

(*Function to make executable*)
fun execute sym_table pc outstream = 
	let
		val l = List.nth (ram,pc)
		val syta = (print_s_table sym_table)
	in
		((*print ((syta)^"\n");*)
		(case (hd l) of
			"DECLARE_INT" => execute ([((hd (tl l)),"null")]@sym_table) (pc+1) outstream
			| "DECLARE_BOOL" => execute ([((hd (tl l)),"null")]@sym_table) (pc+1) outstream
			| "DECLARE_PROC" => execute ([((hd (tl l)),(hd (tl (tl l))))]@sym_table) (pc+1) outstream
			| "PRINT" => execute sym_table (pc+1) (outstream@[(ret_val (hd (tl l)) sym_table)])
			| "READ" => let
							val input_read = process_input_in (retint (TextIO.inputLine TextIO.stdIn))
						in
							execute ([((hd (tl (tl (tl l)))),input_read)]@sym_table) (pc+1) outstream
						end
			| "CALL" => execute ([("proc call",Int.toString(pc))]@sym_table) (retint (Int.fromString(ret_val (hd (tl l)) sym_table))) outstream
			| "IF" => let
						val obt_val = ret_val (hd (tl l)) sym_table
					in
						if obt_val = "tt" then
							execute sym_table (pc+1) outstream
						else
							execute sym_table (retint (Int.fromString(hd (tl (tl l))))) outstream
					end
			| "GOTO" => execute sym_table (retint (Int.fromString(hd (tl (tl l))))) outstream
			| "RETURN" => let
							val (pcs,rem_st) = ret_val_tuple "proc call" sym_table
						in
							execute rem_st ((retint (Int.fromString(pcs)))+1) outstream
						end
			| "ASSIGN" => if (Int.fromString(hd (tl l)) = NONE andalso ((hd (tl l))) <> "tt" andalso ((hd (tl l))) <> "ff") then
							execute (insert_in_symtable (hd (tl (tl (tl l))),(ret_val (hd (tl l)) sym_table)) sym_table) (pc+1) outstream
						else
							execute (insert_in_symtable (hd (tl (tl (tl l))),((hd (tl l)))) sym_table) (pc+1) outstream
			| "END_OF_CODE" => outstream
			| "PLUS" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v2 = str2int (ret_val (hd (tl (tl l))) sym_table)
							val v3 = bi2str (add (str2bi v1,str2bi v2))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "MINUS" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v2 = str2int (ret_val (hd (tl (tl l))) sym_table)
							val v3 = bi2str (sub (str2bi v1,str2bi v2))
							val i_dest = hd (tl (tl (tl l)))
						in
							((*print ("b: "^v3^" ;\n");*)execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream)
						end
			| "MULT" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v2 = str2int (ret_val (hd (tl (tl l))) sym_table)
							val v3 = bi2str (mul (str2bi v1,str2bi v2))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "DIV" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v2 = str2int (ret_val (hd (tl (tl l))) sym_table)
							val v3 = bi2str (div4bigint (str2bi v1,str2bi v2))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "MOD" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v2 = str2int (ret_val (hd (tl (tl l))) sym_table)
							val v3 = bi2str (mod4bigint (str2bi v1,str2bi v2))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "GEQ" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v2 = str2int (ret_val (hd (tl (tl l))) sym_table)
							val v3 = (bin2str (geq (str2bi v1,str2bi v2)))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "GT" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v2 = str2int (ret_val (hd (tl (tl l))) sym_table)
							val v3 = (bin2str (gt (str2bi v1,str2bi v2)))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "LEQ" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v2 = str2int (ret_val (hd (tl (tl l))) sym_table)
							val v3 = (bin2str (leq (str2bi v1,str2bi v2)))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "LT" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v2 = str2int (ret_val (hd (tl (tl l))) sym_table)
							val v3 = (bin2str (lt (str2bi v1,str2bi v2)))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "EQ" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v2 = str2int (ret_val (hd (tl (tl l))) sym_table)
							val v3 = (bin2str (eq (str2bi v1,str2bi v2)))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end 
			| "NEQ" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v2 = str2int (ret_val (hd (tl (tl l))) sym_table)
							val v3 = (bin2str (neq (str2bi v1,str2bi v2)))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "AND" => let
							val v1 = str2bool (ret_val (hd (tl l)) sym_table)
							val v2 = str2bool (ret_val (hd (tl (tl l))) sym_table)
							val v3 = (bin2str (v1 andalso v2))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "OR" => let
							val v1 = str2bool (ret_val (hd (tl l)) sym_table)
							val v2 = str2bool (ret_val (hd (tl (tl l))) sym_table)
							val v3 = (bin2str (v1 orelse v2))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "NOT" => let
							val v1 = str2bool (ret_val (hd (tl l)) sym_table)
							val v3 = (bin2str (not v1))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| "UNMINUS" => let
							val v1 = str2int (ret_val (hd (tl l)) sym_table)
							val v3 = (bi2str (unminus (v1)))
							val i_dest = hd (tl (tl (tl l)))
						in
							execute (insert_in_symtable (i_dest,v3) sym_table) (pc+1) outstream
						end
			| _ => raise InvalidInstruction))
	end;

val opstream = execute [] 0 [];

fun write (filename: string, s) = 
    let val f =  TextIO.openOut filename
    in  (TextIO.output (f, s); TextIO.closeOut f) 
    end;

fun add_cr s = (s^"\n");

val to_print = foldr op^ "" (map add_cr opstream);

val write_file = write (programOutput,to_print);

(*OS.Process.exit(OS.Process.success);*)