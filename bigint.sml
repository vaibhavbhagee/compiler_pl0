signature BigInt =
sig
	type bigint
	val getbigint: int -> bigint
	val bi2str : bigint -> string
	val str2bi : string -> bigint
	val lt : bigint * bigint -> bool
	val leq : bigint * bigint -> bool
	val gt : bigint * bigint -> bool
	val geq : bigint * bigint -> bool
	val eq : bigint * bigint -> bool
	val neq : bigint * bigint -> bool
	val div4bigint : bigint * bigint -> bigint
	val mul : bigint * bigint -> bigint
	val add : bigint * bigint -> bigint
	val sub : bigint * bigint -> bigint
	val mod4bigint : bigint * bigint -> bigint
	val unminus : bigint -> bigint
end;

structure bigInt:BigInt = 
struct
	(* Body *)
	type bigint = string;

	exception div_by_zero;

	fun getbigint i = Int.toString(i)

	and bi2str i = i

	and str2bi i = i

	and preprocess (x::y) = if x = #"~" then
								(x::(preprocess y))
							else if x = #"0" then
								preprocess y
							else (x::y)
		| preprocess [] = [#"0"]

	(*Comparison functions*)

	and lt (a,b) = 
		let fun lt_list (lx::ly) (mx::my) = if lx = #"~" andalso mx = #"~" then
										gt (String.implode(ly),String.implode(my))
									else if lx = #"~" andalso mx <> #"~" then
										true
									else if lx <> #"~" andalso mx = #"~" then
										false
									else
										(
											if List.length(lx::ly) < List.length(mx::my) then
												true
											else if List.length(lx::ly) > List.length(mx::my) then
												false
											else
												(
													if Char.compare(lx,mx) = LESS then
														true
													else if Char.compare(lx,mx) = GREATER then
														false
													else (lt_list ly my)
												)
										) 
			| lt_list [] [] = false
		in lt_list (preprocess (String.explode(a))) (preprocess (String.explode(b)))
		end

	and gt (a,b) = 
		let fun gt_list (lx::ly) (mx::my) = if lx = #"~" andalso mx = #"~" then
										lt (String.implode(ly),String.implode(my))
									else if lx = #"~" andalso mx <> #"~" then
										false
									else if lx <> #"~" andalso mx = #"~" then
										true
									else
										(
											if List.length(lx::ly) < List.length(mx::my) then
												false
											else if List.length(lx::ly) > List.length(mx::my) then
												true
											else
												(
													if Char.compare(lx,mx) = GREATER then
														true
													else if Char.compare(lx,mx) = LESS then
														false
													else (gt_list ly my)
												)
										) 
			| gt_list [] [] = false
		in gt_list (preprocess (String.explode(a))) (preprocess (String.explode(b)))
		end

	and leq (a,b) = not (gt (a,b))
	and geq (a,b) = not (lt (a,b))

	and eq (a,b) = 
		let fun eq_list (lx::ly) (mx::my) = if lx = #"~" andalso mx = #"~" then
										eq (String.implode(ly),String.implode(my))
									else if lx = #"~" andalso mx <> #"~" then
										false
									else if lx <> #"~" andalso mx = #"~" then
										false
									else
										(
											if List.length(lx::ly) < List.length(mx::my) then
												false
											else if List.length(lx::ly) > List.length(mx::my) then
												false
											else
												(
													if Char.compare(lx,mx) = EQUAL then
														(eq_list ly my)
													else false
												)
										) 
				| eq_list [] [] = true
		in eq_list (preprocess (String.explode(a))) (preprocess (String.explode(b)))
		end

	and neq (a,b) = not (eq (a,b))

	(*Arithmetic Functions*)
	and unminus a = if (preprocess (String.explode(a)) = [#"0"]) then
						"0"
					else if hd (preprocess (String.explode(a))) = #"~" then
						String.implode(tl (preprocess (String.explode(a))))
					else
						String.implode([#"~"] @ (preprocess (String.explode(a)))) 

	(*Some processing functions*)
	and conv_to_int_list (x::y) = 
			let fun get_int a = (Char.ord(a) - 48) 
			in map get_int (x::y)
			end
		| conv_to_int_list [] = []

	and conv_to_char_list (x::y) = 
			let fun get_char a = Char.chr(a + 48) 
			in map get_char (x::y)
			end
		| conv_to_char_list [] = []

	and negate l = map ~ l

	and add (a,b) = 
		let fun add_char_list l m = if (hd l) = #"~" andalso (hd m) = #"~" then
										[#"~"] @ (add_char_list (tl l) (tl m))
									else if (hd l) = #"~" andalso (hd m) <> #"~" then
										(*String.explode(unminus (sub (String.implode(tl l),String.implode(m))))*)
										(
											if geq( String.implode(m), String.implode(tl l)) then
												conv_to_char_list (List.rev(add_int_list (List.rev(conv_to_int_list m)) (List.rev(negate (conv_to_int_list (tl l))))))
											else
												[#"~"] @ (conv_to_char_list (List.rev(add_int_list (List.rev(conv_to_int_list (tl l))) (List.rev(negate (conv_to_int_list (m)))))))
										)
									else if (hd l) <> #"~" andalso (hd m) = #"~" then
										(
											if geq( String.implode(l), String.implode(tl m)) then
												conv_to_char_list (List.rev(add_int_list (List.rev(conv_to_int_list l)) (List.rev(negate (conv_to_int_list (tl m))))))
											else
												[#"~"] @ (conv_to_char_list (List.rev(add_int_list (List.rev(conv_to_int_list (tl m))) (List.rev(negate (conv_to_int_list (l)))))))
										)
									else
										conv_to_char_list (List.rev(add_int_list (List.rev(conv_to_int_list l)) (List.rev(conv_to_int_list m))))
			
			and add_int_list (lx::ly) (mx::my) = normalize ([(lx+mx)] @ (add_int_list ly my))
				| add_int_list [] (mx::my) = (mx::my)
				| add_int_list (mx::my) [] = (mx::my)
				| add_int_list [] [] = []

			and normalize (x::y) = if (x div 10) = 0 then
										[(x mod 10)] @ (normalize y)
									else 
										[(x mod 10)] @ (add_int_list [(x div 10)] y)
				| normalize [] = []
		in String.implode(preprocess (add_char_list (preprocess (String.explode(a))) (preprocess (String.explode(b)))))
		end

	and sub (a,b) = (add (a,(unminus b))) 

	and mul (a,b) = 
		let fun mul_char_list l m = if (hd l) = #"~" andalso (hd m) = #"~" then
										(mul_char_list (tl l) (tl m))
									else if (hd l) = #"~" andalso (hd m) <> #"~" then
										[#"~"] @ (mul_char_list (tl l) (m))
									else if (hd l) <> #"~" andalso (hd m) = #"~" then
										[#"~"] @ (mul_char_list (l) (tl m))
									else
										conv_to_char_list (List.rev(mul_int_list (List.rev(conv_to_int_list l)) (List.rev(conv_to_int_list m))))
			
			and add_int_list (lx::ly) (mx::my) = normalize ([(lx+mx)] @ (add_int_list ly my))
				| add_int_list [] (mx::my) = (mx::my)
				| add_int_list (mx::my) [] = (mx::my)
				| add_int_list [] [] = []

			and normalize (x::y) = if (x div 10) = 0 then
										[(x mod 10)] @ (normalize y)
									else 
										[(x mod 10)] @ (add_int_list [(x div 10)] y)
				| normalize [] = []
			and mul_int_list (lx::ly) (mx::my) = normalize ([(lx*mx)] @ (add_int_list (mul_int_list (ly) [mx]) (mul_int_list (lx::ly) (my))))
				| mul_int_list [] (mx::my) = []
				| mul_int_list (mx::my) [] = []
				| mul_int_list [] [] = []
		in String.implode(preprocess (mul_char_list (preprocess (String.explode(a))) (preprocess (String.explode(b)))))
		end

	and repeat_sub (a,b) count = if (lt (a,b)) then
									(count,(conv_to_int_list (String.explode(a))))
								else
									repeat_sub (sub (a,b),b) (count+1)

	(*TODO: div by 0 exception*)
	(*TODO: minus 0 is 0*)

	and div4bigint (a,b) = 
		(if (preprocess (String.explode(b))) = [#"0"] then
			raise div_by_zero
		else
		(let fun div_char_list l m = if (hd l) = #"~" andalso (hd m) = #"~" then
										String.explode(unminus (div4bigint (String.implode(tl l),String.implode(tl m))))
									else if (hd l) = #"~" andalso (hd m) <> #"~" then
										[#"~"] @ String.explode(div4bigint (add (String.implode(tl l),mod4bigint (String.implode(l),String.implode(m))),String.implode(m)))
									else if (hd l) <> #"~" andalso (hd m) = #"~" then
										String.explode(div4bigint (add (String.implode(l),unminus(mod4bigint (String.implode(l),String.implode(m)))),(unminus (String.implode(m)))))
									else
										conv_to_char_list ((div_int_list [] ((conv_to_int_list l)) [] (String.implode(m))))

			and div_int_list (l) (rx::ry) q dvsr = 
						let val (a,b) = repeat_sub ((String.implode(conv_to_char_list ((l)@[rx]))),dvsr) 0
						in (div_int_list (b) (ry) (q @ [a]) dvsr)
						end
				| div_int_list _ [] q dvsr = q
		in String.implode(preprocess (div_char_list (preprocess (String.explode(a))) (preprocess (String.explode(b)))))
		end))

	and mod4bigint (a,b) = 
		(if (preprocess (String.explode(b))) = [#"0"] then
			raise div_by_zero
		else
		(let fun div_char_list l m = if (hd l) = #"~" andalso (hd m) = #"~" then
										String.explode(unminus (mod4bigint (String.implode(tl l),String.implode(tl m))))
									else if (hd l) = #"~" andalso (hd m) <> #"~" then
										(
											if preprocess (String.explode(mod4bigint (String.implode(tl l),String.implode(m)))) = [#"0"] then 
												[#"0"]
											else
												String.explode( sub(String.implode(m), mod4bigint (String.implode(tl l),String.implode(m))) )
										)
									else if (hd l) <> #"~" andalso (hd m) = #"~" then
										(
											if preprocess (String.explode(mod4bigint (String.implode(l),String.implode(tl m)))) = [#"0"] then
												[#"0"]
											else
												String.explode( add(String.implode(m), mod4bigint (String.implode(l),String.implode(tl m))) )

										)
									else
										conv_to_char_list ((div_int_list [] ((conv_to_int_list l)) [] (String.implode(m))))

			and div_int_list (l) (rx::ry) q dvsr = 
						let val (a,b) = repeat_sub ((String.implode(conv_to_char_list ((l)@[rx]))),dvsr) 0
						in (div_int_list (b) (ry) (q @ [a]) dvsr)
						end
				| div_int_list l [] q dvsr = l
		in String.implode(preprocess (div_char_list (preprocess (String.explode(a))) (preprocess (String.explode(b)))))
		end))
end;

open bigInt;

(*Test cases for div*)
(*div4bigint ("~123","23");
mod4bigint ("~123","23");
div4bigint ("123","~23");
mod4bigint ("123","~23");
div4bigint ("123","23");
mod4bigint ("123","23");
div4bigint ("~123","~23");
mod4bigint ("~123","~23");
unminus ("00000000000000000000000000000000000000000");
div4bigint ("~123","0");
mod4bigint ("~123","0");*)
(*div4bigint ("12345","15");
mod4bigint ("12345","15");
mul ("123456789","123456789");
mul (mul ("123456789","123456789"),mul ("123456789","123456789"));
mul (mul (mul ("123456789","123456789"),mul ("123456789","123456789")),mul (mul ("123456789","123456789"),mul ("123456789","123456789")));

fun big_fact (b:bigint) = if eq (b,(str2bi "0")) then
					(str2bi ("1"))
				else
					mul (b,(big_fact (sub (b,"1"))));

big_fact (str2bi "1000");*)
(*div4bigint ("550","11");*)