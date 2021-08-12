(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* val only_capitals = fn : string list -> string list *)			       
val only_capitals = List.filter (fn x => Char.isUpper (String.sub(x, 0))) 

(* val longest_string1 = fn : string list -> string *)
val longest_string1 = List.foldl (fn (x, acc) => if (String.size x) > (String.size acc) then x else acc) "" 

		
(* val longest_string2 = fn : string list -> string *)
val longest_string2 = List.foldl (fn (x, acc) => if (String.size x) >= (String.size acc) then x else acc) "" 

(* val longest_string_helper = fn : (int * int -> bool) -> string list -> string *)
fun longest_string_helper f =
    List.foldl (fn (x, acc) => if f(String.size x, String.size acc) then x else acc) "" 
	       
(* val longest_string3 = fn : string list -> string *)
val longest_string3 = longest_string_helper (fn (x,y) => x > y)
	       
(* val longest_string4 = fn : string list -> string *)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)					    

(* val longest_capitalized = fn : string list -> string *)
val longest_capitalized =  longest_string1 o only_capitals

(* val rev_string = fn : string -> string *)
val rev_string = String.implode o List.rev o String.explode					    

(* val first_answer = fn : (’a -> ’b option) -> ’a list -> ’b *)
fun first_answer f xs =
    let
	val ans = (List.map valOf) o (List.filter isSome) o (List.map f)
    in
	case ans xs of
	    [] => raise NoAnswer
	  | (x::xs) => x
    end
	
						 
(* val all_answers = fn : (’a -> ’b list option) -> ’a list -> ’b list option *)
fun all_answers f [] = SOME []
  | all_answers f xs =
    let
	val ans = List.map f xs;
    in
	if List.exists (not o isSome) ans
	then NONE
	else SOME (List.foldl (op @) [] (List.map valOf ans))
    end
	
(* val count_wildcards = fn : pattern -> int *)
fun count_wildcards (p:pattern) = g (fn () => 1) (fn (x) => 0) p
    
	
(*val count_wild_and_variable_lengths = fn : pattern -> int *)
fun count_wild_and_variable_lengths (p:pattern) =
    g (fn () => 1) (fn (s) => String.size s) p
      
(*val count_some_var = fn : string * pattern -> int *)
fun count_some_var (s:string, p:pattern) =
    g (fn () => 0) (fn (x) => if s = x then 1 else 0) p
      
(* val check_pat = fn : pattern -> bool *)
fun check_pat (p:pattern) =
    let
	fun collect_variable (p:pattern) =
	    case p of
		Variable x => [x]
	      | TupleP ps => List.foldl (fn (p, acc) => (collect_variable p) @ acc) [] ps
	      | ConstructorP (_, p) => collect_variable p
	      | _  => []

	fun is_repeats (xs:string list) =
	    case xs of
		[] => false
	      | [x] => false
	      | (x::xs') => List.exists (fn (y) => x = y) xs' orelse is_repeats (xs')
    in
	(not o is_repeats o collect_variable) p
    end
	
	    
	    
(* val match = fn : valu * pattern -> (string * valu) list option *)
fun match (v:valu, p:pattern) =
    case (p, v) of
	(UnitP, Unit) => SOME []
      | (ConstP n, Const m) => if n = m then SOME [] else NONE
      | (Wildcard, _ ) => SOME []
      | (Variable s, _ ) => SOME [(s, v)]
      | (TupleP ps, Tuple vs) => if (List.length ps) = (List.length vs)
				 then all_answers match (ListPair.zip (vs, ps))
				 else NONE
      | (ConstructorP (s1, p), Constructor (s2, v)) => if s1 = s2
						      then match (v, p)
						      else NONE
      | ( _ , _ ) => NONE
			 
						
(* val first_match = fn : valu -> pattern list -> (string * valu) list option *)      
fun first_match v ps =
    (SOME (first_answer (fn (x) => match (v, x)) ps)) handle NoAnswer => NONE
 
	
	
