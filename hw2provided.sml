(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* val all_except_option = fn : string * string list -> string list option *)
fun all_except_option (s:string, xs:string list) =
    case xs of
	[] => NONE
      | (x::xs') => if same_string(s, x)
		    then SOME xs'
		    else
			case all_except_option (s, xs') of
			    NONE => NONE
			  | SOME ys => SOME (x::ys)

(* val get_substitutions1 = fn : string list list * string -> string list *)
fun get_substitutions1 (xss:string list list, s:string) =
    case xss of
	[] => []
      | (xs::xss') => case all_except_option(s, xs) of
			  NONE => get_substitutions1 (xss', s)
			| SOME ys => ys @ get_substitutions1 (xss', s)
							     
	     
(* val get_substitutions2 = fn : string list list * string -> string list *)
fun get_substitutions2 (xss:string list list, s:string) =
    let
	fun helper (xss:string list list, zs:string list) =
	    case xss of
		[] => zs
	      | (xs::xss') => case all_except_option(s, xs) of
				  NONE => helper (xss', zs)
				| SOME ys => helper (xss', ys @ zs)
    in
	helper (xss, [])
    end
(* val similar_names = fn : string list list * {first:string, last:string, middle:string}
-> {first:string, last:string, middle:string} list *)
fun similar_names (xss:string list list, {first=first_name, last=last_name, middle=middle_name}) =
    let
	val xs = first_name::get_substitutions1 (xss, first_name);
	fun helper (xs:string list) =
	    case xs of
		[] => []
	      | (x::xs') => {first=x, last=last_name, middle=middle_name} :: helper (xs')
    in
	helper (xs)
    end
	

	
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* val card_color = fn : card -> color *)
fun card_color (c:card) =
    case c of
	(Spades, _ ) => Black
      | (Clubs, _ ) => Black
      | _ => Red

(* val card_value = fn : card -> int *)
fun card_value (c:card) =
    case c of
	( _ , Num n) => n
      | ( _ , Ace) => 11
      | ( _ , _ ) => 10
			 
(* val remove_card = fn : card list * card * exn -> card list *)
fun remove_card (cs:card list, c:card, exn) =
    case cs of
	[] => raise exn
      | (card::cs') => if card = c then cs'
		       else card::remove_card (cs', c, exn)

(* val all_same_color = fn : card list -> bool *)
fun all_same_color (cs:card list) =
    case cs of
	[] => true
      | [card1] => true
      | (card1::card2::cs') => if (card_color card1) = (card_color card2)
			       then all_same_color (card2::cs')
			       else false
					
(* val sum_cards = fn : card list -> int *)
fun sum_cards (cs:card list) =
    let
	fun helper (cs:card list, acc:int) =
	    case cs of
		[] => acc
	      | (c::cs') => helper (cs', acc+(card_value c))
    in
	helper (cs, 0)
    end
	
(* val score = fn : card list * int -> int *)
fun score (cs:card list, goal:int) =
    let
	val sum = sum_cards (cs);
	val is_same = all_same_color (cs);
	val preliminary = if sum > goal
			  then 3 * (sum - goal)
			  else (goal - sum)
    in
	if is_same
	then preliminary div 2
	else preliminary
    end

(* val officiate = fn : card list * move list * int -> int *)
fun officiate (cards:card list, moves:move list, goal:int) =
    let
	fun game (cards:card list, moves:move list, helds:card list) =
	    if sum_cards (helds) > goal
	    then score (helds, goal)
	    else
		case (cards, moves, helds) of
		    ( _ , [], helds)                    => score (helds, goal)
		  | ( [], Draw::moves', helds)          => score (helds, goal) 
		  | (card::cards', Draw::moves', helds) => game (cards', moves', card::helds)
		  | (cards, (Discard c)::moves', helds) => game (cards, moves', remove_card (helds, c, IllegalMove))								
    in
	game (cards, moves, [])
    end
	
    
	
