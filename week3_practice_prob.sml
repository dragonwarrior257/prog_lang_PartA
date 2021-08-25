(* Problem 1 to 4 *)
type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

(* Problem 1:
Write a function pass_or_fail of type {grade : int option, id : 'a} -> pass_fail that takes a 
final_grade (or, as the type indicates, a more general type) and returns pass
 if the grade field contains SOME i;SOMEi for an i≥75 (else fail).*)
fun pass_or_fail {grade:int option, id:'a} :pass_fail =
    case grade of
	SOME i => if i >= 75 then pass else fail
      | _ => fail
		 
 (* Problem 2:
Using pass_or_fail\verb|pass_or_fail|pass_or_fail as a helper function, write a function has_passed\verb|has_passed|has_passed of type {grade : int option, id : 'a} -> bool\verb|{grade : int option, id : 'a} -> bool|{grade : int option, id : ’a} -> bool that returns true\verb|true|true if and only if the the grade field contains SOME  i\verb|SOME|\;iSOMEi for an i≥75i\geq 75i≥75.*)
fun has_passed (record as {grade:int option, id:'a}) :bool =
    case pass_or_fail record of
	pass => true
      | fail => false
		    
(* Problem 3:
Using has_passed\verb|has_passed|has_passed as a helper function, write a function number_passed\verb|number_passed|number_passed that takes a list of type final_grade\verb|final_grade|final_grade (or a more general type) and returns how many list elements have passing (again, ≥75\geq 75≥75) grades.*)
val number_passed = (List.length o List.filter has_passed)

(* Problem 4:
Write a function number_misgraded\verb|number_misgraded|number_misgraded of type (pass_fail * final_grade) list -> int\verb|(pass_fail * final_grade) list -> int|(pass_fail * final_grade) list -> int that indicates how many list elements are "mislabeled" where mislabeling means a pair (pass,x)\verb|(pass,x)|(pass,x) where has_passed x\verb|has_passed x|has_passed x is false\verb|false|false or (fail,x)\verb|(fail,x)|(fail,x) where has_passed x\verb|has_passed x|has_passed x is true\verb|true|true.*)
val number_misgraded = List.length o
		       List.filter (fn (pass, fail) => true| (fail, pass) => true | (_, _) => false) o
		       List.map (fn (x,y) => (x, pass_or_fail y))

(* Problem 5-7*)
datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

(*Problem 5:
Write a function tree_height\verb|tree_height|tree_height that accepts an 'a tree\verb|'a tree|’a tree and evaluates to a height of this tree. The height of a tree is the length of the longest path to a leaf. Thus the height of a leaf is 0\verb|0|0.*)
fun tree_height (t:'a tree) :int =
    case t of
	leaf => 0
      | node {value=v, left=ltree, right=rtree} => 1 + Int.max (tree_height ltree, tree_height rtree)
							   
(*Problem 6:
Write a function sum_tree\verb|sum_tree|sum_tree that takes an int tree\verb|int tree|int tree and evaluates to the sum of all values in the nodes.*)
fun sum_tree (t:int tree) :int =
    case t of
	leaf => 0
      | node {value=v, left=ltree, right=rtree} => v + sum_tree ltree + sum_tree rtree
										 
    
(*Problem 7: 
Write a function gardener\verb|gardener|gardener of type flag tree -> flag tree\verb|flag tree -> flag tree|flag tree -> flag tree such that its structure is identical to the original tree except all nodes of the input containing prune_me\verb|prune_me|prune_me are (along with all their descendants) replaced with a leaf.*)
fun gardener (t:flag tree) :flag tree =
    case t of
	leaf => leaf
      | node {value=leave_me_alone, left=ltree, right=rtree} => node {value=leave_me_alone, left=gardener ltree, right=gardener rtree}
      | node {value=prune_me, left=ltree, right=rtree} => leaf 

(*Problem 9-16
A "natural" number is either zero, or the "successor" of a another integer.    So for example the number 1 is just SUCC ZERO\verb|SUCC ZERO|SUCC ZERO, the number 2 is SUCC (SUCC ZERO)\verb|SUCC (SUCC ZERO)|SUCC (SUCC ZERO), and so on.*)
datatype nat = ZERO | SUCC of nat

(*Problem 9:
Write is_positive : nat -> bool\verb|is_positive : nat -> bool|is_positive : nat -> bool, which given a "natural number" returns whether that number is positive (i.e. not zero).*)
fun is_positive ZERO = false
  | is_positive (SUCC _) = true

(*Problem 10:
Write pred : nat -> nat\verb|pred : nat -> nat|pred : nat -> nat, which given a "natural number" returns its predecessor. Since 0 does not have a predecessor in the natural numbers, throw an exception Negative\verb|Negative|Negative (will need to define it first).*)
exception Negative
fun pred ZERO = raise Negative
  | pred (SUCC n) = n 

(* Problem 11:
Write nat_to_int : nat -> int\verb|nat_to_int : nat -> int|nat_to_int : nat -> int, which given a "natural number" returns the corresponding int\verb|int|int. For example, nat_to_int (SUCC (SUCC ZERO)) = 2\verb|nat_to_int (SUCC (SUCC ZERO)) = 2|nat_to_int (SUCC (SUCC ZERO)) = 2.  (Do not use this function for problems 13-16 -- it makes them too easy.)*)
fun nat_to_int (num:nat) :int =
    case num of
	ZERO => 0
      | (SUCC n) => 1 + nat_to_int n

(* Problem 12:
Write int_to_nat : int -> nat\verb|int_to_nat : int -> nat|int_to_nat : int -> nat which given an integer returns a "natural number" representation for it, or throws a Negative\verb|Negative|Negative exception if the integer was negative.  (Again, do not use this function in the next few problems.)*)
fun int_to_nat (num:int) :nat =
    if num < 0 then raise Negative
    else
	case num of
	    0 => ZERO
	  | num => SUCC (int_to_nat (num-1))
			
			
(* Problem 13:
Write add : nat * nat -> nat\verb|add : nat * nat -> nat|add : nat * nat -> nat to perform addition.*)
fun add (num1:nat, num2:nat) :nat =
    case (num1, num2) of
	( _ , ZERO) => num1
      | (ZERO, _ ) => num2
      | ((SUCC m), (SUCC n)) => SUCC (add (m, num2))

(* Problem 14:
Write sub : nat * nat -> nat\verb|sub : nat * nat -> nat|sub : nat * nat -> nat to perform subtraction.  (Hint: Use pred\verb|pred|pred.)*)
fun sub (num1:nat, num2:nat) :nat =
    case (num1, num2) of
        (ZERO, (SUCC n)) => raise Negative
      | (_ , ZERO) => num1
      | (_ , _) => sub (pred num1, pred num2)
			 
(* Problem 15:
Write mult : nat * nat -> nat\verb|mult : nat * nat -> nat|mult : nat * nat -> nat to perform multiplication. (Hint: Use add\verb|add|add.)*)
fun mult (num1:nat, num2:nat) :nat =
    case (num1, num2) of
        (ZERO, _ ) => ZERO
      | ((SUCC ZERO), _) => num2
      | ( _ , _ ) => add(num2, mult(pred num1, num2))  
					     
(* Problem 16:
Write less_than : nat * nat -> bool\verb|less_than : nat * nat -> bool|less_than : nat * nat -> bool to return true\verb|true|true when the first argument is less than the second.*)
fun less_than (num1:nat, num2:nat) :bool =
    let
	val s = sub(num1, num2)
    in
	false
    end
    handle Negative => true
			   
