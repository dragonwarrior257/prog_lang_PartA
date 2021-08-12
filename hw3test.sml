(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3provided.sml";


val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test191 = only_capitals ["a","b","c"] = []
val test192 = only_capitals ["a","b","C"] = ["C"]


val test2 = longest_string1 ["A","bc","C"] = "bc"
val test22 = longest_string1 [] = ""
val test222 = longest_string1 ["A","C"] = "A"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test33 = longest_string2 [] = ""
val test333 = longest_string2 ["A","C"] = "C"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4aa = longest_string3 [] = ""
val test4aaa = longest_string3 ["A","C"] = "A"

val test4b = longest_string4 ["A","B","C"] = "C"
val test4bb = longest_string4 [] = ""
val test4bbb = longest_string4 ["A","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test55 = longest_capitalized [] = ""
val test555 = longest_capitalized ["Ab","bc","C"] = "Ab"


val test6 = rev_string "abc" = "cba"
val test66 = rev_string "" = ""

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test77 = (first_answer (fn x => if x > 5 then SOME x else NONE) [1,2,3,4,5] handle NoAnswer => 10) = 10

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test88 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME []
val test888 = all_answers (fn x => if x > 2 then SOME [x] else NONE) [3,4] = SOME [4,3]


val test9a = count_wildcards Wildcard = 1
val test9a1 = count_wildcards (Variable "a") = 0
val test9a2 = count_wildcards UnitP = 0
val test9a3 = count_wildcards (ConstP 22) = 0
val test9a4 = count_wildcards (TupleP [Wildcard, Wildcard, Wildcard]) = 3
val test9a5 = count_wildcards (ConstructorP ("a", Wildcard)) = 1
val test9a6 = count_wildcards (ConstructorP ("a", (TupleP [Wildcard, Wildcard, Wildcard]))) = 3

val test9b1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b2 = count_wild_and_variable_lengths Wildcard = 1
val test9b3 = count_wild_and_variable_lengths UnitP = 0
val test9b4 = count_wild_and_variable_lengths (ConstP 22) = 0
val test9b5 = count_wild_and_variable_lengths (TupleP [Wildcard, Wildcard, Wildcard]) = 3
val test9b6 = count_wild_and_variable_lengths (TupleP [Variable("a"), Variable("b"), Variable("c")]) = 3
val test9b7 = count_wild_and_variable_lengths (TupleP [Wildcard, Variable("b"), Variable("c"), UnitP]) = 3
val test9b8 = count_wild_and_variable_lengths (ConstructorP ("a", Wildcard)) = 1
val test9b9 = count_wild_and_variable_lengths (ConstructorP ("a", (TupleP [Variable("aa"), Wildcard, Wildcard]))) = 4

val test9c1 = count_some_var ("a", Variable("a")) = 1
val test9c2 = count_some_var ("a", Wildcard) = 0
val test9c3 = count_some_var ("a", UnitP) = 0
val test9c4 = count_some_var ("a", (ConstP 22)) = 0
val test9c5 = count_some_var ("a", (TupleP [Wildcard, Wildcard, Wildcard])) = 0
val test9c6 = count_some_var ("a", (TupleP [Variable("a"), Variable("a"), Variable("c")])) = 2
val test9c7 = count_some_var ("a", (TupleP [Wildcard, Variable("b"), Variable("c"), UnitP])) = 0
val test9c8 = count_some_var ("a", (ConstructorP ("a", Wildcard))) = 0
val test9c9 = count_some_var ("aa", (ConstructorP ("a", (TupleP [Variable("aa"), Wildcard, Wildcard])))) = 1
val test9c10 = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true
val test101 = check_pat (UnitP) = false
val test102 = check_pat (TupleP [Wildcard, Wildcard, Wildcard]) = false
val test103 = check_pat (TupleP [Wildcard, Variable ("b"), Wildcard]) = true
val test104 = check_pat (TupleP [Wildcard, Variable ("b"), Variable ("c")]) = true
val test105 = check_pat (TupleP [Variable("b"), Variable ("b"), Wildcard]) = false
val test106 = check_pat (ConstructorP ("a", Wildcard)) = false
val test107 = check_pat (ConstructorP ("a", (TupleP [Variable("aa"), Wildcard, Wildcard]))) = true
val test108 = check_pat (ConstructorP ("egg",ConstructorP ("egg",ConstP 4))) = true


val test11 = match (Const(1), UnitP) = NONE
val test111 = match (Unit, UnitP) = SOME []
val test112 = match (Const 7, ConstP 7) = SOME []
val test113 = match (Const 7, ConstP 8) = NONE
val test114 = match (Const 7, Wildcard) = SOME []
val test115 = match (Unit, Wildcard) = SOME []
val test116 = match (Tuple [Unit], Wildcard) = SOME []
val test117 = match (Constructor ("a", Unit), Wildcard) = SOME []
val test118 = match (Unit, Variable "a") = SOME [("a", Unit)]
val test119 = match (Const 7, Variable "a") = SOME [("a", Const 7)]
val test1110 = match (Tuple [Unit], Variable "a") = SOME [("a", Tuple [Unit])]
val test1111 = match (Constructor ("a", Unit), Variable "a") = SOME [("a", Constructor ("a", Unit))]
val test1112 = match (Tuple [Unit, Unit], TupleP [UnitP, Wildcard]) = SOME []
val test1113 = match (Tuple [Unit, Const 7], TupleP [UnitP, UnitP]) = NONE
val test1114 = match (Constructor ("a", Const 7), ConstructorP ("a", UnitP)) = NONE
val test1115 = match (Constructor ("b", Const 7), ConstructorP ("a", UnitP)) = NONE
val test1116 = match (Constructor ("a", Unit), ConstructorP ("a", UnitP)) = SOME []

val test12 = (first_match Unit [UnitP]) = SOME [] 
val test121 = (first_match Unit [Variable "s"]) = SOME [("s", Unit)] 
val test122 = (first_match (Const 7) [UnitP])= NONE 
val test123 = first_match (Const 16) [UnitP, Wildcard, Variable "my_var"] = SOME []
val test124 = first_match (Const 16) [UnitP, Variable "my_var", Wildcard] = SOME [("my_var", Const 16)]
val test125 = first_match (Const 16) [UnitP, ConstP 32, ConstructorP ("my_constructor", Variable "my_var")] = NONE