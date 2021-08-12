(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2provided.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []
val test11 = all_except_option ("string", ["notstring"]) = NONE
val test111 = all_except_option ("string", []) = NONE

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test22 = 
get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Fredrick","Freddie","F"]
val test222 = 
get_substitutions1 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Jeffrey","Geoff","Jeffrey"]
val test2222 = get_substitutions1 ([], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test33 = 
get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") = ["Freddie","F","Fredrick"]
val test333 = 
get_substitutions2 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]], "Jeff") = ["Geoff","Jeffrey","Jeffrey"]
val test3333 = get_substitutions2 ([], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]


val test5 = card_color (Clubs, Num 2) = Black
val test55 = card_color (Spades, Num 2) = Black
val test555 = card_color (Hearts, Num 2) = Red
val test5555 = card_color (Diamonds, Num 2) = Red


val test6 = card_value (Clubs, Num 2) = 2
val test61 = card_value (Clubs, Num 3) = 3
val test62 = card_value (Clubs, Num 4) = 4
val test63 = card_value (Clubs, Num 5) = 5
val test64 = card_value (Clubs, Num 6) = 6
val test65 = card_value (Clubs, Num 7) = 7
val test66 = card_value (Clubs, Num 8) = 8
val test67 = card_value (Clubs, Num 9) = 9
val test68 = card_value (Clubs, Num 10) = 10
val test69 = card_value (Clubs, Ace) = 11
val test661 = card_value (Clubs, King) = 10
val test662 = card_value (Clubs, Queen) = 10
val test663 = card_value (Clubs, Jack) = 10


val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test77 = remove_card ([(Hearts, Ace), (Hearts, King)], (Hearts, Ace), IllegalMove) = [(Hearts, King)]
val test777 = (remove_card ([(Hearts, Ace)], (Spades, Ace), IllegalMove) handle IllegalMove => []) = []
val test7777 = (remove_card ([], (Hearts, Ace), IllegalMove) handle IllegalMove => []) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test88 = all_same_color [(Hearts, Ace), (Clubs, Ace)] = false
val test888 = all_same_color [(Hearts, Ace)] = true
val test8888 = all_same_color [] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test99 = sum_cards [] = 0

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test1199 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
             
