(* val is_older = fn : (int * int * int) * (int * int * int) -> bool *)
fun is_older (date1:int * int * int, date2:int * int * int) =
    let
	val yy1 = (#1 date1);
	val mm1 = (#2 date1);
	val dd1 = (#3 date1);
	
	val yy2 = (#1 date2);
	val mm2 = (#2 date2);
	val dd2 = (#3 date2);
    in
	if yy1 < yy2 then true
	else
	    if yy1 > yy2 then false
	    else
		if mm1 < mm2 then true
		else
		    if mm1 > mm2 then false
		    else
			if dd1 < dd2 then true
			else false
    end

	
(* val number_in_month = fn : (int * int * int) list * int -> int *)
fun number_in_month (xs:(int * int * int) list, month: int) =
    if null xs then 0
    else
	if month = (#2 (hd xs))
	then 1 + number_in_month ((tl xs), month)
	else number_in_month ((tl xs), month)

(* val number_in_months = fn : (int * int * int) list * int list -> int *)
fun number_in_months (dates:(int * int * int) list, months: int list) =
    if null months then 0
    else number_in_month (dates, hd months) +
	 number_in_months (dates, tl months)

(* val dates_in_month = fn : (int * int * int) list * int -> (int * int * int) list *)
fun dates_in_month (dates:(int * int * int) list, month:int) =
    if null dates then []
    else
	let
	    val mm = (#2 (hd dates))
	in
	    if month = mm
	    then (hd dates) :: dates_in_month (tl dates, month)
	    else dates_in_month (tl dates, month)
	end

(* val dates_in_months = fn : (int * int * int) list * int list -> (int * int * int) list *)
fun dates_in_months (dates:(int * int * int) list, months:int list) =
    if null months then []
    else
	let
	    val sublist = dates_in_month (dates, hd months)
	in
	    sublist @ dates_in_months(dates, tl months)
	end

(* val get_nth = fn : string list * int -> string *)
fun get_nth (strings:string list, index:int) =
    if index = 1
    then hd strings
    else get_nth (tl strings, index-1)

(* val date_to_string = fn : int * int * int -> string *)
fun date_to_string (date:int * int * int) =
    let
	val month_names = ["January", "February", "March", "April",
                       "May", "June", "July", "August",
		       "September", "October", "November", "December"];
	val yy = Int.toString (#1 date);
	val mm = get_nth (month_names, (#2 date));
	val dd = Int.toString (#3 date);
    in
	mm ^ " " ^ dd ^ ", " ^ yy
    end

(* val number_before_reaching_sum = fn : int * int list -> int *)
fun number_before_reaching_sum (sum:int, xs:int list) =
    let
	val first = hd xs;
    in
	if first < sum
	then 1 + number_before_reaching_sum (sum - first, tl xs)
	else 0
    end

(* val what_month = fn : int -> int *)
fun what_month num:int =
    let
	val days_in_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    in
	1 + number_before_reaching_sum (num, days_in_months)
    end

(* val month_range = fn : int * int -> int list *)
fun month_range (num1:int, num2:int) =
    if num1 > num2 then []
    else
	(what_month num1) :: month_range (num1+1, num2)
					 
(* val oldest = fn : (int * int * int) list -> (int * int * int) option *)	
fun oldest (dates:(int * int * int)list) =
    if null dates then NONE
    else
	let
	    fun oldest (dates:(int * int * int) list) =
		if null (tl dates)
		then (hd dates)
		else
		    let
			val oldest_date = oldest (tl dates);
			val current_date = (hd dates);
		    in
			if is_older (current_date, oldest_date)
			then current_date
			else oldest_date
		    end
	in
	    SOME (oldest dates)
	end
	    
fun remove_dup (xs:int list) =
    let
	fun remove(xs:int list, x:int) =
	    if null xs then []
	    else
		if x = (hd xs) then remove (tl xs, x)
		else (hd xs) :: remove (tl xs, x)
    in
	if null xs then []
	else
	    let
		val new_list = remove(tl xs, hd xs);
	    in
		(hd xs) :: remove_dup new_list
	    end
    end

fun number_in_months_challenge (dates:(int * int * int) list, months: int list) =
    let
	val mms = remove_dup months;
    in
	number_in_months (dates, mms)
    end
	
fun dates_in_months_challenge (dates:(int * int * int) list, months:int list) =
    let
	val mms = remove_dup months;
    in
	dates_in_months (dates, mms)
    end
	
fun duplicate_remover (to_check : int , month : int list) =
    if null month
    then
	[]
    else if hd month = to_check
    then
	duplicate_remover(to_check, tl month)
    else
	hd month :: duplicate_remover(to_check, tl month)        
