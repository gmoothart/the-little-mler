Control.Print.printDepth := 20;

datatype num = 
    Zero
  | One_more_than of num;

exception Too_small;

(*
fun is_zero(n)
  = n = 0;

fun succ(n)
  = n + 1;

fun pred(n)
  = if is_zero(n)
    then raise Too_small
    else n - 1;
*)

fun is_zero(Zero)
    = true
  | is_zero(n)
    = false;

fun succ(n)
  = One_more_than(n);

fun pred(Zero)
    = raise Too_small
  | pred(One_more_than(n))
    = n;
(pred : num -> num);

fun plus(n, m)
  = if is_zero(n)
    then m
    else succ(plus(pred(n), m));

