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
*)


signature N =
  sig
    type number
    exception Too_small
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
  end;

functor NumberAsNum()
  :>
  N
  =
  struct
    datatype num =
        Zero
      | One_more_than of num;
    type number = num
    exception Too_small
    fun succ(n)
      = One_more_than(n)
    fun pred(Zero)
        = raise Too_small
      | pred(One_more_than(n))
        = n
    fun is_zero(Zero)
        = true
      | is_zero(n)
        = false
  end

functor NumberAsInt()
  :>
  N
  =
  struct
    type number = int
    exception Too_small
    fun succ(n)
      = n + 1;
    fun pred(n)
      = if n = 0
        then raise Too_small
        else n - 1;
    fun is_zero(n)
      = n = 0;
  end

structure IntStruct =
  NumberAsInt()

structure NumStruct =
  NumberAsNum()

signature P =
  sig
    type number
    val plus : (number * number) -> number
  end;

functor PON(a_N : N)
  :>
  P
  =
  struct
    type number = a_N.number
    fun plus(n, m)
      = if a_N.is_zero(n)
        then m
        else a_N.succ(plus(a_N.pred(n), m));

  end


