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

signature N =
  sig
    type number
    exception Too_small
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
  end;
*)

signature N_C_R =
  sig
    type number
    exception Too_small
    val conceal : int -> number
    val succ : number -> number
    val pred : number -> number
    val is_zero : number -> bool
    val reveal : number -> int
  end;

functor NumberAsNum()
  :>
  N_C_R
  =
  struct
    datatype num =
        Zero
      | One_more_than of num;
    type number = num
    exception Too_small
    fun conceal(0)
        = Zero
      | conceal(i)
        = One_more_than(conceal(i-1))
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
    fun reveal(Zero)
        = 0
      | reveal(One_more_than(rest))
        = 1 + reveal(rest)
  end

functor NumberAsInt()
  :>
  N_C_R
  =
  struct
    type number = int
    exception Too_small
    fun conceal(n)
      = n
    fun succ(n)
      = n + 1;
    fun pred(n)
      = if n = 0
        then raise Too_small
        else n - 1;
    fun is_zero(n)
      = n = 0;
    fun reveal(n)
      = n
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
  P where type number = a_N.number
  =
  struct
    type number = a_N.number
    fun plus(n, m)
      = if a_N.is_zero(n)
        then m
        else a_N.succ(plus(a_N.pred(n), m));

  end

structure IntArith = PON(IntStruct);
structure NumArith = PON(NumStruct);

signature S =
  sig
    type number1
    type number2
    val similar : (number1 * number2) -> bool
  end;

functor Same(structure a_N : N
             structure b_N : N)
  :>
  S where type number1 = a_N.number
    where type number2 = b_N.number
  =
  struct
    type number1 = a_N.number
    type number2 = b_N.number
    fun similar(a, b)
      = if a_N.is_zero(a) andalso b_N.is_zero(b)
        then true
        else if a_N.is_zero(a) orelse b_N.is_zero(b)
        then false
        else similar(a_N.pred(a), b_N.pred(b))
  end

structure SimIntNum = Same(structure a_N = IntStruct
                           structure b_N = NumStruct);

signature J = 
  sig
    val new_plus : (int * int) -> int
  end

functor NP(structure a_N : N_C_R
           structure a_P : P
           sharing type
           a_N.number
           =
           a_P.number)
  :>
  J
  =
  struct
    fun new_plus(x,y)
      = a_N.reveal(
          a_P.plus(
            a_N.conceal(x), a_N.conceal(y)))
  end

structure NPStruct = NP(structure a_N = NumStruct
                        structure a_P = NumArith);

signature T =
  sig
    type number
    val times : (number * number) -> number
  end

functor TON(structure a_N : N_C_R
           structure a_P : P
           sharing type
           a_N.number
           =
           a_P.number)
  :>
  T where type number = a_N.number
  =
  struct
    type number = a_N.number
    fun times(x,y)
      = if a_N.is_zero(x) 
        then x
        else a_P.plus(x, times(x, a_N.pred(y)))
  end

