Control.Print.printDepth := 20;

datatype 'a List =
    Empty
  | Cons of ('a * 'a List);

datatype Box =
    Bacon
  | Ix of int;

fun is_bacon(Bacon)
    = true
  | is_bacon(Ix(n))
    = false;

(* This was too clever, the book is going in a different direction *)
fun where_is(l)
    = where_is'(l, 0)
and where_is'(Empty, i)
    = 0
  | where_is'(Cons(a, l), i)
    = if is_bacon(a)
      then i + 1
      else where_is'(l, i+1);

exception No_bacon of int;

fun where_is2(Empty)
    = raise No_bacon(0)
  | where_is2(Cons(a, l))
    = if (is_bacon(a))
      then 1
      else 1 + where_is2(l);

exception Out_of_range;

fun list_item(n, Empty)
    = raise Out_of_range
  | list_item(n, Cons(a, l))
    = if (n = 1)
      then a
      else list_item(n-1, l);

(*
fun find(n, Empty)
    = raise No_bacon(0)
  | find(n, Cons(Bacon, l))
    = 1
  | find(n, Cons(Ix(i), l))
    = 1 + find(i, ___)
    *)


