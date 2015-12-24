Control.Print.printDepth := 20;

datatype oraple =
    Orange
  | Apple;

datatype 'a List =
    Empty
  | Cons of ('a * 'a List);


fun eq_oraple(Orange, Orange)
    = true
  | eq_oraple(Apple, Apple)
    = true
  | eq_oraple(x,y)
    = false;

fun subst_oraple(n, a, Empty)
    = Empty
  | subst_oraple(n, a, Cons(e,t))
   = if eq_oraple(a, e)
     then Cons(n, subst_oraple(n, a, t))
     else Cons(e, subst_oraple(n, a, t));

fun subst(rel, n, a, Empty)
    = Empty
  | subst(rel, n, a, Cons(e,t))
   = if rel(a, e)
     then Cons(n, subst_oraple(n, a, t))
     else Cons(e, subst_oraple(n, a, t));

fun in_range_c(small, large)(n)
  = (small < n) andalso (n < large);

fun subst_pred(pred, n, Empty)
    = Empty
  | subst_pred(pred, n, Cons(e, t))
    = if pred(e)
     then Cons(n, subst_pred(pred, n, t))
     else Cons(e, subst_pred(pred, n, t));

fun subst_c(pred)(n, Empty)
    = Empty
  | subst_c(pred)(n, Cons(e, t))
    = if pred(e)
     then Cons(n, subst_c(pred)(n, t))
     else Cons(e, subst_c(pred)(n, t));


(* function must take two lists and return one list of the elements combined *)
fun combine(Empty, l2)
    = l2
  | combine(Cons(a, l1), l2)
    = Cons(a, combine(l1,l2));

fun combine_c(Empty)(l2)
    = l2
  | combine_c(Cons(a, l1))(l2)
    = Cons(a, combine_c(l1)(l2));

fun base(l2)
    = l2;


fun combine_s(Empty)
    = base
  | combine_s(Cons(a, l1))
    = make_cons(a, combine_s(l1))
and
  make_cons(a, f)(l2)
    = Cons(a, f(l2));

