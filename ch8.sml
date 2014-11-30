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
