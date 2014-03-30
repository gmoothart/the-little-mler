Control.Print.printDepth := 20;

datatype oraple = 
    Orange
  | Apple;

fun eq_oraple(Orange, Orange)
    = true
  | eq_oraple(Apple, Apple)
    = true
  | eq_oraple(x,y)
    = false;

(* the interpreter dosesn't like Cons, is it built-in? *)
fun subst_oraple(n, a, Empty)
    = Empty
  | subst_oraple(n, a, Cons(e,t))
   = if eq_oraple(a, e)
     then Cons(n, subst_oraple(n, a, t))
     else Cons(e, subst_oraple(n, a, t));

fun subst(rel, n, a, Empty)
    = Empty
  | subst(n, a, Cons(e,t))
   = if rel(a, e)
     then Cons(n, subst_oraple(n, a, t))
     else Cons(e, subst_oraple(n, a, t));
