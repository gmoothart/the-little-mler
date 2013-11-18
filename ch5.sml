
datatype Fish = 
    Anchovy
  | Lox
  | Tuna;


datatype 'a pizza = 
    Bottom
  | Topping of ('a * ('a pizza));



fun rem_anchovy(Bottom):Fish pizza
     = Bottom
   | rem_anchovy(Topping(Anchovy, x)):Fish pizza
     = rem_anchovy(x)
   | rem_anchovy(Topping(x,y):Fish pizza):Fish pizza
     = Topping(x, rem_anchovy(y));


fun rem_tuna(Bottom):Fish pizza
     = Bottom
   | rem_tuna(Topping(Tuna, x)):Fish pizza
     = rem_tuna(x)
   | rem_tuna(Topping(x,y):Fish pizza):Fish pizza
     = Topping(x, rem_tuna(y));

(* ungramatical, `t` can't appear twice in the pattern
fun rem_topping((t, Bottom): Fish * Fish pizza):Fish pizza
    = Bottom
  | rem_topping((t, Topping(t, x)): Fish * Fish pizza):Fish pizza
    = rem_topping(t, x)
  | rem_topping((t, Topping(z,x)): Fish * Fish pizza):Fish pizza
    = Topping(z, rem_topping(t, x));
*)

fun eq_fish(Anchovy, Anchovy): bool
    = true
  | eq_fish(Lox, Lox): bool
    = true
  | eq_fish(Tuna, Tuna): bool
    = true
  | eq_fish(a:Fish, b:Fish): bool
    = false;
    
fun rem_topping((t, Bottom): Fish * Fish pizza):Fish pizza
    = Bottom
  | rem_topping((x, Topping(t, p)): Fish * Fish pizza):Fish pizza
    = if eq_fish(x,t)
      then rem_topping(x, p)
      else Topping(t, rem_topping(x, p));

fun eq_int(n:int, m:int):bool
  = (n=m);

fun rem_int(x, Bottom): int pizza
    = Bottom
  | rem_int(x, Topping(y, p))
    = if eq_int(x,y)
      then rem_int(x, p)
      else Topping(y, rem_int(x, p));


fun subst_fish(oldf, newf, Bottom)
    = Bottom
  | subst_fish(oldf, newf, Topping(f, p))
    = if eq_fish(oldf, f)
      then Topping(newf, subst_fish(oldf, newf, p))
      else Topping(f, subst_fish(oldf, newf, p));

fun eq_num(Zero, Zero):bool
    = true
  | eq_num(One_more_than(x), One_more_than(y))
    = eq_num(x, y)
  | eq_num(x, y)
    = false;
