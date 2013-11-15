Control.Print.printDepth := 20;

datatype  ShishKebab = 
  Skewer 
                    | Onion of ShishKebab
                    | Lamb of ShishKebab
                    | Tomato of ShishKebab;

(*
 * I like how we can use types to build up a tree of sorts:
 * Tomato(Tomato(Lamb(Onion(Skewer)))). I don't see how it is
 * useful, though. Where are we going?
 *)

fun onlyOnions(Skewer)
    = true
  | onlyOnions(Onion(x))
    = onlyOnions(x)
  | onlyOnions(Lamb(x))
    = false
  | onlyOnions(Tomato(x))
    = false;

(onlyOnions : ShishKebab -> bool);

fun isVeg(Skewer)
    = true
  | isVeg(Onion(x))
    = isVeg(x)
  | isVeg(Lamb(x))
    = false
  | isVeg(Tomato(x))
    = isVeg(x);

(isVeg : ShishKebab -> bool);

datatype 'a Shish = 
    Bottom of 'a
  | Onion of 'a Shish
  | Lamb of 'a Shish
  | Tomato of 'a Shish;

fun isVeggie(Bottom(x))
    = true
  | isVeggie(Onion(x))
    = isVeggie(x)
  | isVeggie(Lamb(x))
    = false
  | isVeggie(Tomato(x))
    = isVeggie(x);

(isVeggie : 'a Shish -> bool);


fun what_bottom(Bottom(x))
    = x
  | what_bottom(Onion(x))
    = what_bottom(x)
  | what_bottom(Lamb(x))
    = what_bottom(x)
  | what_bottom(Tomato(x))
    = what_bottom(x);

(what_bottom : 'a Shish -> 'a);
