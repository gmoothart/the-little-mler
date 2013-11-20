Control.Print.printDepth := 20;

datatype Fruit = 
    Peach
  | Apple
  | Pear
  | Lemon
  | Fig;


fun eq_fruit(Peach, Peach)
    = true
  | eq_fruit(Apple, Apple)
    = true
  | eq_fruit(Pear, Pear)
    = true
  | eq_fruit(Lemon, Lemon)
    = true
  | eq_fruit(Fig, Fig)
    = true
  | eq_fruit(x,y)
    = false;


datatype 'a Tree = 
    Bud
  | Flat of 'a * 'a Tree
  | Split of 'a Tree * 'a Tree;

fun flat_only(Bud) 
    = true
  | flat_only(Flat(a, x)) 
    = flat_only(x)
  | flat_only(Split(x,y))
    = false;

fun split_only(Bud) 
    = true
  | split_only(Flat(a, x)) 
    = false
  | split_only(Split(x,y))
    = if split_only(x) 
      then split_only(y)
      else false;


fun contains_fruit(x: Fruit Tree): bool
    = if split_only(x)
      then false
      else true;

fun max(x:int, y:int):int
  = if x >= y
    then x
    else y;

fun height(Bud):int
    = 0
  | height(Flat(a, x))
    = 1 + height(x)
  | height(Split(x,y))
    = 1 + max(height(x), height(y));

fun subst_in_tree(newf:Fruit, oldf:Fruit, Bud: Fruit Tree):Fruit Tree
    = Bud
  | subst_in_tree(newf:Fruit, oldf:Fruit, Flat(f, x):Fruit Tree):Fruit Tree
    = if eq_fruit(f, oldf)
      then Flat(newf, subst_in_tree(newf, oldf, x))
      else Flat(f, subst_in_tree(newf, oldf, x))
  | subst_in_tree(newf:Fruit, oldf:Fruit, Split(x,y):Fruit Tree):Fruit Tree
    = Split(subst_in_tree(newf, oldf, x), subst_in_tree(newf, oldf, y));


fun occurs(f:Fruit, Bud:Fruit Tree): int
    = 0
  | occurs(f:Fruit, Flat(x, y)): int
    = if eq_fruit(f, x)
      then 1 + occurs(f, y)
      else occurs(f, y)
  | occurs(f:Fruit, Split(x,y):Fruit Tree): int
    = occurs(f, x) + occurs(f, y);


(* Now we are getting somewhere! *)
datatype 'a Slist = 
    Empty
  | Scons of (('a Slist) * 'a Slist)

and

  'a Sexp = 
    An_atom of 'a
  | A_slist of ('a Slist);








