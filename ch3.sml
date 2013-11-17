
datatype  Pizza = 
    Crust
  | Cheese of Pizza
  | Onion of Pizza
  | Anchovy of Pizza
  | Sausage of Pizza;

fun removeAnchovy(Crust)
    = Crust
  | removeAnchovy(Cheese(x))
    = Cheese(removeAnchovy(x))
  | removeAnchovy(Onion(x))
    = Onion(removeAnchovy(x))
  | removeAnchovy(Anchovy(x))
    = removeAnchovy(x)
  | removeAnchovy(Sausage(x))
    = Sausage(removeAnchovy(x));

(removeAnchovy : Pizza -> Pizza);


fun topAnchovyWithCheese(Crust)
    = Crust
  | topAnchovyWithCheese(Cheese(x))
    = Cheese(topAnchovyWithCheese(x))
  | topAnchovyWithCheese(Onion(x))
    = Onion(topAnchovyWithCheese(x))
  | topAnchovyWithCheese(Anchovy(x))
    = Cheese(Anchovy(topAnchovyWithCheese(x)))
  | topAnchovyWithCheese(Sausage(x))
    = Sausage(topAnchovyWithCheese(x));

(topAnchovyWithCheese : Pizza -> Pizza);

fun substAnchovyByCheese(x)
  = removeAnchovy(topAnchovyWithCheese(x));
