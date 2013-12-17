Control.Print.printDepth := 20;

fun true_maker(x) = 
  true;

datatype bool_or_int = 
    Hot of bool
  | Cold of int;


fun hot_maker(x) = 
    Hot;

datatype Chain = 
  Link of (int * (int -> Chain));

fun ints(n)
  = Link(n + 1, ints);


(*  int * chain -> int *)
fun chain_item(n, Link(i,f))
  = if (n = 1)
    then i
    else chain_item(n-1, f(i));
