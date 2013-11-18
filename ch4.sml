
datatype Meza = 
    Shrimp
  | Calamari
  | Escargots
  | Hummus;

datatype Main = 
    Steak
  | Ravioli
  | Chicken
  | Eggplant;

datatype Salad = 
    Green
  | Cucumber
  | Greek;

datatype Dessert = 
    Sundae
  | Mousse
  | Torte;


fun add_a_steak(x:Meza): Meza * Main
    = (x, Steak);


fun eq_main(Steak, Steak)
    = true
  | eq_main(Ravioli, Ravioli)
    = true
  | eq_main(Chicken, Chicken)
    = true
  | eq_main(Eggplant, Eggplant)
    = true
  | eq_main(x, y)
    = false;

(eq_main : Main * Main -> bool);


fun has_steak(a:Meza, Steak, b:Dessert):bool
     = true
   | has_steak(a:Meza, x, b:Dessert):bool
     = false;

