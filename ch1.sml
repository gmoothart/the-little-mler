Control.Print.printDepth := 20;

datatype 'a OpenFacedSandwich = 
  Bread of 'a | 
  Slice of 'a OpenFacedSandwich;
