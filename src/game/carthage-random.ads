package Carthage.Random is

   procedure Reset;
   procedure Reset (Initiator : Integer);

   function Unit_Random return Unit_Real;

   function Normal_Random
     (Standard_Deviation : Non_Negative_Real := 1.0)
      return Real;

   function About
     (Value     : Real;
      Variation : Real)
      return Real;

end Carthage.Random;
