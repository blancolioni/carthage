with Ada.Streams;

with Carthage.Money;
with Carthage.Quantities;

package Carthage.Settings is

   function Daily_Production_Factor return Unit_Real;
   function Daily_Harvest_Factor    return Unit_Real;
   function Daily_Tax_Factor        return Unit_Real;

   function Daily_Laboratory_Cost   return Carthage.Money.Money_Type;
   function Daily_Research_Points   return Non_Negative_Real;

   function Daily_City_Food         return Carthage.Quantities.Quantity_Type;
   function Daily_Unit_Food_Factor  return Unit_Real;

   procedure Configure (Path : String);

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

end Carthage.Settings;
