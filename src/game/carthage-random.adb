with Ada.Numerics.Float_Random;
with Carthage.Elementary_Functions;

package body Carthage.Random is

   Gen : Ada.Numerics.Float_Random.Generator;

   -----------
   -- About --
   -----------

   function About
     (Value : Real;
      Variation : Real)
      return Real
   is
   begin
      return Value - Variation + Unit_Random * Variation * 2.0;
   end About;

   -------------------
   -- Normal_Random --
   -------------------

   function Normal_Random
     (Standard_Deviation : Non_Negative_Real := 1.0)
      return Real
   is
      use Carthage.Elementary_Functions;
      Std_Normal : Real;
   begin
      loop
         declare
            U : constant Signed_Unit_Real :=
                  2.0 * Unit_Random - 1.0;
            V : constant Signed_Unit_Real :=
                  2.0 * Unit_Random - 1.0;
            S : constant Non_Negative_Real := U ** 2 + V ** 2;
         begin
            if S < 1.0 then
               Std_Normal := U * Sqrt (-2.0 * Log (S) / S);
               exit;
            end if;
         end;
      end loop;
      return Std_Normal * Standard_Deviation;
   end Normal_Random;

   -----------
   -- Reset --
   -----------

   procedure Reset is
   begin
      Ada.Numerics.Float_Random.Reset (Gen);
   end Reset;

   -----------
   -- Reset --
   -----------

   procedure Reset (Initiator : Integer) is
   begin
      Ada.Numerics.Float_Random.Reset (Gen, Initiator);
   end Reset;

   -----------------
   -- Unit_Random --
   -----------------

   function Unit_Random return Unit_Real is
   begin
      return Unit_Real (Ada.Numerics.Float_Random.Random (Gen));
   end Unit_Random;

end Carthage.Random;
