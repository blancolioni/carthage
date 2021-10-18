with Ada.Text_IO;

with WL.Heaps;
with WL.Random;

package body Carthage.Tests is

   procedure Run_Heap_Tests;

   --------------------
   -- Run_Heap_Tests --
   --------------------

   procedure Run_Heap_Tests is
      package Heaps is new WL.Heaps (Positive, Positive);
      H : Heaps.Heap;
      Xs : array (1 .. 10) of Positive;
   begin
      for X of Xs loop
         X := WL.Random.Random_Number (1, 100);
      end loop;
      for X of Xs loop
         H.Insert (X, X);
      end loop;

      for X of Xs loop
         declare
            Y : constant Positive := H.First_Element;
         begin
            H.Delete_First;
            Ada.Text_IO.Put (Y'Image);
         end;
      end loop;

      Ada.Text_IO.New_Line;

      for X of Xs loop
         H.Insert (X, X);
      end loop;

      if True then
         for X of Xs loop
            H.Replace (X + 100, X);
         end loop;
      end if;

      for X of Xs loop
         declare
            Y : constant Positive := H.First_Element;
         begin
            H.Delete_First;
            X := Y;
         end;
      end loop;

      for X of Xs loop
         Ada.Text_IO.Put (X'Image);
      end loop;
      Ada.Text_IO.New_Line;

      declare
         Last : Natural := Natural'Last;
      begin
         for X of Xs loop
            if X > Last then
               raise Constraint_Error with
                 "test failed: queue not in order";
            end if;
            Last := X;
         end loop;
      end;
   end Run_Heap_Tests;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Run_Heap_Tests;
   end Run_Tests;

end Carthage.Tests;
