with Ada.Text_IO;

with Carthage.Handles.Houses;
with Carthage.Handles.Worlds;

with Carthage.Handles.Galaxy.Configure;

with Carthage.Handles.Stacks.Create;

with Carthage.Logging;

package body Carthage.Handles.Planets.Configure is

   function To_Coordinate (X : Natural) return Coordinate
   is (Coordinate (Float (X) / 50.0));

   ----------------------
   -- Import_Jump_Gate --
   ----------------------

   procedure Import_Jump_Gate
     (X1, Y1, X2, Y2 : Natural)
   is
      function Find_Nearest (X, Y : Natural) return Planet_Handle;

      ------------------
      -- Find_Nearest --
      ------------------

      function Find_Nearest (X, Y : Natural) return Planet_Handle is
         XX : constant Coordinate := To_Coordinate (X);
         YY : constant Coordinate := To_Coordinate (Y);
         Nearest_Planet : Planet_Handle;
         Shortest_Distance : Real := Real'Last;

         procedure Check (Planet : Planet_Handle);

         -----------
         -- Check --
         -----------

         procedure Check (Planet : Planet_Handle) is
            D : constant Real :=
                  (Planet.Galaxy_X - Real (XX)) ** 2
                  + (Planet.Galaxy_Y - Real (YY)) ** 2;
         begin
            if not Nearest_Planet.Has_Element
              or else D < Shortest_Distance
            then
               Nearest_Planet := Planet;
               Shortest_Distance := D;
            end if;
         end Check;

      begin
         For_All_Planets (Check'Access);
         return Nearest_Planet;
      end Find_Nearest;

      From : constant Planet_Handle := Find_Nearest (X1, Y1);
      To   : constant Planet_Handle := Find_Nearest (X2, Y2);
   begin
      Carthage.Handles.Galaxy.Configure.Import_Gate
        (From.Reference, To.Reference);
   end Import_Jump_Gate;

   -------------------
   -- Import_Planet --
   -------------------

   function Import_Planet
     (Id          : String;
      X, Y        : Natural;
      Tile_Set    : Natural;
      Create_Tile : not null access
        function (Planet : Planet_Reference;
                  X      : Tile_X;
                  Y      : Tile_Y)
      return Tile_Reference)
      return Planet_Handle
   is

      Category : constant Carthage.Handles.Worlds.World_Handle :=
                   Carthage.Handles.Worlds.Get
                     (case Tile_Set is
                         when 0      => "normal",
                         when 1      => "city",
                         when 2      => "ice",
                         when 3      => "jungle",
                         when 4      => "barren",
                         when others =>
                            raise Constraint_Error with
                      Id & ": bad tileset:" & Tile_Set'Image);

      First_Tile : Tile_Reference := Null_Tile_Reference;
      Last_Tile  : Tile_Reference := Null_Tile_Reference;

   begin

      declare
         Reference : constant Planet_Reference :=
                    Create_Planet
                      (Tag        => Id,
                       X          => To_Coordinate (X),
                       Y          => To_Coordinate (Y),
                       Category   => Category.Reference,
                       Tile_Set   => Tile_Set);

         Planet : constant Planet_Handle := Get (Reference);
      begin

         for Y in Tile_Y loop
            for X in Tile_X loop
               declare
                  Tile : constant Tile_Reference :=
                           Create_Tile (Reference, X, Y);
               begin
                  if First_Tile = Null_Tile_Reference then
                     First_Tile := Tile;
                  end if;
                  Last_Tile := Tile;
               end;
            end loop;
         end loop;

         Planet.Set_Tile_Range (First_Tile, Last_Tile);

         Carthage.Logging.Log ("created " & Planet.Tag);
         Ada.Text_IO.Put (" " & Planet.Local_Text);
         Ada.Text_IO.Flush;

         declare
            procedure Create_Orbital_Stack
              (House : Carthage.Handles.Houses.House_Handle);

            --------------------------
            -- Create_Orbital_Stack --
            --------------------------

            procedure Create_Orbital_Stack
              (House : Carthage.Handles.Houses.House_Handle)
            is
            begin
               Planet.Set_Orbital_Stack
                 (House => House.Reference,
                  Stack =>
                    Carthage.Handles.Stacks.Create.Create_Orbital_Stack
                      (Owner  => House,
                       Planet => Planet.Reference));
            end Create_Orbital_Stack;

         begin
            Carthage.Handles.Houses.For_All_Houses
              (Create_Orbital_Stack'Access);
         end;

         return Planet;
      end;
   end Import_Planet;

end Carthage.Handles.Planets.Configure;
