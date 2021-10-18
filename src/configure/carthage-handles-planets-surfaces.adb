with WL.Random;

with Carthage.Climate;
with Carthage.Handles.Terrain;
with Carthage.Handles.Tiles.Configure;

package body Carthage.Handles.Planets.Surfaces is

   use Carthage.Climate;

   Highest : constant := 100;
   subtype Height_Range is Integer range 1 .. Highest;

   type Temperature_Array is array (Tile_X, Tile_Y) of Temperature_Range;

   type Humidity_Array is array (Tile_X, Tile_Y) of Humidity_Range;

   type Height_Array is array (Tile_X, Tile_Y) of Height_Range;

   procedure Create_Climate
     (Category : Carthage.Worlds.Any_Reference;
      Surface  : Height_Array;
      Land_Level  : Integer;
      Temperature : out Temperature_Array;
      Humidity    : out Humidity_Array);

   --------------------
   -- Create_Climate --
   --------------------

   procedure Create_Climate
     (Category    : Carthage.Worlds.Any_Reference;
      Surface     : Height_Array;
      Land_Level  : Integer;
      Temperature : out Temperature_Array;
      Humidity    : out Humidity_Array)
   is
      Lat_Temperature : array (Tile_Y) of Temperature_Range;
   begin
      for Lat in Lat_Temperature'Range loop
         Lat_Temperature (Lat) :=
           Category.Average_Temperature
             + 10.0 - 2.0 * Temperature_Range
           (abs (Planet_Height / 2 - Lat + 1));
      end loop;

      for Y in Tile_Y loop
         declare
            Reversed : constant Boolean :=
                         Y in Planet_Height / 4 .. 3 * Planet_Height / 4;
            Start     : Tile_X'Base :=
                          (if Reversed then Tile_X'Last else Tile_X'First);
         begin

            for X in Tile_X loop
               if Surface (X, Y) < Land_Level then
                  Temperature (X, Y) := Lat_Temperature (Y);
               else
                  Temperature (X, Y) :=
                    Lat_Temperature (Y)
                    - Temperature_Range (Float (Surface (X, Y)) / 10.0);
               end if;
            end loop;

            while Start in Tile_X
              and then Surface (Start, Y) >= Land_Level
            loop
               Humidity (Start, Y) := 0.0;
               if Reversed then
                  Start := Start - 1;
               else
                  Start := Start + 1;
               end if;
            end loop;

            if Start in Tile_X then
               declare
                  H : Humidity_Range := 1.0;
                  X : Tile_X := Start;
               begin
                  loop
                     if Surface (X, Y) < Land_Level then
                        H := 1.0;
                     else
                        H := 0.8 * H + 0.2 * H * Humidity_Range
                          (Float (Surface (X, Y) - Land_Level)
                           / Float (Height_Range'Last - Land_Level));
                     end if;
                     Humidity (X, Y) := H;
                     if X = Tile_X'First and then Reversed then
                        X := Tile_X'Last;
                     elsif X = Tile_X'Last and then not Reversed then
                        X := Tile_X'First;
                     elsif Reversed then
                        X := X - 1;
                     else
                        X := X + 1;
                     end if;
                     exit when X = Start;
                  end loop;
               end;
            end if;
         end;
      end loop;
   end Create_Climate;

   --------------------
   -- Create_Surface --
   --------------------

   procedure Create_Surface
     (Category : Carthage.Worlds.Any_Reference;
      Tiles    : out Tile_Array)
   is
      Heights     : Height_Array;
      Temperature : Temperature_Array;
      Humidity    : Humidity_Array;

      procedure Smooth (Heights : in out Height_Array);

      ------------
      -- Smooth --
      ------------

      procedure Smooth (Heights : in out Height_Array) is
         Source : constant Height_Array := Heights;
      begin
         for Y in Source'Range (2) loop
            for X in Source'Range (1) loop
               declare
                  Neighbour_Count : constant Natural :=
                                      Natural
                                        (Surface_Graph.Edge_Count
                                           (Index_Of ((X, Y))));
                  Total           : Natural := Heights (X, Y);

                  procedure Add_To_Total
                    (Item : Tile_Position;
                     Cost : Float);

                  ------------------
                  -- Add_To_Total --
                  ------------------

                  procedure Add_To_Total
                    (Item : Tile_Position;
                     Cost : Float)
                  is
                     pragma Unreferenced (Cost);
                  begin
                     Total := Total + Source (Item.X, Item.Y);
                  end Add_To_Total;

               begin
                  Surface_Graph.Iterate_Edges
                    ((X, Y), Add_To_Total'Access);
                  Heights (X, Y) := Total / (Neighbour_Count + 1);
               end;
            end loop;
         end loop;
      end Smooth;

   begin
      for Y in Heights'Range (2) loop
         for X in Heights'Range (1) loop
            Heights (X, Y) :=
              WL.Random.Random_Number (Height_Range'First, Height_Range'Last);
         end loop;
      end loop;

      for I in 1 .. Category.Smoothness loop
         Smooth (Heights);
      end loop;

      declare
         Freq_Table    : array (Height_Range) of Natural := (others => 0);
         Terrain_Table : array (Height_Range) of Carthage.Handles.Terrain.Any_Reference;

         Start      : Integer := Height_Range'First;
         Land_Start : Height_Range;
         Have_Land  : Boolean := False;

         procedure Set_Tiles
           (Terrain   : Carthage.Handles.Terrain.Any_Reference;
            Frequency : Carthage.Worlds.Frequency_Range);

         ---------------
         -- Set_Tiles --
         ---------------

         procedure Set_Tiles
           (Terrain   : Carthage.Handles.Terrain.Any_Reference;
            Frequency : Carthage.Worlds.Frequency_Range)
         is
            use Carthage.Worlds;
            Count : constant Natural :=
                      Natural (Frequency
                               * Float (Planet_Width)
                               * Float (Planet_Height));
            Acc   : Natural := 0;
         begin
            while Start <= Highest
              and then Acc <= Count
            loop
               Terrain_Table (Start) := Terrain;
               Acc := Acc + Freq_Table (Start);
               Start := Start + 1;
            end loop;
            if not Have_Land then
               Land_Start := Start;
               Have_Land := True;
            end if;
         end Set_Tiles;

      begin
         for Y in Heights'Range (2) loop
            for X in Heights'Range (1) loop
               declare
                  Height : constant Height_Range := Heights (X, Y);
                  Freq   : Natural renames Freq_Table (Height);
               begin
                  Freq := Freq + 1;
               end;
            end loop;
         end loop;

         Category.Scan_Terrain_Frequency
           (Set_Tiles'Access);

         for I in Start .. Terrain_Table'Last loop
            Terrain_Table (I) := Terrain_Table (Start - 1);
         end loop;

         Create_Climate
           (Category, Heights, Land_Start, Temperature, Humidity);

         for Y in Tiles'Range (2) loop
            for X in Tiles'Range (1) loop
               declare
                  use Carthage.Handles.Terrain;
                  Feature : constant Any_Reference :=
                              Terrain_Table (Heights (X, Y));
                  Base    : constant Any_Reference :=
                              (if Feature.Water
                               then Feature
                               else Category.Base_Land_Terrain
                                 (Temperature (X, Y),
                                  Humidity (X, Y)));
               begin
                  Category.Log
                    (X'Image & Y'Image & "h" & Natural'Image
                       (Natural (Float (Humidity (X, Y)) * 100.0))
                     & " t " & Integer'Image (Integer (Temperature (X, Y)))
                     & " -> "
                     & (if Base = null then "[none]"
                       else Base.Identifier));
                  Tiles (X, Y) :=
                    Carthage.Handles.Tiles.Configure.Create_Tile
                      (Index_Of ((X, Y)), (X, Y), Heights (X, Y) - Land_Start,
                       Base_Terrain => Base,
                       Feature_Terrain => Feature);
               end;
            end loop;
         end loop;

      end;

   end Create_Surface;

end Carthage.Handles.Planets.Surfaces;
