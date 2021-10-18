with Ada.Containers.Doubly_Linked_Lists;

with WL.Heaps;

package body Hexes.Grids is

   ------------------------
   -- Coordinates_Within --
   ------------------------

   function Coordinates_Within
     (Grid     : Hex_Grid;
      Hex      : Cube_Coordinate;
      Distance : Distance_Type)
      return Cube_Coordinate_Array
   is
      pragma Unreferenced (Grid);
   begin
      return Coordinates_Within (Hex, Distance);
   end Coordinates_Within;

   ------------------------
   -- Create_Square_Grid --
   ------------------------

   procedure Create_Square_Grid
     (Grid              : out Hex_Grid;
      Width             : Distance_Type;
      Height            : Distance_Type;
      Horizontal_Wrap   : Boolean;
      Vertical_Wrap     : Boolean;
      Has_Vertical_Axis : Boolean;
      Offset_Odd        : Boolean)
   is
   begin
      Grid.Width := Width;
      Grid.Height := Height;
      Grid.Horizontal_Wrap := Horizontal_Wrap;
      Grid.Vertical_Wrap := Vertical_Wrap;
      Grid.Has_Vertical_Axis := Has_Vertical_Axis;
      Grid.Offset_Odd := Offset_Odd;

      Grid.Tiles.Set_Length
        (Ada.Containers.Count_Type (Width * Height));

   end Create_Square_Grid;

   --------------
   -- Distance --
   --------------

   function Distance
     (Grid     : Hex_Grid;
      From, To : Cube_Coordinate)
      return Distance_Type
   is
      D : Distance_Type := Distance (From, To);
   begin
      if Grid.Horizontal_Wrap then
         if Grid.Has_Vertical_Axis then
            declare
               Center : constant Cube_Coordinate :=
                          (-Grid.Width, Grid.Width / 2, Grid.Width / 2);
            begin
               D := Distance_Type'Min (D, Distance (From, To + Center));
               D := Distance_Type'Min (D, Distance (From, To - Center));
            end;
         end if;
      end if;
      return D;
   end Distance;

   ---------------
   -- Find_Path --
   ---------------

   function Find_Path
     (Grid     : Hex_Grid;
      Start    : Cube_Coordinate;
      Finish   : Cube_Coordinate;
      Passable : not null access
        function (Tile : Hex_Tile) return Boolean;
      Cost     : not null access
        function (Tile : Hex_Tile) return Real)
      return Cube_Coordinate_Array
   is
      Count : constant Natural := Natural (Grid.Width * Grid.Height);

      package Frontier_Queues is
        new WL.Heaps (Real, Cube_Coordinate, ">");
      Frontier : Frontier_Queues.Heap;
      Visited : array (1 .. Count) of Boolean := (others => False);
      Came_From : array (1 .. Count) of Natural := (others => 0);
      Cost_So_Far : array (1 .. Count) of Real := (others => 0.0);
      Start_Index : constant Positive := Grid.Square_Tile_Index (Start);
      Finish_Index : constant Positive := Grid.Square_Tile_Index (Finish);
   begin
      Frontier.Insert (0.0, Start);
      Visited (Start_Index) := True;

      while not Frontier.Is_Empty loop
         declare
            Current      : constant Cube_Coordinate :=
                             Frontier.First_Element;
            Current_Cost : constant Real :=
                             Cost_So_Far (Grid.Square_Tile_Index (Current));
         begin
            Frontier.Delete_First;

            exit when Current = Finish;

            for N of Neighbours (Grid, Current) loop
               declare
                  Tile     : constant Hex_Tile := Get_Tile (Grid, Current);
               begin
                  if Passable (Tile) then
                     declare
                        New_Cost : constant Real :=
                                     Current_Cost + Cost (Tile);
                        Index    : constant Positive :=
                                     Grid.Square_Tile_Index (N);
                     begin
                        if not Visited (Index)
                          or else New_Cost < Cost_So_Far (Index)
                        then
                           if Visited (Index) then
                              Frontier.Replace
                                (New_Cost, N);
                           else
                              Frontier.Insert (New_Cost, N);
                              Visited (Index) := True;
                           end if;
                           Cost_So_Far (Index) := New_Cost;
                           Came_From (Index) :=
                             Grid.Square_Tile_Index (Current);
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;

      if Came_From (Finish_Index) /= 0 then
         declare
            Length : Natural := 0;
            It     : Natural := Finish_Index;
         begin
            while Came_From (It) /= 0 loop
               Length := Length + 1;
               It := Came_From (It);
            end loop;

            declare
               Path : Cube_Coordinate_Array (1 .. Length) :=
                        (others => (0, 0, 0));
            begin
               It := Finish_Index;
               while Came_From (It) /= 0 loop
                  Path (Length) := Grid.Square_Tile_Coordinate (It);
                  It := Came_From (It);
                  Length := Length - 1;
               end loop;

               return Path;
            end;
         end;
      else
         return Path : Cube_Coordinate_Array (1 .. 0);
      end if;
   end Find_Path;

   --------------
   -- Get_Tile --
   --------------

   function Get_Tile
     (Grid     : Hex_Grid;
      Position : Cube_Coordinate)
      return Hex_Tile
   is
      Index : constant Positive := Grid.Square_Tile_Index (Position);
   begin
      return Grid.Tiles.Element (Index);
   end Get_Tile;

   --------------
   -- Get_Tile --
   --------------

   function Get_Tile
     (Grid     : Hex_Grid;
      Position : Axial_Coordinate)
      return Hex_Tile
   is
   begin
      return Get_Tile (Grid, To_Cube (Position));
   end Get_Tile;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (Grid     : Hex_Grid;
      Position : Cube_Coordinate)
      return Cube_Coordinate_Array
   is
      Result : Cube_Coordinate_Array := Neighbours (Position);
      Back   : Natural := 0;
   begin
      if Grid.Horizontal_Wrap and then Grid.Has_Vertical_Axis then
         for I in Result'Range loop
            declare
               Offset : Offset_Coordinate :=
                          Grid.To_Offset_Coordinate (Result (I));
            begin
               if Offset.X < 0 then
                  Offset.X := Offset.X + Grid.Width;
               elsif Offset.X >= Grid.Width then
                  Offset.X := Offset.X - Grid.Width;
               end if;

               if Offset.Y not in 0 .. Grid.Height - 1 then
                  Back := Back + 1;
               else
                  Result (I - Back) := To_Cube (Offset);
               end if;
            end;
         end loop;
      end if;

      return Result (1 .. Result'Last - Back);
   end Neighbours;

   --------------------------
   -- Scan_Connected_Tiles --
   --------------------------

   procedure Scan_Connected_Tiles
     (Grid     : Hex_Grid;
      Start    : Cube_Coordinate;
      Test     : not null access
        function (Tile : Hex_Tile) return Boolean;
      Process  : not null access
        procedure (Tile : Hex_Tile))
   is
      package Position_Queues is
        new Ada.Containers.Doubly_Linked_Lists (Cube_Coordinate);

      Frontier : Position_Queues.List;
      Visited  : array (1 .. Natural (Grid.Width * Grid.Height)) of Boolean :=
                   (others => False);
   begin
      if not Test (Grid.Get_Tile (Start)) then
         return;
      end if;

      Visited (Grid.Square_Tile_Index (Start)) := True;
      Frontier.Append (Start);

      while not Frontier.Is_Empty loop
         declare
            Current : constant Cube_Coordinate := Frontier.First_Element;
            Tile    : constant Hex_Tile := Grid.Get_Tile (Current);
         begin
            Frontier.Delete_First;
            Process (Tile);
            for N of Grid.Neighbours (Current) loop
               declare
                  Index : constant Positive :=
                            Grid.Square_Tile_Index (N);
               begin
                  if not Visited (Index)
                    and then Test (Grid.Get_Tile (N))
                  then
                     Visited (Index) := True;
                     Frontier.Append (N);
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Scan_Connected_Tiles;

   ---------------------
   -- Scan_Neighbours --
   ---------------------

   procedure Scan_Neighbours
     (Grid    : Hex_Grid;
      Start   : Cube_Coordinate;
      Process : not null access
        procedure (Tile : Hex_Tile))
   is
      Ns : constant Cube_Coordinate_Array := Neighbours (Grid, Start);
   begin
      for N of Ns loop
         Process (Get_Tile (Grid, N));
      end loop;
   end Scan_Neighbours;

   ----------------
   -- Scan_Tiles --
   ----------------

   procedure Scan_Tiles
     (Grid     : Hex_Grid;
      Process  : not null access
        procedure (Tile : Hex_Tile))
   is
   begin
      for I in 1 .. Grid.Tiles.Last_Index loop
--           Ada.Text_IO.Put_Line
--             ("index:" & I'Image);
         Process (Grid.Tiles.Element (I));
      end loop;
   end Scan_Tiles;

   -----------------------
   -- Scan_Tiles_Within --
   -----------------------

   procedure Scan_Tiles_Within
     (Grid     : Hex_Grid;
      Start    : Cube_Coordinate;
      Distance : Distance_Type;
      Process  : not null access
        procedure (Tile : Hex_Tile))
   is
      Ns : constant Cube_Coordinate_Array :=
             Coordinates_Within (Grid, Start, Distance);
   begin
      for N of Ns loop
         Process (Get_Tile (Grid, N));
      end loop;
   end Scan_Tiles_Within;

   --------------
   -- Set_Tile --
   --------------

   procedure Set_Tile
     (Grid     : in out Hex_Grid;
      Position : Cube_Coordinate;
      Tile     : Hex_Tile)
   is
      Index : constant Positive := Grid.Square_Tile_Index (Position);
   begin
      Grid.Tiles.Replace_Element (Index, Tile);
   end Set_Tile;

   --------------
   -- Set_Tile --
   --------------

   procedure Set_Tile
     (Grid     : in out Hex_Grid;
      Position : Axial_Coordinate;
      Tile     : Hex_Tile)
   is
   begin
      Set_Tile (Grid, To_Cube (Position), Tile);
   end Set_Tile;

   ----------------------------
   -- Square_Tile_Coordinate --
   ----------------------------

   function Square_Tile_Coordinate
     (Grid  : Hex_Grid;
      Index : Positive)
      return Cube_Coordinate
   is
      Offset : constant Offset_Coordinate :=
                 Offset_Coordinate'
                   (X           => Distance_Type (Index - 1) mod Grid.Width,
                    Y           => Distance_Type (Index - 1) / Grid.Width,
                    Offset_Rows => not Grid.Has_Vertical_Axis,
                    Offset_Odd  => Grid.Offset_Odd);
   begin
      return To_Cube (Offset);
   end Square_Tile_Coordinate;

   -----------------------
   -- Square_Tile_Index --
   -----------------------

   function Square_Tile_Index
     (Grid : Hex_Grid;
      Cube : Cube_Coordinate)
      return Positive
   is
      Offset : constant Offset_Coordinate :=
                 Grid.To_Offset_Coordinate (Cube);
   begin
      return Index : constant Positive :=
        Natural (Offset.Y * Grid.Width + Offset.X) + 1
      do
         pragma Assert
           (Index in 1 .. Positive (Grid.Width) * Positive (Grid.Height));
      end return;
   end Square_Tile_Index;

   ------------------------
   -- To_Cube_Coordinate --
   ------------------------

   function To_Cube_Coordinate
     (Grid          : Hex_Grid;
      Across_Offset : Distance_Type;
      Down_Offset   : Distance_Type)
      return Cube_Coordinate
   is
   begin
      return To_Cube
        (Offset_Coordinate'
           (X           => Across_Offset,
            Y           => Down_Offset,
            Offset_Rows => not Grid.Has_Vertical_Axis,
            Offset_Odd  => Grid.Offset_Odd));
   end To_Cube_Coordinate;

end Hexes.Grids;
