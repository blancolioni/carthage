package body Hexes is

   Cube_Directions : constant Cube_Coordinate_Array :=
                       (1 => (1, -1, 0),
                        2 => (1, 0, -1),
                        3 => (0, 1, -1),
                        4 => (-1, 1, 0),
                        5 => (-1, 0, 1),
                        6 => (0, -1, 1));

   ------------------------
   -- Coordinates_Within --
   ------------------------

   function Coordinates_Within
     (Hex      : Cube_Coordinate;
      Distance : Distance_Type)
      return Cube_Coordinate_Array
   is
      Max : constant Natural := (2 * Natural (Distance) + 1) ** 2;

      Result : Cube_Coordinate_Array (1 .. Max);
      Count  : Natural := 0;
   begin
      for DX in -Distance .. Distance loop
         for DY in Coordinate_Type'Max (-Distance, -DX - Distance)
           .. Coordinate_Type'Min (Distance, -DX + Distance)
         loop
            Count := Count + 1;
            Result (Count) := Hex + (DX, DY, -DX - DY);
         end loop;
      end loop;
      return Result (1 .. Count);
   end Coordinates_Within;

   ------------------------
   -- Coordinates_Within --
   ------------------------

   function Coordinates_Within
     (Hex      : Axial_Coordinate;
      Distance : Distance_Type)
      return Axial_Coordinate_Array
   is
   begin
      return To_Axial_Array
        (Coordinates_Within (To_Cube (Hex), Distance));
   end Coordinates_Within;

   --------------
   -- Distance --
   --------------

   function Distance (From, To : Cube_Coordinate) return Distance_Type is
   begin
      return (abs (From.X - To.X) + abs (From.Y - To.Y) + abs (From.Z - To.Z))
        / 2;
   end Distance;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (Hex : Cube_Coordinate)
      return Cube_Coordinate_Array
   is
   begin
      return Ns : Cube_Coordinate_Array := Cube_Directions do
         for N of Ns loop
            N.X := N.X + Hex.X;
            N.Y := N.Y + Hex.Y;
            N.Z := N.Z + Hex.Z;
         end loop;
      end return;
   end Neighbours;

   ----------------
   -- Neighbours --
   ----------------

   function Neighbours
     (Hex : Axial_Coordinate)
      return Axial_Coordinate_Array
   is
   begin
      return To_Axial_Array
        (Neighbours (To_Cube (Hex)));
   end Neighbours;

   --------------------
   -- To_Axial_Array --
   --------------------

   function To_Axial_Array
     (Cubes : Cube_Coordinate_Array)
      return Axial_Coordinate_Array
   is
   begin
      return Axials : Axial_Coordinate_Array (Cubes'Range) do
         for I in Axials'Range loop
            Axials (I) := To_Axial (Cubes (I));
         end loop;
      end return;
   end To_Axial_Array;

   -------------
   -- To_Cube --
   -------------

   function To_Cube
     (Offset : Offset_Coordinate)
      return Cube_Coordinate
   is
      X, Z : Coordinate_Type;
   begin
      if Offset.Offset_Rows then
         Z := Offset.Y;
         if Offset.Offset_Odd then
            X := Offset.X - (Offset.Y - (Offset.Y mod 2)) / 2;
         else
            X := Offset.X - (Offset.Y + (Offset.Y mod 2)) / 2;
         end if;
      else
         X := Offset.X;
         if Offset.Offset_Odd then
            Z := Offset.Y - (Offset.X - (Offset.X mod 2)) / 2;
         else
            Z := Offset.Y - (Offset.X + (Offset.X mod 2)) / 2;
         end if;
      end if;
      return (X, -X - Z, Z);
   end To_Cube;

   -------------------
   -- To_Cube_Array --
   -------------------

   function To_Cube_Array
     (Axials : Axial_Coordinate_Array)
      return Cube_Coordinate_Array
   is
   begin
      return Cubes : Cube_Coordinate_Array (Axials'Range) do
         for I in Cubes'Range loop
            Cubes (I) := To_Cube (Axials (I));
         end loop;
      end return;
   end To_Cube_Array;

   ---------------
   -- To_Offset --
   ---------------

   function To_Offset
     (Cube        : Cube_Coordinate;
      Offset_Rows : Boolean;
      Offset_Odd  : Boolean)
      return Offset_Coordinate
   is
      X : Coordinate_Type := Cube.X;
      Y : Coordinate_Type := Cube.Z;
   begin
      if Offset_Rows then
         if Offset_Odd then
            X := X + (Y - Y mod 2) / 2;
         else
            X := X + (Y + Y mod 2) / 2;
         end if;
      else
         if Offset_Odd then
            Y := Y + (X - X mod 2) / 2;
         else
            Y := Y + (X + X mod 2) / 2;
         end if;
      end if;
      return (X, Y, Offset_Rows, Offset_Odd);
   end To_Offset;

end Hexes;
