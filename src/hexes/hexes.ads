package Hexes is

   type Coordinate_Type is new Integer;

   subtype Distance_Type is Coordinate_Type range 0 .. Coordinate_Type'Last;

   type Cube_Coordinate is private;

   function "+" (Left, Right : Cube_Coordinate) return Cube_Coordinate;
   function "-" (Left, Right : Cube_Coordinate) return Cube_Coordinate;

   function Cube_X (Cube : Cube_Coordinate) return Coordinate_Type;
   function Cube_Y (Cube : Cube_Coordinate) return Coordinate_Type;
   function Cube_Z (Cube : Cube_Coordinate) return Coordinate_Type;

   type Cube_Coordinate_Array is array (Positive range <>) of Cube_Coordinate;

   function Distance (From, To : Cube_Coordinate) return Distance_Type;

   function Neighbours
     (Hex : Cube_Coordinate)
      return Cube_Coordinate_Array;

   function Coordinates_Within
     (Hex      : Cube_Coordinate;
      Distance : Distance_Type)
      return Cube_Coordinate_Array;

   function Image (Hex : Cube_Coordinate) return String;

   type Axial_Coordinate is private;

   type Axial_Coordinate_Array is
     array (Positive range <>) of Axial_Coordinate;

   function Neighbours
     (Hex : Axial_Coordinate)
      return Axial_Coordinate_Array;

   function Coordinates_Within
     (Hex      : Axial_Coordinate;
      Distance : Distance_Type)
      return Axial_Coordinate_Array;

   function Image (Hex : Axial_Coordinate) return String;

   type Offset_Coordinate is private;

   function Offset (X, Y        : Distance_Type;
                    Offset_Rows : Boolean;
                    Offset_Odd  : Boolean)
                    return Offset_Coordinate;

   function Offset_X (Hex : Offset_Coordinate) return Distance_Type;
   function Offset_Y (Hex : Offset_Coordinate) return Distance_Type;

   function To_Axial
     (Cube : Cube_Coordinate)
      return Axial_Coordinate;

   function To_Offset
     (Cube        : Cube_Coordinate;
      Offset_Rows : Boolean;
      Offset_Odd  : Boolean)
      return Offset_Coordinate;

   function To_Cube
     (Axial : Axial_Coordinate)
      return Cube_Coordinate;

   function To_Cube
     (Offset : Offset_Coordinate)
      return Cube_Coordinate;

private

   type Cube_Coordinate is
      record
         X, Y, Z : Coordinate_Type := 0;
      end record
   with Invariant => X + Y + Z = 0;

   function "+" (Left, Right : Cube_Coordinate) return Cube_Coordinate
   is (Left.X + Right.X, Left.Y + Right.Y, Left.Z + Right.Z);

   function "-" (Left, Right : Cube_Coordinate) return Cube_Coordinate
   is (Left.X - Right.X, Left.Y - Right.Y, Left.Z - Right.Z);

   function Cube_X (Cube : Cube_Coordinate) return Coordinate_Type
   is (Cube.X);

   function Cube_Y (Cube : Cube_Coordinate) return Coordinate_Type
   is (Cube.Y);

   function Cube_Z (Cube : Cube_Coordinate) return Coordinate_Type
   is (Cube.Z);

   type Axial_Coordinate is
      record
         Q, R : Coordinate_Type := 0;
      end record;

   type Offset_Coordinate is
      record
         X, Y        : Coordinate_Type := 0;
         Offset_Rows : Boolean;
         Offset_Odd  : Boolean;
      end record;

   function Offset (X, Y        : Distance_Type;
                    Offset_Rows : Boolean;
                    Offset_Odd  : Boolean)
                    return Offset_Coordinate
   is (X, Y, Offset_Rows, Offset_Odd);

   function Offset_X (Hex : Offset_Coordinate) return Distance_Type
   is (Hex.X);

   function Offset_Y (Hex : Offset_Coordinate) return Distance_Type
   is (Hex.Y);

   function To_Axial
     (Cube : Cube_Coordinate)
      return Axial_Coordinate
   is (Cube.X, Cube.Z);

   function To_Cube
     (Axial : Axial_Coordinate)
      return Cube_Coordinate
   is (Axial.Q, -Axial.Q - Axial.R, Axial.R);

   function To_Axial_Array
     (Cubes : Cube_Coordinate_Array)
      return Axial_Coordinate_Array;

   function To_Cube_Array
     (Axials : Axial_Coordinate_Array)
      return Cube_Coordinate_Array;

   function Image (Hex : Cube_Coordinate) return String
   is ("<x" & Hex.X'Image & " y" & Hex.Y'Image & " z" & Hex.Z'Image & ">");

   function Image (Hex : Axial_Coordinate) return String
   is ("<q" & Hex.Q'Image & " r" & Hex.R'Image & ">");

end Hexes;
