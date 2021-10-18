with Ada.Containers.Vectors;

private package Hexes.Cube_Vectors is

   package Vectors is
     new Ada.Containers.Vectors (Positive, Cube_Coordinate);

   subtype Vector is Vectors.Vector;

   function To_Array
     (Vector : Vectors.Vector)
      return Cube_Coordinate_Array;

end Hexes.Cube_Vectors;
