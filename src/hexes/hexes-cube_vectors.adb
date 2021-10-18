package body Hexes.Cube_Vectors is

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Vector : Vectors.Vector)
      return Cube_Coordinate_Array
   is
   begin
      return A : Cube_Coordinate_Array (1 .. Vector.Last_Index) :=
        (others => (0, 0, 0))
      do
         for I in A'Range loop
            A (I) := Vector (I);
         end loop;
      end return;
   end To_Array;

end Hexes.Cube_Vectors;
