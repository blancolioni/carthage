private with Ada.Containers.Vectors;

generic
   type Hex_Tile is private;
   type Real is digits <>;
   with function "=" (Left, Right : Hex_Tile) return Boolean is <>;
package Hexes.Grids is

   type Hex_Grid is tagged private;

   procedure Create_Square_Grid
     (Grid              : out Hex_Grid;
      Width             : Distance_Type;
      Height            : Distance_Type;
      Horizontal_Wrap   : Boolean;
      Vertical_Wrap     : Boolean;
      Has_Vertical_Axis : Boolean;
      Offset_Odd        : Boolean);

   function Width (Grid : Hex_Grid) return Distance_Type;
   function Height (Grid : Hex_Grid) return Distance_Type;

   function To_Offset_Coordinate
     (Grid : Hex_Grid;
      Cube : Cube_Coordinate)
      return Offset_Coordinate;

   function To_Cube_Coordinate
     (Grid          : Hex_Grid;
      Across_Offset : Distance_Type;
      Down_Offset   : Distance_Type)
      return Cube_Coordinate
     with Pre => Across_Offset < Width (Grid)
     and then Down_Offset < Height (Grid);
--
--     function To_Axial_Coordinate
--       (Grid          : Hex_Grid;
--        Across_Offset : Distance_Type;
--        Down_Offset   : Distance_Type)
--        return Axial_Coordinate
--       with Pre => Across_Offset < Width (Grid)
--       and then Down_Offset < Height (Grid);

   procedure Set_Tile
     (Grid     : in out Hex_Grid;
      Position : Cube_Coordinate;
      Tile     : Hex_Tile);

   procedure Set_Tile
     (Grid     : in out Hex_Grid;
      Position : Axial_Coordinate;
      Tile     : Hex_Tile);

   function Get_Tile
     (Grid     : Hex_Grid;
      Position : Cube_Coordinate)
      return Hex_Tile;

   function Get_Tile
     (Grid     : Hex_Grid;
      Position : Axial_Coordinate)
      return Hex_Tile;

   function Distance
     (Grid     : Hex_Grid;
      From, To : Cube_Coordinate) return Distance_Type;

   function Neighbours
     (Grid     : Hex_Grid;
      Position : Cube_Coordinate)
      return Cube_Coordinate_Array;

   function Coordinates_Within
     (Grid     : Hex_Grid;
      Hex      : Cube_Coordinate;
      Distance : Distance_Type)
      return Cube_Coordinate_Array;

   function Find_Path
     (Grid     : Hex_Grid;
      Start    : Cube_Coordinate;
      Finish   : Cube_Coordinate;
      Passable : not null access
        function (Tile : Hex_Tile) return Boolean;
      Cost     : not null access
        function (Tile : Hex_Tile) return Real)
      return Cube_Coordinate_Array;

   procedure Scan_Neighbours
     (Grid    : Hex_Grid;
      Start   : Cube_Coordinate;
      Process : not null access
        procedure (Tile : Hex_Tile));

   procedure Scan_Tiles_Within
     (Grid     : Hex_Grid;
      Start    : Cube_Coordinate;
      Distance : Distance_Type;
      Process  : not null access
        procedure (Tile : Hex_Tile));

   procedure Scan_Connected_Tiles
     (Grid     : Hex_Grid;
      Start    : Cube_Coordinate;
      Test     : not null access
        function (Tile : Hex_Tile) return Boolean;
      Process  : not null access
        procedure (Tile : Hex_Tile));

   procedure Scan_Tiles
     (Grid     : Hex_Grid;
      Process  : not null access
        procedure (Tile : Hex_Tile));

private

   package Tile_Vectors is
     new Ada.Containers.Vectors (Positive, Hex_Tile);

   type Hex_Grid is tagged
      record
         Width             : Distance_Type;
         Height            : Distance_Type;
         Tiles             : Tile_Vectors.Vector;
         Horizontal_Wrap   : Boolean;
         Vertical_Wrap     : Boolean;
         Has_Vertical_Axis : Boolean;
         Offset_Odd        : Boolean;
      end record;

   function Square_Tile_Index
     (Grid : Hex_Grid;
      Cube : Cube_Coordinate)
      return Positive;

   function Square_Tile_Coordinate
     (Grid : Hex_Grid;
      Index : Positive)
     return Cube_Coordinate;

   --     procedure Get_XY
--       (Grid  : Hex_Grid;
--        Axial : Axial_Coordinate;
--        X, Y  : out Natural);
--
--     function Get_Tile_Index
--       (Grid  : Hex_Grid;
--        Axial : Axial_Coordinate)
--        return Positive;
--
--     function Get_Tile_Index
--       (Grid  : Hex_Grid;
--        Cube  : Cube_Coordinate)
--        return Positive
--     is (Get_Tile_Index (Grid, To_Axial (Cube)));
--
--     function Get_Tile_Index_Coordinate
--       (Grid  : Hex_Grid;
--        Index : Positive)
--        return Axial_Coordinate;
--
--     function Get_Tile_Index_Coordinate
--       (Grid  : Hex_Grid;
--        Index : Positive)
--        return Cube_Coordinate
--     is (To_Cube (Get_Tile_Index_Coordinate (Grid, Index)));
--
--     function To_Cube_Coordinate
--       (Grid          : Hex_Grid;
--        Across_Offset : Distance_Type;
--        Down_Offset   : Distance_Type)
--        return Cube_Coordinate
--     is (To_Cube (To_Axial_Coordinate (Grid, Across_Offset, Down_Offset)));

   function Width (Grid : Hex_Grid) return Distance_Type
   is (Grid.Width);

   function Height (Grid : Hex_Grid) return Distance_Type
   is (Grid.Height);

   function To_Offset_Coordinate
     (Grid : Hex_Grid;
      Cube : Cube_Coordinate)
      return Offset_Coordinate
   is (To_Offset
       (Cube        => Cube,
        Offset_Rows => not Grid.Has_Vertical_Axis,
        Offset_Odd  => Grid.Offset_Odd));

end Hexes.Grids;
