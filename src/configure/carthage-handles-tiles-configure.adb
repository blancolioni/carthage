package body Carthage.Handles.Tiles.Configure is

   -----------------
   -- Create_Tile --
   -----------------

   function Create_Tile
     (Planet          : Planet_Reference;
      Position        : Tile_Position;
      Height          : Integer;
      Base_Terrain    : Carthage.Handles.Terrain.Terrain_Handle;
      Feature_Terrain : Carthage.Handles.Terrain.Terrain_Handle)
      return Tile_Handle
   is
      pragma Unreferenced (Height);
   begin
      return Create_Tile
        (Planet   => Planet,
         Position => Position,
         Terrain  => (Base_Terrain, Feature_Terrain),
         Road     => False,
         River    => False);
   end Create_Tile;

   -----------------
   -- Create_Tile --
   -----------------

   function Create_Tile
     (Planet   : Planet_Reference;
      Position : Tile_Position;
      Terrain  : Terrain_Layer_Array;
      Road     : Boolean;
      River    : Boolean)
      return Tile_Handle
   is
      Reference : constant Tile_Reference :=
                    Create_Tile
                      (Planet   => Planet,
                       Position => Position,
                       Terrain  => Terrain,
                       Road     => Road,
                       River    => River);
   begin
      return Get (Reference);
   end Create_Tile;

end Carthage.Handles.Tiles.Configure;
