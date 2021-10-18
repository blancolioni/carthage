with Carthage.Handles.Terrain;

package Carthage.Handles.Tiles.Configure is

   function Create_Tile
     (Planet          : Planet_Reference;
      Position        : Tile_Position;
      Height          : Integer;
      Base_Terrain    : Carthage.Handles.Terrain.Terrain_Handle;
      Feature_Terrain : Carthage.Handles.Terrain.Terrain_Handle)
      return Tile_Handle;

   function Create_Tile
     (Planet   : Planet_Reference;
      Position : Tile_Position;
      Terrain  : Terrain_Layer_Array;
      Road     : Boolean;
      River    : Boolean)
      return Tile_Handle;

end Carthage.Handles.Tiles.Configure;
