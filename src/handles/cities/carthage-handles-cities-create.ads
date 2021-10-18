with Carthage.Handles.Planets;
with Carthage.Handles.Structures;
with Carthage.Handles.Tiles;

package Carthage.Handles.Cities.Create is

   function New_City
     (Planet    : Carthage.Handles.Planets.Planet_Handle;
      Tile      : Carthage.Handles.Tiles.Tile_Handle;
      Structure : Carthage.Handles.Structures.Structure_Handle;
      Owner     : Carthage.Handles.Houses.House_Handle;
      Health    : Health_Type;
      Loyalty   : Loyalty_Type)
      return Carthage.Handles.Cities.City_Handle
     with Pre => not Carthage.Handles.Tiles.Has_City (Tile),
     Post => New_City'Result.Has_Element
     and then Carthage.Handles.Tiles.Has_City (Tile)
     and then Get (Carthage.Handles.Tiles.Get_City (Tile)).Identifier
       = New_City'Result.Identifier;

   procedure New_City
     (Planet    : Carthage.Handles.Planets.Planet_Handle;
      Tile      : Carthage.Handles.Tiles.Tile_Handle;
      Structure : Carthage.Handles.Structures.Structure_Handle;
      Owner     : Carthage.Handles.Houses.House_Handle;
      Health    : Health_Type;
      Loyalty   : Loyalty_Type)
     with Pre => not Carthage.Handles.Tiles.Has_City (Tile),
     Post => Carthage.Handles.Tiles.Has_City (Tile);

end Carthage.Handles.Cities.Create;
