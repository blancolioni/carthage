with Carthage.Handles.Planets;
with Carthage.Handles.Tiles;

package Carthage.Handles.Assets.Moves is

   procedure Start_Jump
     (Asset       : Asset_Handle;
      Start       : Carthage.Handles.Planets.Planet_Handle;
      Destination : Carthage.Handles.Planets.Planet_Handle);

   procedure Start_Launch
     (Asset : Asset_Handle);

   procedure Start_Landing
     (Asset : Asset_Handle;
      Tile  : Carthage.Handles.Tiles.Tile_Handle);

end Carthage.Handles.Assets.Moves;
