package Carthage.Handles.Stacks.Create is

   function New_Ground_Stack
     (Manager   : access Stack_Manager_Interface'Class;
      Owner     : Carthage.Handles.Houses.House_Handle;
      Planet    : Planet_Reference;
      Tile      : Carthage.Handles.Tiles.Tile_Handle)
      return Stack_Handle
     with Post => Tile.Has_Stacks
     and then Carthage.Handles.Tiles."="
       (New_Ground_Stack'Result.Current_Tile, Tile)
       and then New_Ground_Stack'Result.Planet = Planet
       and then New_Ground_Stack'Result.Owner.Reference = Owner.Reference;

   function Create_Orbital_Stack
     (Owner     : Carthage.Handles.Houses.House_Handle;
      Planet    : Planet_Reference)
     return Stack_Reference;

end Carthage.Handles.Stacks.Create;
