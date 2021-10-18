package body Carthage.Handles.Stacks.Create is

   --------------------------
   -- Create_Orbital_Stack --
   --------------------------

   function Create_Orbital_Stack
     (Owner     : Carthage.Handles.Houses.House_Handle;
      Planet    : Planet_Reference)
     return Stack_Reference
   is
      Reference : constant Stack_Reference :=
                    Create_Stack
                      (Owner  => Owner.Reference,
                       Planet => Planet,
                       Tile   => Null_Tile_Reference);
   begin
      return Reference;
   end Create_Orbital_Stack;

   ----------------------
   -- New_Ground_Stack --
   ----------------------

   function New_Ground_Stack
     (Manager   : access Stack_Manager_Interface'Class;
      Owner     : Carthage.Handles.Houses.House_Handle;
      Planet    : Planet_Reference;
      Tile      : Carthage.Handles.Tiles.Tile_Handle)
      return Stack_Handle
   is
      pragma Unreferenced (Manager);
      Reference : constant Stack_Reference :=
                    Create_Stack
                      (Owner  => Owner.Reference,
                       Planet => Planet,
                       Tile   => Tile.Reference);
   begin
      return Get (Reference);
   end New_Ground_Stack;

end Carthage.Handles.Stacks.Create;
