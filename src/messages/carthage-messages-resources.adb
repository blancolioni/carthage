with Carthage.Handles.Planets;

package body Carthage.Messages.Resources is

   function Available
     (House    : Carthage.Handles.Houses.House_Handle;
      Tile     : Carthage.Handles.Tiles.Tile_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
      return Message_Interface'Class
   is
   begin
      return Resource_Available_Message'
        (Resource => Resource,
         Quantity => Quantity,
         House    => House,
         Tile     => Tile);
   end Available;

   -----------------
   -- Description --
   -----------------

   overriding function Description
     (This : Resource_Available_Message)
      return String
   is
   begin
      return This.House.Tag & ": available: "
        & This.Resource.Tag & "; location "
        & This.Tile.Short_Name
        & "; quantity " & Carthage.Quantities.Show (This.Quantity);
   end Description;

   -----------------
   -- Description --
   -----------------

   overriding function Description
     (This : Resource_Required_Message)
      return String
   is
   begin
      return This.House.Tag & ": required: "
        & This.Resource.Tag & "; location "
        & This.Tile.Short_Name
        & "; quantity " & Carthage.Quantities.Show (This.Quantity)
        & "; minimum " & Carthage.Quantities.Show (This.Minimum);
   end Description;

   --------------
   -- Required --
   --------------

   function Required
     (House    : Carthage.Handles.Houses.House_Handle;
      Tile     : Carthage.Handles.Tiles.Tile_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type;
      Minimum  : Carthage.Quantities.Quantity_Type)
      return Message_Interface'Class
   is
   begin
      return Resource_Required_Message'
        (Resource => Resource,
         Quantity => Quantity,
         Minimum  => Minimum,
         House    => House,
         Tile     => Tile);
   end Required;

   ---------
   -- Tag --
   ---------

   overriding function Tag (This : Resource_Message) return String is
   begin
      return This.House.Tag
        & "--"
        & Carthage.Handles.Planets.Get (This.Tile.Planet).Tag
        & "--"
        & "resources";
   end Tag;

end Carthage.Messages.Resources;
