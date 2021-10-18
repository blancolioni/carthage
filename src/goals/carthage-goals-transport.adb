------------------------------
-- Carthage.Goals.Transport --
------------------------------

package body Carthage.Goals.Transport is

   function Is_Resource_Request_Goal
     (Goal : Goal_Record'Class)
      return Boolean
   is (Goal in Resource_Request_Goal'Class);

   function Is_Resource_Available_Goal
     (Goal : Goal_Record'Class)
      return Boolean
   is (Goal in Resource_Supply_Goal'Class);

   ------------------------
   -- Resource_Available --
   ------------------------

   function Resource_Available
     (Source   : Carthage.Handles.Tiles.Tile_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
      return Goal_Record'Class
   is
   begin
      return Resource_Supply_Goal'
        (Priority => Middle_Priority,
         Resource => Resource.Reference,
         Quantity => Quantity,
         Source   => Source.Reference);
   end Resource_Available;

   ----------------------
   -- Resource_Request --
   ----------------------

   function Resource_Request
     (Destination : Carthage.Handles.Tiles.Tile_Handle;
      Resource    : Carthage.Handles.Resources.Resource_Handle;
      Quantity    : Carthage.Quantities.Quantity_Type;
      Priority    : Goal_Priority := Middle_Priority)
      return Goal_Record'Class
   is
   begin
      return Resource_Request_Goal'
        (Priority => Priority,
         Resource => Resource.Reference,
         Quantity => Quantity,
         Destination => Destination.Reference);
   end Resource_Request;

   ------------------
   -- Set_Quantity --
   ------------------

   procedure Set_Quantity
     (Goal     : in out Goal_Record'Class;
      Quantity : Carthage.Quantities.Quantity_Type)
   is
   begin
      Transport_Goal'Class (Goal).Quantity := Quantity;
   end Set_Quantity;

end Carthage.Goals.Transport;
