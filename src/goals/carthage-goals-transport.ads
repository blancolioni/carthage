with Carthage.Handles.Resources;
with Carthage.Handles.Tiles;

with Carthage.Quantities;

package Carthage.Goals.Transport is

   function Is_Transport_Goal
     (Goal : Goal_Record'Class)
      return Boolean;

   function Get_Resource
     (Goal : Goal_Record'Class)
      return Carthage.Handles.Resources.Resource_Handle
     with Pre => Is_Transport_Goal (Goal);

   function Get_Quantity
     (Goal : Goal_Record'Class)
      return Carthage.Quantities.Quantity_Type
     with Pre => Is_Transport_Goal (Goal);

   procedure Set_Quantity
     (Goal     : in out Goal_Record'Class;
      Quantity : Carthage.Quantities.Quantity_Type)
     with Pre => Is_Transport_Goal (Goal),
     Post => Carthage.Quantities."="
       (Get_Quantity (Goal), Quantity);

   function Is_Resource_Request_Goal
     (Goal : Goal_Record'Class)
      return Boolean;

   function Get_Resource_Destination
     (Goal : Goal_Record'Class)
      return Carthage.Handles.Tiles.Tile_Handle
     with Pre => Is_Resource_Request_Goal (Goal);

   function Is_Resource_Available_Goal
     (Goal : Goal_Record'Class)
      return Boolean;

   function Get_Resource_Source
     (Goal : Goal_Record'Class)
      return Carthage.Handles.Tiles.Tile_Handle
     with Pre => Is_Resource_Available_Goal (Goal);

   function Resource_Request
     (Destination : Carthage.Handles.Tiles.Tile_Handle;
      Resource    : Carthage.Handles.Resources.Resource_Handle;
      Quantity    : Carthage.Quantities.Quantity_Type;
      Priority    : Goal_Priority := Middle_Priority)
      return Goal_Record'Class;

   function Resource_Available
     (Source   : Carthage.Handles.Tiles.Tile_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
      return Goal_Record'Class;

private

   type Transport_Goal is
     abstract new Goal_Record with
      record
         Resource : Carthage.Handles.Resource_Reference;
         Quantity : Carthage.Quantities.Quantity_Type;
      end record;

   function Is_Transport_Goal
     (Goal : Goal_Record'Class)
      return Boolean
   is (Goal in Transport_Goal'Class);

   function Get_Resource
     (Goal : Goal_Record'Class)
      return Carthage.Handles.Resources.Resource_Handle
   is (Carthage.Handles.Resources.Get
         (Transport_Goal (Goal).Resource));

   function Get_Quantity
     (Goal : Goal_Record'Class)
      return Carthage.Quantities.Quantity_Type
   is (Transport_Goal (Goal).Quantity);

   type Resource_Request_Goal is
     new Transport_Goal with
      record
         Destination : Carthage.Handles.Tile_Reference;
      end record;

   overriding function Show (Goal : Resource_Request_Goal) return String
   is ("request "
       & Carthage.Quantities.Show (Goal.Quantity)
       & " " & Carthage.Handles.Resources.Get (Goal.Resource).Local_Text);

   function Get_Resource_Destination
     (Goal : Goal_Record'Class)
      return Carthage.Handles.Tiles.Tile_Handle
   is (Carthage.Handles.Tiles.Get
         (Resource_Request_Goal'Class (Goal).Destination));

   type Resource_Supply_Goal is
     new Transport_Goal with
      record
         Source : Carthage.Handles.Tile_Reference;
      end record;

   overriding function Show (Goal : Resource_Supply_Goal) return String
   is ("supply "
       & Carthage.Quantities.Show (Goal.Quantity)
       & " " & Carthage.Handles.Resources.Get (Goal.Resource).Local_Text);

   function Get_Resource_Source
     (Goal : Goal_Record'Class)
      return Carthage.Handles.Tiles.Tile_Handle
   is (Carthage.Handles.Tiles.Get
       (Resource_Supply_Goal'Class (Goal).Source));

end Carthage.Goals.Transport;
