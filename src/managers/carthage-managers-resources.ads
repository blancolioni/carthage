private with Ada.Containers.Ordered_Maps;
private with WL.String_Maps;

with Carthage.Handles.Planets;
with Carthage.Handles.Tiles;

with Carthage.Messages;
with Carthage.Quantities;

package Carthage.Managers.Resources is

   subtype Parent is Carthage.Managers.Instance;

   type Instance is new Parent with private;
   subtype Any_Instance is Instance'Class;
   type Reference is access all Instance'Class;

   procedure Create_Planet_Resource_Manager
     (House  : Carthage.Handles.Houses.House_Handle;
      Planet : Carthage.Handles.Planets.Planet_Handle);

private

   task type Resource_Manager_Task is
      entry Start (Manager_Ref : Reference);
   end Resource_Manager_Task;

   type Task_Reference is access Resource_Manager_Task;

   type Resource_Record is
      record
         Quantity  : Carthage.Quantities.Quantity_Type;
         Minimum   : Carthage.Quantities.Quantity_Type;
         Fulfilled : Carthage.Quantities.Quantity_Type;
      end record;

   function Less_Than
     (Left, Right : Carthage.Handles.Tiles.Tile_Handle)
      return Boolean
   is (Left.Position.Y < Right.Position.Y
       or else (Left.Position.Y = Right.Position.Y
         and then Left.Position.X < Right.Position.X));

   package Tile_Quantity_Maps is
     new Ada.Containers.Ordered_Maps
       (Carthage.Handles.Tiles.Tile_Handle,
        Resource_Record,
        Less_Than);

   package Resource_Maps is
     new WL.String_Maps (Tile_Quantity_Maps.Map, Tile_Quantity_Maps."=");

   type Instance is new Parent with
      record
         Planet        : Carthage.Handles.Planets.Planet_Handle;
         Resource_Bank : Carthage.Messages.Message_Bank_Reference;
         Resource_Task : Task_Reference;
         Requirements  : Resource_Maps.Map;
         Supply        : Resource_Maps.Map;
      end record;

   overriding function Name (This : Instance) return String;
   overriding procedure Stop (This : in out Instance);

   procedure Resolve_Resource_Requests (This : in out Instance'Class);

end Carthage.Managers.Resources;
