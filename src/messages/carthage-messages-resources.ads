with Carthage.Handles.Houses;
with Carthage.Handles.Resources;
with Carthage.Handles.Tiles;
with Carthage.Quantities;

package Carthage.Messages.Resources is

   function Is_Resource_Message
     (This : Message_Interface'Class)
      return Boolean;

   type Resource_Message is abstract new Message_Interface with private;

   function Is_Resource_Available_Message
     (This : Resource_Message'Class)
      return Boolean;

   function Is_Resource_Required_Message
     (This : Resource_Message'Class)
      return Boolean;

   function Resource (This : Resource_Message'Class)
                      return Carthage.Handles.Resources.Resource_Handle;

   function Quantity (This : Resource_Message'Class)
                      return Carthage.Quantities.Quantity_Type;

   function Minimum
     (This : Resource_Message)
      return Carthage.Quantities.Quantity_Type;

   function Tile (This : Resource_Message'Class)
                  return Carthage.Handles.Tiles.Tile_Handle;

   type Resource_Available_Message is new Resource_Message with private;

   type Resource_Required_Message is new Resource_Message with private;

   overriding function Minimum
     (This : Resource_Required_Message)
      return Carthage.Quantities.Quantity_Type;

   function Available
     (House    : Carthage.Handles.Houses.House_Handle;
      Tile     : Carthage.Handles.Tiles.Tile_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
      return Message_Interface'Class;

   function Required
     (House    : Carthage.Handles.Houses.House_Handle;
      Tile     : Carthage.Handles.Tiles.Tile_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type;
      Minimum  : Carthage.Quantities.Quantity_Type)
      return Message_Interface'Class;

private

   type Resource_Message is abstract new Message_Interface with
      record
         House    : Carthage.Handles.Houses.House_Handle;
         Tile     : Carthage.Handles.Tiles.Tile_Handle;
         Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type;
      end record;

   overriding function Tag (This : Resource_Message) return String;

   function Resource (This : Resource_Message'Class)
                      return Carthage.Handles.Resources.Resource_Handle
   is (This.Resource);

   function Quantity (This : Resource_Message'Class)
                      return Carthage.Quantities.Quantity_Type
   is (This.Quantity);

   function Minimum
     (This : Resource_Message)
      return Carthage.Quantities.Quantity_Type
   is (Carthage.Quantities.Zero);

   function Tile (This : Resource_Message'Class)
                  return Carthage.Handles.Tiles.Tile_Handle
   is (This.Tile);

   function Is_Resource_Message
     (This : Message_Interface'Class)
      return Boolean
   is (This in Resource_Message'Class);

   type Resource_Available_Message is new Resource_Message with
      record
         null;
      end record;

   overriding function Description
     (This : Resource_Available_Message)
      return String;

   type Resource_Required_Message is new Resource_Message with
      record
         Minimum : Carthage.Quantities.Quantity_Type;
      end record;

   overriding function Description
     (This : Resource_Required_Message)
      return String;

   overriding function Minimum
     (This : Resource_Required_Message)
      return Carthage.Quantities.Quantity_Type
   is (This.Minimum);

   function Is_Resource_Available_Message
     (This : Resource_Message'Class)
      return Boolean
   is (This in Resource_Available_Message'Class);

   function Is_Resource_Required_Message
     (This : Resource_Message'Class)
      return Boolean
   is (This in Resource_Required_Message'Class);

end Carthage.Messages.Resources;
