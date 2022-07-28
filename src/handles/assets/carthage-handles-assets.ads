with Ada.Streams;

with Carthage.Handles.Resources;
with Carthage.Handles.Units;
with Carthage.Money;
with Carthage.Quantities;

package Carthage.Handles.Assets is

   Asset_Version : constant Object_Version := (0, 1, 0);

   subtype Coordinate is Unit_Real;

   type Asset_Experience is
     (Green, Expert, Elite);

   type Asset_Handle is
     new Root_Carthage_Handle
     and Dynamic_Object_Interface
   with private;

   function Reference
     (Handle : Asset_Handle)
      return Asset_Reference;

   function Get (Reference : Asset_Reference) return Asset_Handle;
   function Empty_Handle return Asset_Handle;

   function Owner (This : Asset_Handle) return House_Reference;

   function Has_Resource
     (This     : Asset_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Boolean
     with Post => Has_Resource'Result =
       (This.Resource_Cargo.Reference = Resource.Reference
       and then Carthage.Quantities.">"
         (This.Resource_Quantity, Carthage.Quantities.Zero));

   function Resource_Cargo
     (This : Asset_Handle)
      return Carthage.Handles.Resources.Resource_Handle;

   function Resource_Quantity
     (This : Asset_Handle)
      return Carthage.Quantities.Quantity_Type;

   procedure Set_Resource
     (This     : Asset_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle)
     with Pre => Carthage.Quantities."="
       (This.Resource_Quantity, Carthage.Quantities.Zero);

   procedure Add_Quantity
     (This     : Asset_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
     with Pre => This.Resource_Cargo.Has_Element;

   procedure Set_Quantity
     (This     : Asset_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
     with Pre => This.Resource_Cargo.Has_Element,
     Post => Carthage.Quantities."=" (This.Resource_Quantity, Quantity);

   procedure Remove_Quantity
     (This     : Asset_Handle;
      Quantity : Carthage.Quantities.Quantity_Type;
      Received : out Carthage.Quantities.Quantity_Type)
     with Pre => This.Resource_Cargo.Has_Element;

   function Unit
     (Asset : Asset_Handle)
      return Carthage.Handles.Units.Unit_Handle;

   function Stack
     (Asset : Asset_Handle)
      return Stack_Reference;

   function Is_Space_Asset
     (This : Asset_Handle)
      return Boolean
   is (Carthage.Handles.Units.Is_Space_Unit (This.Unit));

   function Is_Ground_Asset
     (This : Asset_Handle)
      return Boolean
   is (Carthage.Handles.Units.Is_Ground_Unit (This.Unit));

   function Movement
     (This : Asset_Handle)
      return Natural;

   function Spot
     (This : Asset_Handle)
      return Natural;

   function Health
     (This : Asset_Handle)
      return Health_Type;

   function Cargo_Capacity
     (This : Asset_Handle)
      return Natural;

   function Maintenance
     (This : Asset_Handle)
      return Carthage.Money.Money_Type;

   function Revenue
     (This : Asset_Handle)
      return Carthage.Money.Money_Type;

   function Alive
     (This : Asset_Handle)
      return Boolean;

   function Can_Enter
     (This : Asset_Handle;
      World : World_Reference;
      Tile  : Tile_Reference)
      return Boolean;

   function Movement_Cost
     (This  : Asset_Handle;
      World : World_Reference;
      Tile  : Tile_Reference)
      return Non_Negative_Real;

   procedure Damage
     (This   : Asset_Handle;
      Points : Positive);

   procedure Start_Jump (This : Asset_Handle);
   procedure Finish_Jump (This : Asset_Handle);

   procedure Delete_Asset (This : Asset_Handle);

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure For_All_Owned_Assets
     (Owner : House_Reference;
      Process : not null access
        procedure (Asset : Asset_Handle));

private

   type Asset_Handle is
     new Root_Carthage_Handle
     and Dynamic_Object_Interface with
      record
         Reference : Asset_Reference := 0;
      end record;

   overriding function Identifier
     (Asset : Asset_Handle)
      return Object_Identifier;

   overriding function Log_Identifier
     (Asset : Asset_Handle)
      return String;

   overriding function Short_Name
     (Asset : Asset_Handle)
      return String;

   function Reference
     (Handle : Asset_Handle)
      return Asset_Reference
   is (Handle.Reference);

   function Get (Reference : Asset_Reference) return Asset_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Asset_Handle
   is (False, 0);

   function Maintenance
     (This : Asset_Handle)
      return Carthage.Money.Money_Type
   is (This.Unit.Maintenance);

   function Revenue
     (This : Asset_Handle)
      return Carthage.Money.Money_Type
   is (This.Unit.Revenue);

   type Asset_Record is
      record
         Identifier : Object_Identifier;
         Owner      : House_Reference;
         Unit       : Unit_Reference;
         Stack      : Stack_Reference;
         Active     : Boolean;
         Health     : Health_Type;
         Loyalty    : Loyalty_Type;
         Experience : Asset_Experience;
         Resource   : Resource_Reference;
         Quantity   : Carthage.Quantities.Quantity_Type;
         Jumping    : Boolean;
         Launching  : Boolean;
         Landing    : Boolean;
      end record;

   function Create_Asset
     (Rec : Asset_Record)
      return Asset_Reference;

end Carthage.Handles.Assets;
