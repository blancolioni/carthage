with Ada.Streams;

with Carthage.Handles.Assets;
with Carthage.Handles.Houses;
with Carthage.Handles.Resources;
with Carthage.Handles.Stocks;
with Carthage.Handles.Tiles;

with Carthage.Calendar;
with Carthage.Quantities;

package Carthage.Handles.Stacks is

   Maximum_Stack_Size : constant := 40;

   type Stack_Asset_Count is range 0 .. Maximum_Stack_Size;
   subtype Stack_Asset_Index is
     Stack_Asset_Count range 1 .. Stack_Asset_Count'Last;

   type Stack_Property is
     (Defender);
   --  defender: avoid moving and attacking with this stack

   type Stack_Manager_Interface is interface;

   procedure Take_Resource
     (From     : in out Stack_Manager_Interface;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : in out Carthage.Quantities.Quantity_Type)
   is abstract;

   Stack_Version : constant Object_Version := (0, 1, 0);

   subtype Coordinate is Unit_Real;

   type Stack_Handle is
     new Root_Carthage_Handle
     and Dynamic_Object_Interface
     and Carthage.Handles.Stocks.Stock_Handle_Interface
   with private;

   overriding function Identifier
     (This : Stack_Handle)
      return Object_Identifier;

   overriding function Quantity
     (This     : Stack_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type;

   overriding procedure Add
     (This           : Stack_Handle;
      Resource       : Carthage.Handles.Resources.Resource_Handle'Class;
      Added_Quantity : Carthage.Quantities.Quantity_Type);

   overriding procedure Take
     (This     : Stack_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class;
      Quantity : Carthage.Quantities.Quantity_Type;
      Received : out Carthage.Quantities.Quantity_Type);

   function Reference
     (Handle : Stack_Handle)
      return Stack_Reference;

   function Get (Reference : Stack_Reference) return Stack_Handle;
   function Empty_Handle return Stack_Handle;

   function Owner
     (This : Stack_Handle)
      return Carthage.Handles.Houses.House_Handle;

   function Has_Assets (This : Stack_Handle) return Boolean;
   function Is_Full (This : Stack_Handle) return Boolean;
   function Is_Orbital (This : Stack_Handle) return Boolean;
   function Is_Ground (This : Stack_Handle) return Boolean;

   function Description
     (This : Stack_Handle)
      return String;

   function Planet (This : Stack_Handle) return Planet_Reference;

   function Has_Property
     (This : Stack_Handle;
      Property : Stack_Property)
      return Boolean;

   procedure Set_Property
     (This : in out Stack_Handle;
      Property : Stack_Property);

   procedure Clear_Property
     (This : in out Stack_Handle;
      Property : Stack_Property);

   procedure On_Hostile_Spotted
     (Manager : in out Stack_Manager_Interface;
      Stack   : Stack_Handle'Class;
      Hostile : Stack_Handle'Class;
      Stop    : out Boolean)
   is abstract;

   procedure On_Movement_Ended
     (Manager : in out Stack_Manager_Interface;
      Stack   : Stack_Handle'Class)
   is null;

   procedure On_Stack_Removed
     (Manager : in out Stack_Manager_Interface;
      Stack   : Stack_Handle'Class)
   is null;

   procedure On_Asset_Added
     (Manager : in out Stack_Manager_Interface;
      Stack   : Stack_Handle'Class;
      Asset   : Carthage.Handles.Assets.Asset_Handle)
   is null;

   procedure On_Asset_Removed
     (Manager : in out Stack_Manager_Interface;
      Stack   : Stack_Handle'Class;
      Asset   : Carthage.Handles.Assets.Asset_Handle)
   is null;

   function Movement
     (This : Stack_Handle)
      return Natural
     with Pre => This.Has_Assets;

   function Has_Movement
     (This : Stack_Handle)
      return Boolean;

   function Current_Tile
     (This : Stack_Handle)
      return Carthage.Handles.Tiles.Tile_Handle;

   function Current_Position
     (This : Stack_Handle)
     return Tile_Position;

   function Next_Tile
     (This : Stack_Handle)
      return Carthage.Handles.Tiles.Tile_Handle
     with Pre => Has_Movement (This);

   function Next_Position
     (This : Stack_Handle)
      return Tile_Position
     with Pre => Has_Movement (This);

   function Current_Movement
     (This : Stack_Handle)
      return Array_Of_Positions
     with Pre => Has_Movement (This);

   function Spot
     (This : Stack_Handle)
      return Natural
     with Pre => This.Has_Assets;

   function Can_Enter
     (This : Stack_Handle;
      Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Boolean
     with Pre => This.Has_Assets;

   function Movement_Cost
     (This : Stack_Handle;
      Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Non_Negative_Real
     with Pre => This.Has_Assets;

   function Movement_Duration
     (This : Stack_Handle;
      Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Duration
     with Pre => This.Has_Assets and then Can_Enter (This, Tile);

   function Movement_Progress
     (This : Stack_Handle)
      return Unit_Real;

   function Find_Path
     (This    : Stack_Handle;
      Tile    : Carthage.Handles.Tiles.Tile_Handle)
      return Array_Of_Positions;

   function Total
     (This : Stack_Handle;
      Value : not null access
        function (Asset : Carthage.Handles.Assets.Asset_Handle) return Integer)
      return Integer;

   function Total_Strength
     (This : Stack_Handle)
      return Natural;

   function Asset_Count (This : Stack_Handle) return Stack_Asset_Count;
   function Asset (This : Stack_Handle;
                   Index : Stack_Asset_Index)
                   return Asset_Reference
     with Pre => Index <= Asset_Count (This);

   procedure Add_Asset
     (This  : Stack_Handle;
      Asset : Asset_Reference);

   procedure Remove_Asset
     (This  : Stack_Handle;
      Asset : Asset_Reference);

   procedure Move_To_Tile
     (This  : Stack_Handle;
      Tile  : Carthage.Handles.Tiles.Tile_Handle)
     with Pre => Can_Enter (This, Tile);

   procedure Remove_Dead_Assets
     (This : Stack_Handle);

   procedure Set_Manager
     (This : Stack_Handle;
      Manager : not null access Stack_Manager_Interface'Class)
   is null;

   procedure Delete_Stack (Stack : Stack_Handle);

   procedure For_All_Stacks
     (Process : not null access procedure (Stack : Stack_Handle));

   procedure For_All_Ground_Stacks
     (Process : not null access procedure (Stack : Stack_Handle));

   procedure Remove_Empty_Ground_Stacks;

   type Asset_Meta_Manager_Interface is interface;

   procedure On_Hostile_Spotted
     (Manager : in out Asset_Meta_Manager_Interface;
      Spotter : Stack_Handle'Class;
      Hostile : Stack_Handle'Class)
   is abstract;

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

private

   type Stack_Handle is
     new Root_Carthage_Handle
     and Dynamic_Object_Interface
     and Carthage.Handles.Stocks.Stock_Handle_Interface with
      record
         Reference : Stack_Reference := 0;
      end record;

   overriding function Short_Name
     (Stack : Stack_Handle)
      return String;

   procedure Execute_Movement_Step
     (Stack : Stack_Handle'Class)
     with Pre => Stack.Has_Movement;

   procedure Start_Next_Movement
     (Stack : Stack_Handle'Class;
      Clock : Carthage.Calendar.Time)
     with Pre => Stack.Has_Movement;

   procedure Clear_Movement
     (Stack : Stack_Handle'Class);

   procedure Set_Movement_Path
     (Stack : Stack_Handle'Class;
      Path  : Array_Of_Positions);

   function Reference
     (Handle : Stack_Handle)
      return Stack_Reference
   is (Handle.Reference);

   function Get (Reference : Stack_Reference) return Stack_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Stack_Handle
   is (False, 0);

   function Create_Stack
     (Owner     : House_Reference;
      Planet    : Planet_Reference;
      Tile      : Tile_Reference)
      return Stack_Reference;

   type Stack_Order_Type is (Move_To_Tile);

   type Stack_Order_Record (Order_Type : Stack_Order_Type) is
      record
         case Order_Type is
            when Move_To_Tile =>
               Destination : Tile_Reference;
         end case;
      end record;

   function First_Order
     (Stack : Stack_Handle'Class)
      return Stack_Order_Record;

   procedure Delete_First_Order
     (Stack : Stack_Handle'Class);

end Carthage.Handles.Stacks;
