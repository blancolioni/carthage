private with Ada.Containers.Vectors;

with Ada.Streams;

with Carthage.Handles.Houses;
with Carthage.Handles.Resources;
with Carthage.Handles.Terrain;

with Carthage.Quantities;

package Carthage.Handles.Tiles is

   Tile_Version : constant Object_Version := (0, 1, 0);

   type Tile_Handle is
     new Root_Carthage_Handle
     and Dynamic_Object_Interface
   with private;

   function Reference (Handle : Tile_Handle) return Tile_Reference;
   function Get (Reference : Tile_Reference) return Tile_Handle;
   function Empty_Handle return Tile_Handle;

   function Planet (Handle : Tile_Handle) return Planet_Reference;

   function Resource_Quantity
     (Tile     : Tile_Handle;
      House    : Carthage.Handles.Houses.House_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type;

   procedure Add_Resource
     (Tile     : Tile_Handle;
      Owner    : Carthage.Handles.Houses.House_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type);

   procedure Take_Resource
     (Tile     : Tile_Handle;
      Owner    : Carthage.Handles.Houses.House_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type;
      Received : out Carthage.Quantities.Quantity_Type);

   subtype Terrain_Layer is Positive range 1 .. 3;

   type Terrain_Layer_Array is
     array (Terrain_Layer) of Carthage.Handles.Terrain.Terrain_Handle;

   function Position_Image (Position : Tile_Position) return String;

   function Position
     (This : Tile_Handle)
      return Tile_Position;

   function Terrain
     (This : Tile_Handle;
      Layer : Terrain_Layer)
      return Carthage.Handles.Terrain.Terrain_Handle;

   function Terrain_Layers
     (This : Tile_Handle)
      return Terrain_Layer_Array;

   function Base_Terrain
     (This : Tile_Handle)
      return Carthage.Handles.Terrain.Terrain_Handle;

   function Has_Terrain
     (This    : Tile_Handle;
      Terrain : Carthage.Handles.Terrain.Terrain_Handle)
      return Boolean;

   function Seen_By
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
      return Boolean;

   function Explored_By
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
      return Boolean;

   function Currently_Visible_To
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
      return Boolean;

   function Is_Water (This : Tile_Handle) return Boolean;

   function Has_City (This : Tile_Handle) return Boolean;
   function Has_Road (This : Tile_Handle) return Boolean;
   function Has_Stacks (This : Tile_Handle) return Boolean;

   function Get_City
     (This : Tile_Handle)
      return City_Reference
     with Pre => Has_City (This);

   procedure Set_City
     (This : Tile_Handle;
      City : City_Reference)
     with Pre => not Has_City (This),
     Post => Has_City (This)
     and then Get_City (This) = City;

   function First_Stack
     (This : Tile_Handle)
      return Stack_Reference
     with Pre => Has_Stacks (This);

   procedure Add_Stack
     (This : Tile_Handle;
      Stack : Stack_Reference)
     with Pre => not This.Has_Stack (Stack),
       Post => This.Has_Stack (Stack);

   procedure Remove_Stack
     (This  : Tile_Handle;
      Stack : Stack_Reference)
     with Pre => This.Has_Stack (Stack),
     Post => not This.Has_Stack (Stack);

   procedure Scan_Stacks
     (This : Tile_Handle;
     Process       : not null access
        procedure (Stack : Stack_Reference);
      Skip_Empty    : Boolean := True);

   function Description
     (This : Tile_Handle)
      return String;

   function Has_Stack
     (This  : Tile_Handle;
      Stack : Stack_Reference)
      return Boolean;

   function Has_Stack
     (This  : Tile_Handle;
      Match : not null access
        function (Stack : Stack_Reference)
      return Boolean)
      return Boolean;

   function Find_Stack
     (This  : Tile_Handle;
      Match : not null access
        function (Stack : Stack_Reference)
      return Boolean)
      return Stack_Reference;

   procedure Set_Road
     (This : Tile_Handle;
      Road : Boolean);

   procedure Set_Seen_By
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
     with Post => Seen_By (This, House);

   procedure Set_Explored_By
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
     with Post => Explored_By (This, House);

   procedure Set_Currently_Visible_To
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
     with Post => Currently_Visible_To (This, House);

   procedure Clear_Visibility
     (This  : Tile_Handle);

   type Array_Of_Tiles is array (Positive range <>) of Tile_Handle;

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

private

   type Set_Of_Terrain is array (Real_Terrain_Reference) of Boolean
     with Pack, Size => Real_Terrain_Reference'Last;

   type Tile_Record is
      record
         Identifier  : Object_Identifier;
         Planet      : Planet_Reference;
         Position    : Tile_Position;
         River       : Boolean;
         Terrain     : Terrain_Layer_Array;
         Has_Terrain : Set_Of_Terrain;
      end record;

   package Tile_Vectors is
     new Ada.Containers.Vectors (Real_Tile_Reference, Tile_Record);

   Tile_Vector : Tile_Vectors.Vector;

   function Get
     (Handle : Tile_Handle)
      return Tile_Vectors.Constant_Reference_Type
   is (Tile_Vector (Handle.Reference));

   type Tile_Handle is
     new Root_Carthage_Handle
     and Dynamic_Object_Interface with
      record
         Reference : Tile_Reference := 0;
      end record;

   overriding function Identifier
     (Handle : Tile_Handle)
      return Object_Identifier;

   overriding function Short_Name
     (Handle : Tile_Handle)
      return String;

   function Reference (Handle : Tile_Handle) return Tile_Reference
   is (Handle.Reference);

   function Get (Reference : Tile_Reference) return Tile_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Tile_Handle
   is (False, 0);

   function Planet (Handle : Tile_Handle) return Planet_Reference
   is (Get (Handle).Planet);

   function Position
     (This : Tile_Handle)
      return Tile_Position
   is (Get (This).Position);

   function Has_Terrain
     (This    : Tile_Handle;
      Terrain : Carthage.Handles.Terrain.Terrain_Handle)
      return Boolean
   is (Get (This).Has_Terrain (Terrain.Reference));

   function Create_Tile
     (Planet   : Planet_Reference;
      Position : Tile_Position;
      River    : Boolean;
      Road     : Boolean;
      Terrain  : Terrain_Layer_Array)
      return Tile_Reference;

end Carthage.Handles.Tiles;
