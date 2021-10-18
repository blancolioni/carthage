with Ada.Streams;

with Carthage.Handles.Resources;

with Carthage.Quantities;

package Carthage.Handles.Stocks is

   Stock_Version : constant Object_Version := (0, 1, 0);

   type Stock_Handle_Interface is interface
     and Carthage.Handles.Resources.Stock_Reader_Interface;

   procedure Set_Quantity
     (This     : Stock_Handle_Interface;
      Item     : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
   is abstract
     with Post'Class => Carthage.Quantities."="
       (This.Quantity (Item), Quantity);

   procedure Add
     (This     : Stock_Handle_Interface'Class;
      Item     : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type);

   procedure Remove
     (This     : Stock_Handle_Interface'Class;
      Item     : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
     with Pre => Carthage.Quantities."<=" (Quantity, This.Quantity (Item));

   procedure Remove_Stock
     (From  : Stock_Handle_Interface'Class;
      Stock : Carthage.Handles.Resources.Stock_Reader_Interface'Class);

   type Stock_Handle is
     new Root_Carthage_Handle
     and Stock_Handle_Interface
   with private;

   overriding function Quantity
     (Stock    : Stock_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type;

   overriding procedure Set_Quantity
     (This     : Stock_Handle;
      Item     : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type);

   function Reference (Handle : Stock_Handle) return Stock_Reference;
   function Get (Reference : Stock_Reference) return Stock_Handle;
   function Empty_Handle return Stock_Handle;

   function Create_Stock
     return Stock_Reference;

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   type Has_Stock_Handle_Interface is interface;

   function Get_Stock_Handle
     (This : Has_Stock_Handle_Interface)
      return Stock_Handle'Class
      is abstract;

private

   type Stock_Handle is
     new Root_Carthage_Handle
     and Stock_Handle_Interface with
      record
         Reference : Stock_Reference := 0;
      end record;

   overriding function Short_Name
     (Handle : Stock_Handle)
      return String
   is ("stock" & Handle.Reference'Image);

   function Reference (Handle : Stock_Handle) return Stock_Reference
   is (Handle.Reference);

   function Get (Reference : Stock_Reference) return Stock_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Stock_Handle
   is (False, 0);

end Carthage.Handles.Stocks;
