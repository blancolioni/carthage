with Ada.Streams;

private with WL.String_Maps;

with Carthage.Money;
with Carthage.Quantities;

package Carthage.Handles.Resources is

   Resource_Version : constant Object_Version := (0, 1, 0);

   type Resource_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface
   with private;

   function Reference (Handle : Resource_Handle) return Resource_Reference;
   function Get (Reference : Resource_Reference) return Resource_Handle;
   function Empty_Handle return Resource_Handle;

   function Base_Price
     (This : Resource_Handle)
      return Carthage.Money.Price_Type;

   function Food return Resource_Handle;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Resource_Handle
   with Pre => Exists (Tag);

   function Get_By_Index (Index : Natural) return Resource_Handle;

   type Stock_Reader_Interface is interface;

   function Quantity
     (Stock    : Stock_Reader_Interface;
      Resource : Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type
      is abstract;

   type Stock_Interface is interface and Stock_Reader_Interface;

   procedure Set_Quantity
     (Stock        : in out Stock_Interface;
      Resource     : Resource_Handle'Class;
      New_Quantity : Carthage.Quantities.Quantity_Type)
   is abstract;

   function Whole_Quantity
     (Stock    : Stock_Reader_Interface'Class;
      Resource : Resource_Handle'Class)
      return Natural;

   function Is_Empty
     (Stock : Stock_Reader_Interface'Class)
      return Boolean;

   procedure Clear_Stock
     (Stock    : in out Stock_Interface'Class);

   procedure Add
     (Stock    : in out Stock_Interface'Class;
      Resource       : Resource_Handle;
      Added_Quantity : Carthage.Quantities.Quantity_Type);

   procedure Add
     (Stock          : in out Stock_Interface'Class;
      Resource       : Resource_Handle;
      Added_Quantity : Natural);

   procedure Remove
     (Stock            : in out Stock_Interface'Class;
      Resource         : Resource_Handle;
      Removed_Quantity : Carthage.Quantities.Quantity_Type)
     with Pre =>
       Carthage.Quantities."<="
         (Removed_Quantity, Stock.Quantity (Resource));

   procedure Remove
     (Stock            : in out Stock_Interface'Class;
      Resource         : Resource_Handle;
      Removed_Quantity : Natural);

   procedure Scan_Stock
     (Stock    : Stock_Reader_Interface'Class;
      Process  : not null access
        procedure (Resource : Resource_Handle;
                   Quantity : Carthage.Quantities.Quantity_Type));

   procedure Add_Stock
     (To   : in out Stock_Interface'Class;
      From : Stock_Interface'Class);

   procedure Remove_Stock
     (From  : in out Stock_Interface'Class;
      Stock : Stock_Interface'Class);

   type Resource_Stock is new Stock_Interface with private;

   overriding function Quantity
     (Stock    : Resource_Stock;
      Resource : Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type;

   overriding procedure Set_Quantity
     (Stock        : in out Resource_Stock;
      Resource     : Resource_Handle'Class;
      New_Quantity : Carthage.Quantities.Quantity_Type);

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure For_All_Resources
     (Process : not null access
        procedure (Resource : Resource_Handle));

private

   type Resource_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface with
      record
         Reference : Resource_Reference := 0;
      end record;

   overriding function Tag
     (Handle : Resource_Handle)
      return String;

   overriding function Localisation_Tag
     (Handle : Resource_Handle)
      return String
   is ("resource-" & Resource_Handle'Class (Handle).Tag);

   overriding function Short_Name
     (Handle : Resource_Handle)
      return String
   is (Handle.Tag);

   function Reference (Handle : Resource_Handle) return Resource_Reference
   is (Handle.Reference);

   function Get (Reference : Resource_Reference) return Resource_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Resource_Handle
   is (False, 0);

   function Food return Resource_Handle
   is (Get ("food"));

   function Whole_Quantity
     (Stock    : Stock_Reader_Interface'Class;
      Resource : Resource_Handle'Class)
      return Natural
   is (Carthage.Quantities.To_Natural (Stock.Quantity (Resource)));

   package Quantity_Maps is
     new WL.String_Maps (Carthage.Quantities.Quantity_Type,
                         Carthage.Quantities."=");

   type Resource_Stock is new Stock_Interface with
      record
         Map : Quantity_Maps.Map;
      end record;

   procedure Create
     (Tag        : String;
      Base_Price : Carthage.Money.Price_Type)
     with Post => Exists (Tag)
     and then Carthage.Money."=" (Get (Tag).Base_Price, Base_Price);

end Carthage.Handles.Resources;
