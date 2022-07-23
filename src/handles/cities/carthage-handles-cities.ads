private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Doubly_Linked_Lists;

with Ada.Streams;

with Carthage.Handles.Houses;
with Carthage.Handles.Resources;
with Carthage.Handles.Stocks;

with Carthage.Money;
with Carthage.Quantities;

package Carthage.Handles.Cities is

   City_Version : constant Object_Version := (0, 1, 0);

   subtype Coordinate is Unit_Real;

   type City_Handle is
     new Root_Carthage_Handle
     and Dynamic_Object_Interface
     and Carthage.Handles.Stocks.Stock_Handle_Interface
   with private;

   overriding function Quantity
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type;

   overriding procedure Set_Quantity
     (This     : City_Handle;
      Item     : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type);

   function Reference
     (Handle : City_Handle)
      return City_Reference;

   function Get (Reference : City_Reference) return City_Handle;
   function Empty_Handle return City_Handle;

   function Planet
     (City : City_Handle)
      return Planet_Reference;

   function Owner
     (City : City_Handle)
      return Carthage.Handles.Houses.House_Handle;

   function Structure
     (City : City_Handle)
      return Structure_Reference;

   function Agora
     (City : City_Handle)
      return City_Handle;

   function Loyalty
     (City : City_Handle)
      return Loyalty_Type;

   function Health
     (City : City_Handle)
      return Health_Type;

   function Tile
     (City : City_Handle)
      return Tile_Reference;

   function Description (City : City_Handle) return String;

   type City_Manager_Interface is interface;

   procedure On_Resource_Arrival
     (Manager  : in out City_Manager_Interface;
      This     : City_Handle'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Positive)
   is null;

   procedure Set_Manager
     (This : City_Handle;
      Manager : not null access City_Manager_Interface'Class);

   function Is_Agora
     (This : City_Handle)
      return Boolean;

   function Agora_Buys_For
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Carthage.Money.Price_Type
     with Pre => Is_Agora (This);

   function Agora_Sells_For
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Carthage.Money.Price_Type
     with Pre => Is_Agora (This);

   procedure After_Agora_Buys
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
     with Pre => Is_Agora (This);

   procedure After_Agora_Sells
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
     with Pre => Is_Agora (This);

   function Seen_By
     (This  : City_Handle;
      House : Carthage.Handles.Houses.House_Handle)
      return Boolean;

   procedure Buy_Resource
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Positive);

   procedure Sell_Resource
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Positive);

   procedure Set_Agora
     (This     : City_Handle;
      Agora    : City_Handle);

   procedure Set_Seen_By
     (This  : City_Handle;
      House : Carthage.Handles.Houses.House_Handle);

   procedure Look
     (This : City_Handle);

   procedure Execute_Production
     (This       : City_Handle;
      Efficiency : Unit_Real;
      Factor     : Unit_Real);

   procedure For_All_Cities
     (Process : not null access procedure (This : City_Handle));

   procedure For_All_Cities_With_Structure
     (Structure : Structure_Reference;
      Process   : not null access procedure (This : City_Handle));

   procedure For_All_Producers
     (Resource  : Carthage.Handles.Resources.Resource_Handle;
      Process   : not null access procedure (This : City_Handle));

   procedure For_All_Harvesters
     (Process : not null access procedure (This : City_Handle));

   --  procedure Scan_Cities
   --    (Owner   : Carthage.Handles.Houses.House_Handle;
   --     Process : not null access procedure (City : City_Class));
   --
   --  procedure Scan_Cities
   --  (Test    : not null access function (City : City_Class) return Boolean;
   --     Process : not null access procedure (City : City_Class));

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

private

   type City_Handle is
     new Root_Carthage_Handle
     and Dynamic_Object_Interface
     and Carthage.Handles.Stocks.Stock_Handle_Interface with
      record
         Reference : City_Reference := 0;
      end record;

   overriding function Identifier
     (City : City_Handle)
      return Object_Identifier;

   overriding function Short_Name
     (City : City_Handle)
      return String;

   procedure Scan_Harvest_Tiles
     (This      : City_Handle;
      Process   : not null access
        procedure (Tile : Tile_Reference));

   function Reference
     (Handle : City_Handle)
      return City_Reference
   is (Handle.Reference);

   function Get (Reference : City_Reference) return City_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return City_Handle
   is (False, 0);

   type City_Order_Class is (Buy, Sell, Transfer);

   type City_Order_Record (Class : City_Order_Class) is
      record
         case Class is
            when Buy | Sell | Transfer =>
               Other_City : City_Reference;
               Resource   : Resource_Reference;
               Quantity   : Positive;
         end case;
      end record;

   package City_Order_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (City_Order_Record);

   type Agora_Resource_Record is
      record
         Buy_Price  : Carthage.Money.Price_Type := Carthage.Money.Zero;
         Sell_Price : Carthage.Money.Price_Type := Carthage.Money.Zero;
      end record;

   type Agora_Price_Array is
     array (Real_Resource_Reference) of Agora_Resource_Record;

   package Harvest_Tile_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Tile_Reference);

   type City_Record is
      record
         Identifier    : Object_Identifier;
         Planet        : Planet_Reference;
         Tile          : Tile_Reference;
         Structure     : Structure_Reference;
         Owner         : House_Reference;
         Health        : Health_Type;
         Loyalty       : Loyalty_Type;
         Agora         : City_Reference;
         Progress      : Unit_Real;
         Orders        : City_Order_Lists.List;
         Seen_By       : Set_Of_Houses;
         Prices        : Agora_Price_Array;
         Transfer      : Carthage.Handles.Resources.Resource_Stock;
         Harvest_Tiles : Harvest_Tile_Lists.List;
      end record;

   function Create_City (Rec : City_Record) return City_Reference;

   procedure Process_Orders
     (City : City_Handle'Class;
      Process : not null access
        procedure (Order : City_Order_Record));

end Carthage.Handles.Cities;
