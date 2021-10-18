private with Ada.Containers.Doubly_Linked_Lists;
private with WL.Localisation;

with Ada.Streams;

with Carthage.Colors;
with Carthage.Money;

package Carthage.Handles.Houses is

   House_Version : constant Object_Version := (0, 1, 0);

   type House_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface
   with private;

   function Reference
     (Handle : House_Handle)
      return House_Reference;

   function Get (Reference : House_Reference) return House_Handle;

   function Empty_Handle return House_Handle;

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   overriding function Tag
     (This : House_Handle)
      return String;

   overriding function Short_Name
     (This : House_Handle)
      return String;

   overriding function Long_Name
     (This : House_Handle)
      return String;

   type Treaty_Status is (Allied, Neutral, Hostile, War);

   type House_Category is
     (Noble, Church, League, Imperial, Vau, Symbiot, Rebels);

   function Treaty_Status_With
     (This  : House_Handle;
      Other : House_Handle)
      return Treaty_Status;

   function At_War_With
     (This  : House_Handle;
      Other : House_Handle)
      return Boolean;

   function Capital
     (This : House_Handle)
      return Planet_Reference;

   function Category
     (This : House_Handle)
      return House_Category;

   function Color
     (This : House_Handle)
      return Carthage.Colors.Color_Type;

   procedure Scan_Known_Planets
     (This    : House_Handle;
      Process : not null access
        procedure (Planet : Planet_Reference));

   function Cash
     (This : House_Handle)
      return Carthage.Money.Money_Type;

   function Debt
     (This : House_Handle)
      return Carthage.Money.Money_Type;

   procedure Spend
     (This   : House_Handle;
      Amount : Carthage.Money.Money_Type);

   procedure Earn
     (This   : House_Handle;
      Amount : Carthage.Money.Money_Type);

   function Unit_Pay
     (This : House_Handle)
      return Unit_Real;

   procedure For_All_Houses
     (Process : not null access
        procedure (House : House_Handle));

   procedure Log_Status
     (This : House_Handle);

   function Is_Element_Of
     (House : House_Handle;
      Set   : Set_Of_Houses)
      return Boolean;

   procedure Include
     (House : House_Handle;
      Set   : in out Set_Of_Houses);

   procedure Exclude
     (House : House_Handle;
      Set   : in out Set_Of_Houses);

   type House_Manager_Interface is interface;

   procedure Set_House_Manager
     (This    : House_Handle;
      Manager : not null access House_Manager_Interface'Class);

   function Get (Tag : String) return House_Handle;

private

   type House_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface with
      record
         Reference : House_Reference := 0;
      end record;

   overriding function Localisation_Tag
     (This : House_Handle)
      return String
   is ("house-" & House_Handle'Class (This).Tag);

   function Reference
     (Handle : House_Handle)
      return House_Reference
   is (Handle.Reference);

   function Get (Reference : House_Reference) return House_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return House_Handle
   is (False, 0);

   overriding function Short_Name
     (This : House_Handle)
      return String
   is (WL.Localisation.Local_Text (This.Tag));

   overriding function Long_Name
     (This : House_Handle)
      return String
   is (WL.Localisation.Local_Text (This.Tag & "-full"));

   function Is_Element_Of
     (House : House_Handle;
      Set   : Set_Of_Houses)
      return Boolean
   is (Is_Element (Set, House.Reference));

   type House_Treaties is array (Real_House_Reference) of Treaty_Status;

   package Planet_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Planet_Reference);

   type House_Record is
      record
         Tag           : Ada.Strings.Unbounded.Unbounded_String;
         Category      : House_Category;
         Capital       : Planet_Reference;
         Tax_Rate      : Unit_Real;
         Tithe_Skim    : Unit_Real;
         Unit_Pay      : Unit_Real;
         Cash          : Carthage.Money.Money_Type;
         Debt          : Carthage.Money.Money_Type;
         Color         : Carthage.Colors.Color_Type;
         Treaties      : House_Treaties := (others => Neutral);
         Known_Planets : Planet_Lists.List;
      end record;

   function Create (Rec : House_Record) return House_Reference;

   procedure Set_Initial_Capital
     (Handle  : House_Handle'Class;
      Capital : Planet_Reference)
     with Pre => Handle.Capital = Null_Planet_Reference,
     Post => Handle.Capital = Capital;

end Carthage.Handles.Houses;
