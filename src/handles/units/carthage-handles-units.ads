with Ada.Streams;

with Carthage.Money;
with Carthage.Quantities;

package Carthage.Handles.Units is

   Unit_Version : constant Object_Version := (0, 1, 0);

   type Unit_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface
   with private;

   function Reference (Handle : Unit_Handle) return Unit_Reference;
   function Get (Reference : Unit_Reference) return Unit_Handle;
   function Empty_Handle return Unit_Handle;

   function Resource_Name
     (This : Unit_Handle)
      return String;

   function Can_Target
     (Weapon : Weapon_Category;
      Unit   : Unit_Category)
      return Boolean;

   function Category
     (This : Unit_Handle)
      return Unit_Category;

   function Is_Space_Unit
     (This : Unit_Handle)
      return Boolean
   is (This.Category in Space_Category);

   function Is_Ground_Unit
     (This : Unit_Handle)
      return Boolean
   is (This.Category in Ground_Category);

   function Movement
     (This : Unit_Handle)
      return Natural;

   function Spot
     (This : Unit_Handle)
      return Natural;

   function Eat
     (This : Unit_Handle)
      return Carthage.Quantities.Quantity_Type;

   function Maintenance
     (This : Unit_Handle)
      return Carthage.Money.Money_Type;

   function Revenue
     (This : Unit_Handle)
      return Carthage.Money.Money_Type;

   function Agility
     (This : Unit_Handle)
      return Natural;

   function Armour
     (This : Unit_Handle)
      return Natural;

   function Psychic_Defense
     (This : Unit_Handle)
      return Natural;

   function Rank
     (This : Unit_Handle)
      return Natural;

   function Cargo_Capacity
     (This : Unit_Handle)
      return Natural;

   function Has_Attack
     (This   : Unit_Handle;
      Weapon : Weapon_Category)
      return Boolean;

   function Accuracy
     (This   : Unit_Handle;
      Weapon : Weapon_Category)
      return Positive
     with Pre => Has_Attack (This, Weapon);

   function Strength
     (This   : Unit_Handle;
      Weapon : Weapon_Category)
      return Positive
     with Pre => Has_Attack (This, Weapon);

   function Exists (Tag : String) return Boolean;

   function Get (Tag : String) return Unit_Handle
     with Pre => Exists (Tag);

   function Is_Valid_Index (Index : Positive) return Boolean;

   function Get_By_Index (Index : Positive) return Unit_Handle
     with Pre => Is_Valid_Index (Index),
     Post => Get_By_Index'Result.Has_Element;

   function Cargo_Pod return Unit_Handle;

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

private

   Cargo_Pod_Index : Natural := 0;

   type Unit_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface with
      record
         Reference : Unit_Reference := 0;
      end record;

   overriding function Tag
     (Handle : Unit_Handle)
      return String;

   overriding function Localisation_Tag
     (Handle : Unit_Handle)
      return String
   is ("unit-" & Unit_Handle'Class (Handle).Tag);

   overriding function Short_Name
     (Handle : Unit_Handle)
      return String
   is (Handle.Tag);

   function Reference (Handle : Unit_Handle) return Unit_Reference
   is (Handle.Reference);

   function Get (Reference : Unit_Reference) return Unit_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Unit_Handle
   is (False, 0);

   Cargo_Pod_Handle : Unit_Handle;

   type Weapon_Target_Array is
     array (Weapon_Category, Unit_Category) of Boolean;

   Weapon_Targets   : Weapon_Target_Array := (others => (others => False));

   type Weapon_Record is
      record
         Accuracy : Natural := 0;
         Strength : Natural := 0;
      end record;

   type Weapon_Array is array (Weapon_Category) of Weapon_Record;

   function Can_Target
     (Weapon : Weapon_Category;
      Unit   : Unit_Category)
      return Boolean
   is (Weapon_Targets (Weapon, Unit));

   type Unit_Record is
      record
         Tag            : Ada.Strings.Unbounded.Unbounded_String;
         Index          : Positive;
         Category       : Unit_Category;
         Is_Sceptre     : Boolean;
         Is_Noble       : Boolean;
         Move           : Natural;
         Spot           : Natural;
         Camouflage     : Natural;
         Agility        : Natural;
         Armour         : Natural;
         Psy_Defence    : Natural;
         Cargo          : Natural;
         Can_Be_Cargo   : Boolean;
         Combat         : Boolean;
         Maintenance    : Carthage.Money.Money_Type;
         Revenue        : Carthage.Money.Money_Type;
         Credit_Cost    : Natural;
         Eat            : Carthage.Quantities.Quantity_Type;
         Rank           : Natural;
         Build_Unit     : Unit_Reference;
         Turns_To_Build : Natural;
         Image_Resource : Ada.Strings.Unbounded.Unbounded_String;
         Weapons        : Weapon_Array;
      end record;

   procedure Create (Rec : Unit_Record);

end Carthage.Handles.Units;
