private with Ada.Strings.Unbounded;
private with Carthage.Real_Images;

with Tarana;

package Carthage.Handles is

   type Unit_Category is
     (Jump, Space, Lander, Naval, Hover, Air, Foot, Tread, Wheel, Crawler);

   type Weapon_Category is
     (Water, Indirect, Air, Direct, Close, Psy,
      Ranged_Space, Direct_Space, Close_Space);

   subtype Space_Category is Unit_Category range Jump .. Lander;
   subtype Ground_Category is Unit_Category range Naval .. Crawler;

   subtype Object_Identifier is String (1 .. 8);
   type Object_Version is array (1 .. 3) of Natural;

   type Root_Carthage_Handle is
     abstract new Tarana.Signal_Source_Interface
   with private;

   function Has_Element
     (Handle : Root_Carthage_Handle'Class)
      return Boolean;

   function Short_Name
     (Handle : Root_Carthage_Handle)
      return String
      is abstract;

   function Long_Name
     (Handle : Root_Carthage_Handle)
      return String
   is (Root_Carthage_Handle'Class (Handle).Short_Name);

   function Log_Identifier
     (Handle : Root_Carthage_Handle)
      return String
   is (Root_Carthage_Handle'Class (Handle).Short_Name);

   procedure Log
     (Handle  : Root_Carthage_Handle'Class;
      Message : String);

   procedure Information
     (Handle  : Root_Carthage_Handle'Class;
      Message : String);

   type Has_Name_Interface is interface;

   function Name (Has_Name : Has_Name_Interface) return String is abstract;

   type Static_Object_Interface is interface;

   function Tag
     (Object : Static_Object_Interface)
      return String
   is abstract;

   type Dynamic_Object_Interface is interface;

   function Identifier
     (Object : Dynamic_Object_Interface)
      return Object_Identifier
      is abstract;

   type Localised_Interface is interface;

   function Localisation_Tag
     (Localised : Localised_Interface)
      return String
      is abstract;

   function Local_Text
     (Localised : Localised_Interface'Class)
      return String;

   type Asset_Reference is private;
   Null_Asset_Reference : constant Asset_Reference;

   type City_Reference is private;
   Null_City_Reference : constant City_Reference;

   type Goal_Queue_Reference is private;
   Null_Goal_Queue_Reference : constant Goal_Queue_Reference;

   type House_Reference is private;
   Null_House_Reference : constant House_Reference;

   type Manager_Reference is private;
   Null_Manager_Reference : constant Manager_Reference;

   type Planet_Reference is private;
   Null_Planet_Reference : constant Planet_Reference;

   type Resource_Reference is private;
   Null_Resource_Reference : constant Resource_Reference;

   type Stack_Reference is private;
   Null_Stack_Reference : constant Stack_Reference;

   type Stock_Reference is private;
   Null_Stock_Reference : constant Stock_Reference;

   type Structure_Reference is private;
   Null_Structure_Reference : constant Structure_Reference;

   type Technology_Reference is private;
   Null_Technology_Reference : constant Technology_Reference;

   type Terrain_Reference is private;
   Null_Terrain_Reference : constant Terrain_Reference;

   type Tile_Reference is private;
   Null_Tile_Reference : constant Tile_Reference;

   type Unit_Reference is private;
   Null_Unit_Reference : constant Unit_Reference;

   type World_Reference is private;
   Null_World_Reference : constant World_Reference;

   type Set_Of_Houses is private;

   Empty_House_Set : constant Set_Of_Houses;

   function Is_Element
     (Set   : Set_Of_Houses;
      House : House_Reference)
      return Boolean;

   procedure Clear
     (Set   : in out Set_Of_Houses);

   procedure Include
     (Set   : in out Set_Of_Houses;
      House : House_Reference);

   procedure Exclude
     (Set   : in out Set_Of_Houses;
      House : House_Reference);

private

   Current_Identifier : Object_Identifier;

   function Next_Identifier
     return Object_Identifier;

   type Root_Carthage_Handle is
     abstract new Tarana.Signal_Source_Interface with
      record
         Has_Element : Boolean := False;
      end record;

   function Has_Element (Handle : Root_Carthage_Handle'Class) return Boolean
   is (Handle.Has_Element);

   type Asset_Reference is new Natural;

   subtype Real_Asset_Reference is
     Asset_Reference range 1 .. Asset_Reference'Last;

   Null_Asset_Reference : constant Asset_Reference := 0;

   type City_Reference is new Natural;

   subtype Real_City_Reference is
     City_Reference range 1 .. City_Reference'Last;

   Null_City_Reference : constant City_Reference := 0;

   type Goal_Queue_Reference is range 0 .. 10_000;

   subtype Real_Goal_Queue_Reference is
     Goal_Queue_Reference range 1 .. Goal_Queue_Reference'Last;

   Null_Goal_Queue_Reference : constant Goal_Queue_Reference := 0;

   type House_Reference is range 0 .. 32;

   subtype Real_House_Reference is
     House_Reference range 1 .. House_Reference'Last;

   Null_House_Reference : constant House_Reference := 0;

   type Planet_Reference is range 0 .. 64;

   subtype Real_Planet_Reference is
     Planet_Reference range 1 .. Planet_Reference'Last;

   Null_Planet_Reference : constant Planet_Reference := 0;

   type Manager_Reference is range 0 .. 9_999;

   subtype Real_Manager_Reference is
     Manager_Reference range 1 .. Manager_Reference'Last;

   Null_Manager_Reference : constant Manager_Reference := 0;

   type Resource_Reference is range 0 .. 32;

   subtype Real_Resource_Reference is
     Resource_Reference range 1 .. Resource_Reference'Last;

   Null_Resource_Reference : constant Resource_Reference := 0;

   type Stack_Reference is new Natural;

   subtype Real_Stack_Reference is
     Stack_Reference range 1 .. Stack_Reference'Last;

   Null_Stack_Reference : constant Stack_Reference := 0;

   type Stock_Reference is new Natural;

   subtype Real_Stock_Reference is
     Stock_Reference range 1 .. Stock_Reference'Last;

   Null_Stock_Reference : constant Stock_Reference := 0;

   type Structure_Reference is range 0 .. 32;

   subtype Real_Structure_Reference is
     Structure_Reference range 1 .. Structure_Reference'Last;

   Null_Structure_Reference : constant Structure_Reference := 0;

   type Technology_Reference is range 0 .. 256;

   subtype Real_Technology_Reference is
     Technology_Reference range 1 .. Technology_Reference'Last;

   Null_Technology_Reference : constant Technology_Reference := 0;

   type Terrain_Reference is range 0 .. 64;

   subtype Real_Terrain_Reference is
     Terrain_Reference range 1 .. Terrain_Reference'Last;

   Null_Terrain_Reference : constant Terrain_Reference := 0;

   type Tile_Reference is new Natural;

   subtype Real_Tile_Reference is
     Tile_Reference range 1 .. Tile_Reference'Last;

   Null_Tile_Reference : constant Tile_Reference := 0;

   type Unit_Reference is range 0 .. 1_000;

   subtype Real_Unit_Reference is
     Unit_Reference range 1 .. Unit_Reference'Last;

   Null_Unit_Reference : constant Unit_Reference := 0;

   type World_Reference is range 0 .. 16;

   subtype Real_World_Reference is
     World_Reference range 1 .. World_Reference'Last;

   Null_World_Reference : constant World_Reference := 0;

   type Set_Of_Houses is array (Real_House_Reference) of Boolean
     with Pack, Size => Real_House_Reference'Last;

   Empty_House_Set : constant Set_Of_Houses := (others => False);

   function Is_Element
     (Set   : Set_Of_Houses;
      House : House_Reference)
      return Boolean
   is (Set (House));

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   function Image (X : Real) return String
                   renames Carthage.Real_Images.Approximate_Image;

end Carthage.Handles;
