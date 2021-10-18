with Ada.Streams;

package Carthage.Handles.Worlds is

   World_Version : constant Object_Version := (0, 1, 0);

   type World_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface
   with private;

   function Reference (Handle : World_Handle) return World_Reference;
   function Get (Reference : World_Reference) return World_Handle;
   function Empty_Handle return World_Handle;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return World_Handle
     with Pre => Exists (Tag);

   function Movement_Multiplier
     (World   : World_Handle;
      Terrain : Terrain_Reference;
      Unit    : Unit_Category)
      return Non_Negative_Real;

   function Road_Multiplier
     (World   : World_Handle;
      Unit    : Unit_Category)
      return Non_Negative_Real;

   procedure Scan_Worlds
     (Process : not null access
        procedure (World : World_Handle));

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

private

   type World_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface with
      record
         Reference : World_Reference := 0;
      end record;

   overriding function Tag
     (Handle : World_Handle)
      return String;

   overriding function Localisation_Tag
     (Handle : World_Handle)
      return String
   is ("world-" & World_Handle'Class (Handle).Tag);

   overriding function Short_Name
     (Handle : World_Handle)
      return String
   is (Handle.Tag);

   function Reference (Handle : World_Handle) return World_Reference
   is (Handle.Reference);

   function Get (Reference : World_Reference) return World_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return World_Handle
   is (False, 0);

   procedure Create_World (Tag : String);

   procedure Set_Terrain_Movement
     (World      : World_Handle'Class;
      Unit       : Unit_Category;
      Terrain    : Terrain_Reference;
      Multiplier : Real);

   procedure Set_Road_Movement
     (World      : World_Handle'Class;
      Unit       : Unit_Category;
      Multiplier : Real);

end Carthage.Handles.Worlds;
