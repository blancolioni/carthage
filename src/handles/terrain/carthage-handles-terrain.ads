with Ada.Streams;

with Carthage.Colors;

package Carthage.Handles.Terrain is

   Terrain_Version : constant Object_Version := (0, 1, 0);

   type Terrain_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface
   with private;

   function Reference (Handle : Terrain_Handle) return Terrain_Reference;
   function Get (Reference : Terrain_Reference) return Terrain_Handle;
   function Empty_Handle return Terrain_Handle;

   function Is_Terrain_Tag (Tag : String) return Boolean;
   function Get (Tag : String) return Terrain_Handle
     with Pre => Is_Terrain_Tag (Tag);

   function Is_Ocean (This : Terrain_Handle) return Boolean;
   function Is_Water (This : Terrain_Handle) return Boolean;

   function Color
     (This     : Terrain_Handle;
      Category : String)
      return Carthage.Colors.Color_Type;

   function Road_Cost
     (This     : Terrain_Handle;
      Category : String)
      return Natural;

   procedure For_All_Terrain
     (Process : not null access
        procedure (Terrain : Terrain_Handle));

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

private

   type Terrain_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface with
      record
         Reference : Terrain_Reference := 0;
      end record;

   overriding function Tag
     (Handle : Terrain_Handle)
      return String;

   overriding function Localisation_Tag
     (Handle : Terrain_Handle)
      return String
   is ("terrain-" & Terrain_Handle'Class (Handle).Tag);

   overriding function Short_Name
     (Handle : Terrain_Handle)
      return String
   is (Handle.Tag);

   function Reference (Handle : Terrain_Handle) return Terrain_Reference
   is (Handle.Reference);

   function Get (Reference : Terrain_Reference) return Terrain_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Terrain_Handle
   is (False, 0);

   procedure Create
     (Tag        : String;
      Ocean      : Boolean;
      Water      : Boolean;
      Configure_Category : not null access
        procedure (Category : String;
                   Color    : out Carthage.Colors.Color_Type;
                   Road_Cost : out Natural));

end Carthage.Handles.Terrain;
