with Ada.Streams;

package Carthage.Handles.Technology is

   Technology_Version : constant Object_Version := (0, 1, 0);

   type Technology_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface
   with private;

   function Reference (Handle : Technology_Handle) return Technology_Reference;
   function Get (Reference : Technology_Reference) return Technology_Handle;
   function Empty_Handle return Technology_Handle;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Technology_Handle
     with Pre => Exists (Tag);

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

private

   type Technology_Handle is
     new Root_Carthage_Handle
     and Static_Object_Interface
     and Localised_Interface with
      record
         Reference : Technology_Reference := 0;
      end record;

   overriding function Tag
     (Handle : Technology_Handle)
      return String;

   overriding function Localisation_Tag
     (Handle : Technology_Handle)
      return String
   is ("technology-" & Technology_Handle'Class (Handle).Tag);

   overriding function Short_Name
     (Handle : Technology_Handle)
      return String
   is (Handle.Tag);

   function Create
     (Tag       : String;
      Like      : Natural;
      Cost      : Research_Points)
      return Technology_Reference;

   procedure Set_Enabled_By
     (Technology : Technology_Handle;
      Enabled_By : Technology_Handle);

   function Reference (Handle : Technology_Handle) return Technology_Reference
   is (Handle.Reference);

   function Get (Reference : Technology_Reference) return Technology_Handle
   is (Reference /= 0, Reference);

   function Empty_Handle return Technology_Handle
   is (False, 0);

end Carthage.Handles.Technology;
