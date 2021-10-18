with Ada.Containers.Vectors;
with WL.String_Maps;

package body Carthage.Handles.Technology is

   type Technology_Record is
      record
         Tag        : Ada.Strings.Unbounded.Unbounded_String;
         Top_Level  : Boolean;
         Enabled_By : Technology_Reference;
         Cost       : Research_Points;
         Like       : Natural;
      end record;

   package Technology_Vectors is
     new Ada.Containers.Vectors (Real_Technology_Reference, Technology_Record);

   package Technology_Maps is
     new WL.String_Maps (Technology_Reference);

   Technology_Vector : Technology_Vectors.Vector;
   Technology_Map    : Technology_Maps.Map;

   function Get
     (Handle : Technology_Handle)
      return Technology_Vectors.Constant_Reference_Type
   is (Technology_Vector (Handle.Reference));

   function Exists (Tag : String) return Boolean
   is (Technology_Map.Contains (Tag));

   function Get (Tag : String) return Technology_Handle
   is (Get (Technology_Map (Tag)));

   overriding function Tag
     (Handle : Technology_Handle)
      return String
   is (-Get (Handle).Tag);

   ------------
   -- Create --
   ------------

   function Create
     (Tag       : String;
      Like      : Natural;
      Cost      : Research_Points)
      return Technology_Reference
   is
   begin
      Technology_Vector.Append
        (Technology_Record'
           (Tag        => +Tag,
            Top_Level  => True,
            Enabled_By => Null_Technology_Reference,
            Cost       => Cost,
            Like       => Like));
      Technology_Map.Insert (Tag, Technology_Vector.Last_Index);
      return Technology_Vector.Last_Index;
   end Create;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Technology_Vectors.Vector'Read (Stream, Technology_Vector);
      for Reference in 1 .. Technology_Vector.Last_Index loop
         Technology_Map.Insert (Get (Reference).Tag, Reference);
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Technology_Vectors.Vector'Write (Stream, Technology_Vector);
   end Save;

   --------------------
   -- Set_Enabled_By --
   --------------------

   procedure Set_Enabled_By
     (Technology : Technology_Handle;
      Enabled_By : Technology_Handle)
   is
   begin
      Technology_Vector (Technology.Reference).Enabled_By :=
        Enabled_By.Reference;
      Technology_Vector (Technology.Reference).Top_Level := False;
   end Set_Enabled_By;

end Carthage.Handles.Technology;
