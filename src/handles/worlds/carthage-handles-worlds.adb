with Ada.Containers.Vectors;
with WL.String_Maps;

package body Carthage.Handles.Worlds is

   --  movement and road multiplier arrays moved outside world_record
   --  due to gnat bug involving vectors, records, and arrays

   type Movement_Multiplier_Array is
     array (World_Reference, Terrain_Reference, Unit_Category) of Real;

   Default_Movement_Multiplier : constant Movement_Multiplier_Array :=
                                   (others => (others => (others => 1.0)));

   Current_Movement_Multiplier : Movement_Multiplier_Array :=
                                   Default_Movement_Multiplier;

   type Road_Multiplier_Array is
     array (World_Reference, Unit_Category) of Real;

   Default_Road_Multiplier : constant Road_Multiplier_Array :=
                               (others => (others => 1.0));

   Current_Road_Multiplier : Road_Multiplier_Array :=
                               Default_Road_Multiplier;

   type World_Record is
      record
         Tag      : Ada.Strings.Unbounded.Unbounded_String;
         --  Movement : Movement_Multiplier_Array;
         --  := (others => (others => 1.0));
         --       Road     : Road_Multiplier_Array;
         --  := (others => 1.0);
      end record;

   package World_Vectors is
     new Ada.Containers.Vectors (Real_World_Reference, World_Record);

   package World_Maps is
     new WL.String_Maps (World_Reference);

   World_Vector : World_Vectors.Vector;
   World_Map    : World_Maps.Map;

   function Get
     (Handle : World_Handle)
      return World_Vectors.Constant_Reference_Type
   is (World_Vector (Handle.Reference));

   function Exists (Tag : String) return Boolean
   is (World_Map.Contains (Tag));

   function Get (Tag : String) return World_Handle
   is (Get (World_Map.Element (Tag)));

   overriding function Tag
     (Handle : World_Handle)
      return String
   is (-Get (Handle).Tag);

   function Movement_Multiplier
     (World   : World_Handle;
      Terrain : Terrain_Reference;
      Unit    : Unit_Category)
      return Non_Negative_Real
   is (Current_Movement_Multiplier (World.Reference, Terrain, Unit));

   function Road_Multiplier
     (World   : World_Handle;
      Unit    : Unit_Category)
      return Non_Negative_Real
   is (Current_Road_Multiplier (World.Reference, Unit));

   ------------------
   -- Create_World --
   ------------------

   procedure Create_World (Tag : String) is
      Rec : World_Record;
   begin
      Rec.Tag := +Tag;
      --  Rec.Movement := Default_Movement_Multiplier;

      World_Vector.Append (Rec);
        --  (World_Record'
        --     (Tag      => +Tag, Movement => Default_Movement_Multiplier));
            --  Movement => (others => (others => 1.0))));
            --  Road     => (others => 1.0)));
      World_Map.Insert (Tag, World_Vector.Last_Index);
   end Create_World;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      World_Vectors.Vector'Read (Stream, World_Vector);
      Movement_Multiplier_Array'Read (Stream, Current_Movement_Multiplier);
      Road_Multiplier_Array'Read (Stream, Current_Road_Multiplier);
      for Reference in 1 .. World_Vector.Last_Index loop
         World_Map.Insert (Get (Reference).Tag, Reference);
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      World_Vectors.Vector'Write (Stream, World_Vector);
      Movement_Multiplier_Array'Write (Stream, Current_Movement_Multiplier);
      Road_Multiplier_Array'Write (Stream, Current_Road_Multiplier);
   end Save;

   -----------------
   -- Scan_Worlds --
   -----------------

   procedure Scan_Worlds
     (Process : not null access
        procedure (World : World_Handle))
   is
   begin
      for Reference in 1 .. World_Vector.Last_Index loop
         Process (Get (Reference));
      end loop;
   end Scan_Worlds;

   -----------------------
   -- Set_Road_Movement --
   -----------------------

   procedure Set_Road_Movement
     (World      : World_Handle'Class;
      Unit       : Unit_Category;
      Multiplier : Real)
   is
   begin
      Current_Road_Multiplier (World.Reference, Unit) := Multiplier;
   end Set_Road_Movement;

      --  Movement : Road_Multiplier_Array renames
      --               World_Vector (World.Reference).Road;
   --  begin
   --     Movement (Unit) := Multiplier;
   --  end Set_Road_Movement;

   --------------------------
   -- Set_Terrain_Movement --
   --------------------------

   procedure Set_Terrain_Movement
     (World      : World_Handle'Class;
      Unit       : Unit_Category;
      Terrain    : Terrain_Reference;
      Multiplier : Real)
   is
   begin
      Current_Movement_Multiplier (World.Reference, Terrain, Unit) :=
        Multiplier;
   end Set_Terrain_Movement;

      --  Movement : Movement_Multiplier_Array renames
      --               World_Vector (World.Reference).Movement;
   --  begin
   --     Movement (Terrain, Unit) := Multiplier;
   --  end Set_Terrain_Movement;

end Carthage.Handles.Worlds;
