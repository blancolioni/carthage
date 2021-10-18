with Ada.Containers.Vectors;
with WL.String_Maps;

with Carthage.Handles.Worlds;

package body Carthage.Handles.Terrain is

   type Terrain_World_Record is
      record
         Color     : Carthage.Colors.Color_Type;
         Road_Cost : Natural;
      end record;

   type Terrain_World_Array is
     array (Real_World_Reference) of Terrain_World_Record;

   type Terrain_Record is
      record
         Tag        : Ada.Strings.Unbounded.Unbounded_String;
         Categories : Terrain_World_Array :=
                        (others => ((0.5, 0.5, 0.5, 1.0), 1));
         Water      : Boolean := False;
         Ocean      : Boolean := False;
      end record;

   package Terrain_Vectors is
     new Ada.Containers.Vectors (Real_Terrain_Reference, Terrain_Record);

   package Terrain_Maps is
     new WL.String_Maps (Terrain_Reference);

   Terrain_Vector : Terrain_Vectors.Vector;
   Terrain_Map    : Terrain_Maps.Map;

   function Get
     (Handle : Terrain_Handle)
      return Terrain_Vectors.Constant_Reference_Type
   is (Terrain_Vector (Handle.Reference));

   function Is_Terrain_Tag (Tag : String) return Boolean
   is (Terrain_Map.Contains (Tag));

   function Get (Tag : String) return Terrain_Handle
   is (Get (Terrain_Map (Tag)));

   overriding function Tag
     (Handle : Terrain_Handle)
      return String
   is (-Get (Handle).Tag);

   function Is_Ocean (This : Terrain_Handle) return Boolean
   is (Get (This).Ocean);

   function Is_Water (This : Terrain_Handle) return Boolean
   is (Get (This).Water);

   -----------
   -- Color --
   -----------

   function Color
     (This     : Terrain_Handle;
      Category : String)
      return Carthage.Colors.Color_Type
   is
   begin
      return Get (This)
        .Categories (Carthage.Handles.Worlds.Get (Category).Reference)
        .Color;
   end Color;

   ------------
   -- Create --
   ------------

   procedure Create
     (Tag                : String;
      Ocean              : Boolean;
      Water              : Boolean;
      Configure_Category : not null access
        procedure (Category : String;
                   Color    : out Carthage.Colors.Color_Type;
                   Road_Cost : out Natural))
   is
      Rec : Terrain_Record :=
              Terrain_Record'
                (Tag        => +Tag,
                 Categories => <>,
                 Water      => Water,
                 Ocean      => Ocean);

      procedure Cat_Config (World : Carthage.Handles.Worlds.World_Handle);

      ----------------
      -- Cat_Config --
      ----------------

      procedure Cat_Config (World : Carthage.Handles.Worlds.World_Handle) is
         Color : Carthage.Colors.Color_Type;
         Road_Cost : Natural;
      begin
         Configure_Category (World.Tag, Color, Road_Cost);
         Rec.Categories (World.Reference) :=
           Terrain_World_Record'
             (Color     => Color,
              Road_Cost => Road_Cost);
      end Cat_Config;

   begin
      Carthage.Handles.Worlds.Scan_Worlds
        (Cat_Config'Access);
      Terrain_Vector.Append (Rec);
      Terrain_Map.Insert (Tag, Terrain_Vector.Last_Index);
   end Create;

   ---------------------
   -- For_All_Terrain --
   ---------------------

   procedure For_All_Terrain
     (Process : not null access
        procedure (Terrain : Terrain_Handle))
   is
   begin
      for Reference in 1 .. Terrain_Vector.Last_Index loop
         Process (Get (Reference));
      end loop;
   end For_All_Terrain;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Terrain_Vectors.Vector'Read (Stream, Terrain_Vector);
      for Reference in 1 .. Terrain_Vector.Last_Index loop
         Terrain_Map.Insert (Get (Reference).Tag, Reference);
      end loop;
   end Load;

   ---------------
   -- Road_Cost --
   ---------------

   function Road_Cost
     (This     : Terrain_Handle;
      Category : String)
      return Natural
   is
   begin
      return Get (This)
        .Categories (Carthage.Handles.Worlds.Get (Category).Reference)
        .Road_Cost;
   end Road_Cost;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Terrain_Vectors.Vector'Write (Stream, Terrain_Vector);
   end Save;

end Carthage.Handles.Terrain;
