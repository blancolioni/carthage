with Carthage.Handles.Houses;
with Carthage.Handles.Planets;
with Carthage.Handles.Stacks;
with Carthage.Handles.Terrain;
with Carthage.Handles.Tiles;
with Carthage.Handles.Worlds;

with Carthage.Handles.Vectors;

with Carthage.Logging;

package body Carthage.Handles.Assets is

   Log_Cargo : constant Boolean := False;

   package Asset_Vectors is
     new Carthage.Handles.Vectors
       (Real_Asset_Reference, Asset_Record, "asset");

   Asset_Vector : Asset_Vectors.Vector;

   function Get
     (Handle : Asset_Handle)
      return Asset_Vectors.Constant_Reference_Type
   is (Asset_Vector (Handle.Reference));

   overriding function Identifier
     (Asset : Asset_Handle)
      return Object_Identifier
   is (Get (Asset).Identifier);

   overriding function Short_Name
     (Asset : Asset_Handle)
      return String
   is (Get (Asset).Identifier
       & "-"
       & Carthage.Handles.Houses.Get (Get (Asset).Owner).Tag
       & "-"
       & Asset.Unit.Tag);

   function Unit
     (Asset : Asset_Handle)
      return Carthage.Handles.Units.Unit_Handle
     is (Carthage.Handles.Units.Get (Get (Asset).Unit));

   function Stack
     (Asset : Asset_Handle)
      return Stack_Reference
   is (Get (Asset).Stack);

   function Owner (This : Asset_Handle) return House_Reference
   is (Get (This).Owner);

   function Movement
     (This : Asset_Handle)
      return Natural
   is (This.Unit.Movement);

   function Spot
     (This : Asset_Handle)
      return Natural
   is (This.Unit.Spot);

   function Health
     (This : Asset_Handle)
      return Health_Type
   is (Get (This).Health);

   function Cargo_Capacity
     (This : Asset_Handle)
      return Natural
   is (This.Unit.Cargo_Capacity);

   function Has_Resource
     (This     : Asset_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Boolean
   is ((This.Resource_Cargo.Reference = Resource.Reference)
       and then Carthage.Quantities.">"
         (This.Resource_Quantity, Carthage.Quantities.Zero));

   function Resource_Cargo
     (This : Asset_Handle)
      return Carthage.Handles.Resources.Resource_Handle
   is (Carthage.Handles.Resources.Get (Get (This).Resource));

   function Resource_Quantity
     (This : Asset_Handle)
      return Carthage.Quantities.Quantity_Type
   is (Get (This).Quantity);

   function Alive
     (This : Asset_Handle)
      return Boolean
   is (This.Health > 0);

   ------------------
   -- Add_Quantity --
   ------------------

   procedure Add_Quantity
     (This     : Asset_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
   is

      procedure Update (Rec : in out Asset_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Asset_Record) is
         use type Carthage.Quantities.Quantity_Type;
      begin
         Rec.Quantity := Rec.Quantity + Quantity;
      end Update;

   begin
      Asset_Vector.Update (This.Reference, Update'Access);

      if Log_Cargo then
         This.Log ("added " & Carthage.Quantities.Show (Quantity)
                   & " " & This.Resource_Cargo.Local_Text
                   & "; total now "
                   & Carthage.Quantities.Show (This.Resource_Quantity));
      end if;

   end Add_Quantity;

   ---------------
   -- Can_Enter --
   ---------------

   function Can_Enter
     (This  : Asset_Handle;
      World : World_Reference;
      Tile  : Tile_Reference)
      return Boolean
   is
   begin
      return This.Movement_Cost (World, Tile) > 0.0;
   end Can_Enter;

   ------------------
   -- Create_Asset --
   ------------------

   function Create_Asset
     (Rec : Asset_Record)
      return Asset_Reference
   is
   begin
      return Reference : Asset_Reference do
         Asset_Vector.Append (Rec, Reference);
      end return;
   end Create_Asset;

   ------------
   -- Damage --
   ------------

   procedure Damage
     (This   : Asset_Handle;
      Points : Positive)
   is

      procedure Update (Rec : in out Asset_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Asset_Record) is
         New_Health : constant Health_Type :=
                        (if Points > Natural (This.Health)
                         then 0
                         else This.Health - Health_Type (Points));
      begin
         Rec.Health := New_Health;
      end Update;
   begin
      Asset_Vector.Update (This.Reference, Update'Access);
   end Damage;

   ------------------
   -- Delete_Asset --
   ------------------

   procedure Delete_Asset (This : Asset_Handle) is

      Current_Stack : constant Stack_Reference :=
                        Get (This).Stack;

      procedure Update (Rec : in out Asset_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Asset_Record) is
      begin
         Rec.Active := False;
         Rec.Stack := Null_Stack_Reference;
         Rec.Owner := Null_House_Reference;
         Rec.Unit := Null_Unit_Reference;
      end Update;

   begin
      Asset_Vector.Update (This.Reference, Update'Access);
      if Current_Stack /= Null_Stack_Reference then
         Carthage.Handles.Stacks.Get (Current_Stack)
           .Remove_Asset (This.Reference);
      end if;
   end Delete_Asset;

   -----------------
   -- Finish_Jump --
   -----------------

   procedure Finish_Jump (This : Asset_Handle) is

      procedure Update (Rec : in out Asset_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Asset_Record) is
      begin
         Rec.Jumping := False;
      end Update;

   begin
      Asset_Vector.Update (This.Reference, Update'Access);
   end Finish_Jump;

   --------------------------
   -- For_All_Owned_Assets --
   --------------------------

   procedure For_All_Owned_Assets
     (Owner   : House_Reference;
      Process : not null access
        procedure (Asset : Asset_Handle))
   is
   begin
      for Reference in 1 .. Asset_Vector.Last_Index loop
         declare
            Asset : constant Asset_Handle := Get (Reference);
         begin
            if Get (Asset).Owner = Owner then
               Process (Asset);
            end if;
         end;
      end loop;
   end For_All_Owned_Assets;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Asset_Vector.Read (Stream);
      Carthage.Logging.Log
        ("loaded" & Asset_Vector.Last_Index'Image & " assets");
   end Load;

   --------------------
   -- Log_Identifier --
   --------------------

   overriding function Log_Identifier
     (Asset : Asset_Handle)
      return String
   is
      Stack  : constant Carthage.Handles.Stacks.Stack_Handle :=
                 Carthage.Handles.Stacks.Get (Get (Asset).Stack);
      Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                 Carthage.Handles.Planets.Get
                   (Stack.Planet);
      Tile   : constant Carthage.Handles.Tiles.Tile_Handle :=
                 Stack.Current_Tile;
   begin
      return Asset.Identifier
        & "-"
        & Carthage.Handles.Houses.Get (Get (Asset).Owner).Tag
        & "-"
        & Asset.Unit.Tag
        & (if Asset.Unit.Is_Cargo_Pod
           and then Asset.Resource_Cargo.Has_Element
           then "-["
           & Carthage.Quantities.Show (Asset.Resource_Quantity)
           & " "
           & Asset.Resource_Cargo.Tag
           & "]"
           else "")
        & "-["
        & Planet.Local_Text
        & "]"
        & (if Tile.Has_Element
           then "-[" & Tile.Description & "]"
           else "");
   end Log_Identifier;

   -------------------
   -- Movement_Cost --
   -------------------

   function Movement_Cost
     (This  : Asset_Handle;
      World : World_Reference;
      Tile  : Tile_Reference)
      return Non_Negative_Real
   is
      use Carthage.Handles.Terrain;
      use Carthage.Handles.Tiles;
      Layers : constant Terrain_Layer_Array :=
                 Carthage.Handles.Tiles.Get (Tile).Terrain_Layers;
      Road   : constant Boolean :=
                 Carthage.Handles.Tiles.Get (Tile).Has_Road;
      W      : constant Carthage.Handles.Worlds.World_Handle :=
                 Carthage.Handles.Worlds.Get (World);
      Category : constant Unit_Category := This.Unit.Category;
      Cost : Real := 1.0;
   begin
      for Terrain of Layers loop
         exit when not Terrain.Has_Element;
         Cost := Cost * W.Movement_Multiplier (Terrain.Reference, Category);
      end loop;
      if Road then
         Cost := Cost * W.Road_Multiplier (Category);
      end if;
      return Cost;
   end Movement_Cost;

   ---------------------
   -- Remove_Quantity --
   ---------------------

   procedure Remove_Quantity
     (This     : Asset_Handle;
      Quantity : Carthage.Quantities.Quantity_Type;
      Received : out Carthage.Quantities.Quantity_Type)
   is

      procedure Update (Rec : in out Asset_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Asset_Record) is
         use Carthage.Quantities;
      begin
         Received := Min (Quantity, Rec.Quantity);
         Rec.Quantity := Rec.Quantity - Received;
      end Update;

   begin
      Asset_Vector.Update (This.Reference, Update'Access);
   end Remove_Quantity;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Asset_Vector.Write (Stream);
   end Save;

   ------------------
   -- Set_Quantity --
   ------------------

   procedure Set_Quantity
     (This     : Asset_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
   is

      procedure Update (Rec : in out Asset_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Asset_Record) is
      begin
         Rec.Quantity := Quantity;
      end Update;

   begin
      Asset_Vector.Update (This.Reference, Update'Access);
   end Set_Quantity;

   ------------------
   -- Set_Resource --
   ------------------

   procedure Set_Resource
     (This     : Asset_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle)
   is
      procedure Update (Rec : in out Asset_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Asset_Record) is
      begin
         Rec.Resource := Resource.Reference;
      end Update;

   begin
      Asset_Vector.Update (This.Reference, Update'Access);
   end Set_Resource;

   ----------------
   -- Start_Jump --
   ----------------

   procedure Start_Jump (This : Asset_Handle) is

      procedure Update (Rec : in out Asset_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Asset_Record) is
      begin
         Rec.Jumping := True;
      end Update;

   begin
      Asset_Vector.Update (This.Reference, Update'Access);
   end Start_Jump;

end Carthage.Handles.Assets;
