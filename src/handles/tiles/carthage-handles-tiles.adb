with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;

with Carthage.Handles.Assets;
with Carthage.Handles.Cities;
with Carthage.Handles.Stacks.Create;
with Carthage.Handles.Structures;

with Carthage.Handles.Vectors;

package body Carthage.Handles.Tiles is

   package Stack_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Stack_Reference);

   type Variable_Tile_Record is
      record
         City        : City_Reference;
         Road        : Boolean;
         Seen_By     : Set_Of_Houses;
         Explored_By : Set_Of_Houses;
         Visible     : Set_Of_Houses;
         Stacks      : Stack_Lists.List;
      end record;

   package Variable_Tile_Vectors is
     new Carthage.Handles.Vectors
       (Real_Tile_Reference, Variable_Tile_Record, "tile");

   Variable_Tile_Vector : Variable_Tile_Vectors.Vector;

   function Get_Variable
     (Handle : Tile_Handle)
      return Variable_Tile_Vectors.Constant_Reference_Type
   is (Variable_Tile_Vector (Handle.Reference));

   overriding function Identifier
     (Handle : Tile_Handle)
      return Object_Identifier
   is (Get (Handle).Identifier);

   overriding function Short_Name
     (Handle : Tile_Handle)
      return String
   is (Position_Image (Handle.Position));

   function Terrain_Layers
     (This : Tile_Handle)
      return Terrain_Layer_Array
   is (Get (This).Terrain);

   function Base_Terrain
     (This : Tile_Handle)
      return Carthage.Handles.Terrain.Terrain_Handle
   is (Get (This).Terrain (1));

   function Is_Water (This : Tile_Handle) return Boolean
   is (This.Base_Terrain.Is_Water);

   function Has_City (This : Tile_Handle) return Boolean
   is (Get_Variable (This).City /= Null_City_Reference);

   function Get_City
     (This : Tile_Handle)
      return City_Reference
   is (Get_Variable (This).City);

   function Has_Road (This : Tile_Handle) return Boolean
   is (Get_Variable (This).Road);

   function Has_Stacks (This : Tile_Handle) return Boolean
   is (not Get_Variable (This).Stacks.Is_Empty);

   function First_Stack
     (This : Tile_Handle)
      return Stack_Reference
   is (Get_Variable (This).Stacks.First_Element);

   --------------------
   -- Position_Image --
   --------------------

   function Position_Image (Position : Tile_Position) return String
   is ("("
       & Ada.Strings.Fixed.Trim (Tile_X'Image (Position.X), Ada.Strings.Left)
       & ","
       & Ada.Strings.Fixed.Trim (Tile_Y'Image (Position.Y), Ada.Strings.Left)
       & ")");

   ------------------
   -- Add_Resource --
   ------------------

   procedure Add_Resource
     (Tile     : Tile_Handle;
      Owner    : Carthage.Handles.Houses.House_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
   is
      use type Carthage.Quantities.Quantity_Type;
   begin
      Tile.Set_Resource_Quantity
        (Owner    => Owner,
         Resource => Resource,
         Quantity => Tile.Resource_Quantity (Owner, Resource) + Quantity);
   end Add_Resource;

   ---------------
   -- Add_Stack --
   ---------------

   procedure Add_Stack
     (This  : Tile_Handle;
      Stack : Stack_Reference)
   is
      procedure Update (Rec : in out Variable_Tile_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Variable_Tile_Record) is
      begin
         Rec.Stacks.Append (Stack);
      end Update;

   begin
      Variable_Tile_Vector.Update (This.Reference, Update'Access);
   end Add_Stack;

   ----------------------
   -- Clear_Visibility --
   ----------------------

   procedure Clear_Visibility
     (This  : Tile_Handle)
   is
      procedure Update (Rec : in out Variable_Tile_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Variable_Tile_Record) is
      begin
         Clear (Rec.Visible);
      end Update;

   begin
      Variable_Tile_Vector.Update (This.Reference, Update'Access);
   end Clear_Visibility;

   -----------------
   -- Create_Tile --
   -----------------

   function Create_Tile
     (Planet   : Planet_Reference;
      Position : Tile_Position;
      River    : Boolean;
      Road     : Boolean;
      Terrain  : Terrain_Layer_Array)
      return Tile_Reference
   is
      Constant_Rec : Tile_Record :=
                       Tile_Record'
                         (Identifier  => Next_Identifier,
                          Planet      => Planet,
                          Position    => Position,
                          River       => River,
                          Terrain     => Terrain,
                          Has_Terrain => (others => False));
      Variable_Rec : constant Variable_Tile_Record :=
                       Variable_Tile_Record'
                         (City        => Null_City_Reference,
                          Road        => Road,
                          Seen_By     => Empty_House_Set,
                          Explored_By => Empty_House_Set,
                          Visible     => Empty_House_Set,
                          Stacks      => <>);
      Reference   : Tile_Reference;
   begin
      for T of Terrain loop
         Constant_Rec.Has_Terrain (T.Reference) := True;
      end loop;

      Variable_Tile_Vector.Append (Variable_Rec, Reference);
      Tile_Vector.Append (Constant_Rec);
      pragma Assert (Tile_Vector.Last_Index = Reference);
      return Tile_Vector.Last_Index;
   end Create_Tile;

   --------------------------
   -- Currently_Visible_To --
   --------------------------

   function Currently_Visible_To
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
      return Boolean
   is
   begin
      return House.Is_Element_Of (Get_Variable (This).Visible);
   end Currently_Visible_To;

   -----------------
   -- Description --
   -----------------

   function Description
     (This : Tile_Handle)
      return String
   is
      use Carthage.Handles.Terrain;
      Rec : Tile_Record renames Get (This);
      Layer_1 : constant Terrain_Handle := Rec.Terrain (1);
      Layer_2 : constant Terrain_Handle := Rec.Terrain (2);
      City    : constant City_Reference :=
                  Get_Variable (This).City;
      Terrain_Text : constant String :=
                       (if Layer_2.Has_Element
                        and then Layer_2 /= Layer_1
                        then Layer_1.Tag & " " & Layer_2.Tag
                        else Layer_1.Tag);
      City_Text : constant String :=
                       (if City = Null_City_Reference then ""
                        else Carthage.Handles.Structures.Get
                          (Carthage.Handles.Cities.Get (City).Structure)
                          .Tag);
   begin
      return Position_Image (Position (This))
        & " "
        & (if City_Text = "" then "" else City_Text & " ") & Terrain_Text;
   end Description;

   -----------------
   -- Explored_By --
   -----------------

   function Explored_By
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
      return Boolean
   is
   begin
      return House.Is_Element_Of
        (Get_Variable (This).Explored_By);
   end Explored_By;

   ----------------
   -- Find_Stack --
   ----------------

   function Find_Stack
     (This  : Tile_Handle;
      Match : not null access
        function (Stack : Stack_Reference)
      return Boolean)
      return Stack_Reference
   is
   begin
      for Stack of Get_Variable (This).Stacks loop
         if Match (Stack) then
            return Stack;
         end if;
      end loop;
      return Null_Stack_Reference;
   end Find_Stack;

   ---------------
   -- Has_Stack --
   ---------------

   function Has_Stack
     (This  : Tile_Handle;
      Match : not null access
        function (Stack : Stack_Reference)
      return Boolean)
      return Boolean
   is
   begin
      for Stack of Get_Variable (This).Stacks loop
         if Match (Stack) then
            return True;
         end if;
      end loop;
      return False;
   end Has_Stack;

   ---------------
   -- Has_Stack --
   ---------------

   function Has_Stack
     (This  : Tile_Handle;
      Stack : Stack_Reference)
      return Boolean
   is
   begin
      return Stack_Lists.Has_Element (Get_Variable (This).Stacks.Find (Stack));
   end Has_Stack;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Tile_Vectors.Vector'Read (Stream, Tile_Vector);
      Variable_Tile_Vector.Read (Stream);
   end Load;

   ---------------------
   -- Remove_Resource --
   ---------------------

   procedure Remove_Resource
     (Tile     : Tile_Handle;
      Owner    : Carthage.Handles.Houses.House_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
   is
      use type Carthage.Quantities.Quantity_Type;
   begin
      Tile.Set_Resource_Quantity
        (Owner    => Owner,
         Resource => Resource,
         Quantity => Tile.Resource_Quantity (Owner, Resource) - Quantity);
   end Remove_Resource;

   ------------------
   -- Remove_Stack --
   ------------------

   procedure Remove_Stack
     (This  : Tile_Handle;
      Stack : Stack_Reference)
   is
      procedure Update (Rec : in out Variable_Tile_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Variable_Tile_Record) is
         Position : Stack_Lists.Cursor := Rec.Stacks.Find (Stack);
      begin
         Rec.Stacks.Delete (Position);
      end Update;

   begin
      Variable_Tile_Vector.Update (This.Reference, Update'Access);
   end Remove_Stack;

   -----------------------
   -- Resource_Quantity --
   -----------------------

   function Resource_Quantity
     (Tile     : Tile_Handle;
      House    : Carthage.Handles.Houses.House_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type
   is
      use Carthage.Quantities;
      Quantity : Quantity_Type := Zero;

      procedure Check_Stack (Reference : Stack_Reference);

      -----------------
      -- Check_Stack --
      -----------------

      procedure Check_Stack (Reference : Stack_Reference) is
         Stack : constant Carthage.Handles.Stacks.Stack_Handle :=
                   Carthage.Handles.Stacks.Get (Reference);
      begin
         if Stack.Owner.Reference = House.Reference then
            for Index in 1 .. Stack.Asset_Count loop
               declare
                  use Carthage.Handles.Assets;
                  Asset : constant Asset_Handle :=
                            Get (Stack.Asset (Index));
               begin
                  if Asset.Resource_Cargo.Reference
                    = Resource.Reference
                  then
                     Quantity := Quantity + Asset.Resource_Quantity;
                  end if;
               end;
            end loop;
         end if;
      end Check_Stack;

   begin
      Tile.Scan_Stacks (Check_Stack'Access);
      return Quantity;
   end Resource_Quantity;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Tile_Vectors.Vector'Write (Stream, Tile_Vector);
      Variable_Tile_Vector.Write (Stream);
   end Save;

   -----------------
   -- Scan_Stacks --
   -----------------

   procedure Scan_Stacks
     (This : Tile_Handle;
     Process       : not null access
        procedure (Stack : Stack_Reference);
      Skip_Empty    : Boolean := True)
   is
   begin
      for Stack of Get_Variable (This).Stacks loop
         if not Skip_Empty
           or else Carthage.Handles.Stacks.Get (Stack).Has_Assets
         then
            Process (Stack);
         end if;
      end loop;
   end Scan_Stacks;

   -------------
   -- Seen_By --
   -------------

   function Seen_By
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
      return Boolean
   is
   begin
      return House.Is_Element_Of
        (Get_Variable (This).Seen_By);
   end Seen_By;

   --------------
   -- Set_City --
   --------------

   procedure Set_City
     (This : Tile_Handle;
      City : City_Reference)
   is
      procedure Update (Rec : in out Variable_Tile_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Variable_Tile_Record) is
      begin
         Rec.City := City;
      end Update;

   begin
      Variable_Tile_Vector.Update (This.Reference, Update'Access);
   end Set_City;

   ------------------------------
   -- Set_Currently_Visible_To --
   ------------------------------

   procedure Set_Currently_Visible_To
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
   is
      procedure Update (Rec : in out Variable_Tile_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Variable_Tile_Record) is
      begin
         House.Include (Rec.Seen_By);
         House.Include (Rec.Explored_By);
         House.Include (Rec.Visible);
      end Update;

   begin
      Variable_Tile_Vector.Update (This.Reference, Update'Access);
   end Set_Currently_Visible_To;

   ---------------------
   -- Set_Explored_By --
   ---------------------

   procedure Set_Explored_By
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
   is
      procedure Update (Rec : in out Variable_Tile_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Variable_Tile_Record) is
      begin
         House.Include (Rec.Explored_By);
      end Update;

   begin
      Variable_Tile_Vector.Update (This.Reference, Update'Access);
   end Set_Explored_By;

   ---------------------------
   -- Set_Resource_Quantity --
   ---------------------------

   procedure Set_Resource_Quantity
     (Tile     : Tile_Handle;
      Owner    : Carthage.Handles.Houses.House_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
   is
      use Carthage.Quantities;
      Stack     : Carthage.Handles.Stacks.Stack_Handle;
   begin
      for Reference of Get_Variable (Tile).Stacks loop
         declare
            Handle : constant Carthage.Handles.Stacks.Stack_Handle :=
                       Carthage.Handles.Stacks.Get (Reference);
         begin
            if Handle.Owner.Reference = Owner.Reference then
               if Handle.Quantity (Resource) > Zero then
                  Stack := Handle;
                  exit;
               end if;

               if not Stack.Has_Element then
                  Stack := Handle;
               end if;

            end if;
         end;
      end loop;

      if not Stack.Has_Element then
         Stack := Carthage.Handles.Stacks.Create.New_Ground_Stack
           (Manager => null,
            Owner   => Owner,
            Planet  => Tile.Planet,
            Tile    => Tile);
      end if;

      Stack.Set_Quantity (Resource, Quantity);

   end Set_Resource_Quantity;

   --------------
   -- Set_Road --
   --------------

   procedure Set_Road
     (This : Tile_Handle;
      Road : Boolean)
   is
      procedure Update (Rec : in out Variable_Tile_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Variable_Tile_Record) is
      begin
         Rec.Road := Road;
      end Update;

   begin
      Variable_Tile_Vector.Update (This.Reference, Update'Access);
   end Set_Road;

   -----------------
   -- Set_Seen_By --
   -----------------

   procedure Set_Seen_By
     (This  : Tile_Handle;
      House : Carthage.Handles.Houses.House_Handle)
   is
      procedure Update (Rec : in out Variable_Tile_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Variable_Tile_Record) is
      begin
         House.Include (Rec.Seen_By);
      end Update;

   begin
      Variable_Tile_Vector.Update (This.Reference, Update'Access);
   end Set_Seen_By;

   -------------
   -- Terrain --
   -------------

   function Terrain
     (This  : Tile_Handle;
      Layer : Terrain_Layer)
      return Carthage.Handles.Terrain.Terrain_Handle
   is
   begin
      return Get (This).Terrain (Layer);
   end Terrain;

end Carthage.Handles.Tiles;
