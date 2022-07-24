with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Exceptions;

with Carthage.Handles.Assets.Create;
with Carthage.Handles.Planets;
with Carthage.Handles.Units;

with Carthage.Handles.Vectors;

package body Carthage.Handles.Stacks is

   type Stack_Asset_Array is
     array (Stack_Asset_Index) of Asset_Reference;

   package Tile_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Tile_Reference);

   type Property_Array is array (Stack_Property) of Boolean;

   package Stack_Order_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Stack_Order_Record);

   type Stack_Record is
      record
         Identifier    : Object_Identifier;
         Active        : Boolean;
         Orbital       : Boolean;
         Moving        : Boolean;
         Properties    : Property_Array;
         House         : House_Reference;
         Planet        : Planet_Reference;
         Tile          : Tile_Reference;
         Next_Tile     : Tile_Reference;
         Departed_At   : Carthage.Calendar.Time;
         Move_Duration : Duration;
         Size          : Stack_Asset_Count;
         Assets        : Stack_Asset_Array;
         Path          : Tile_Lists.List;
         Orders        : Stack_Order_Lists.List;
      end record;

   package Stack_Vectors is
     new Carthage.Handles.Vectors
       (Real_Stack_Reference, Stack_Record, "stack");

   Stack_Vector : Stack_Vectors.Vector;

   function Get
     (Handle : Stack_Handle)
      return Stack_Vectors.Constant_Reference_Type
   is (Stack_Vector (Handle.Reference));

   function Owner
     (This : Stack_Handle)
      return Carthage.Handles.Houses.House_Handle
   is (Carthage.Handles.Houses.Get (Get (This).House));

   overriding function Identifier
     (This : Stack_Handle)
      return Object_Identifier
   is (Get (This).Identifier);

   overriding function Short_Name
     (Stack : Stack_Handle)
      return String
   is (Stack.Owner.Tag
       & "-" & Carthage.Handles.Planets.Get (Stack.Planet).Tag
       & "-" & Get (Stack).Identifier);

   function Has_Assets (This : Stack_Handle) return Boolean
   is (This.Asset_Count > 0);

   function Is_Full (This : Stack_Handle) return Boolean
   is (This.Asset_Count = Maximum_Stack_Size);

   function Is_Orbital (This : Stack_Handle) return Boolean
   is (Get (This).Orbital);

   function Is_Ground (This : Stack_Handle) return Boolean
   is (not This.Is_Orbital);

   function Planet (This : Stack_Handle) return Planet_Reference
   is (Get (This).Planet);

   function Has_Property
     (This     : Stack_Handle;
      Property : Stack_Property)
      return Boolean
   is (Get (This).Properties (Property));

   function Has_Movement
     (This : Stack_Handle)
      return Boolean
   is (not Get (This).Path.Is_Empty);

   function Current_Tile
     (This : Stack_Handle)
      return Carthage.Handles.Tiles.Tile_Handle
   is (Carthage.Handles.Tiles.Get (Get (This).Tile));

   function Next_Tile
     (This : Stack_Handle)
      return Carthage.Handles.Tiles.Tile_Handle
   is (Carthage.Handles.Tiles.Get (Get (This).Next_Tile));

   function Current_Position
     (This : Stack_Handle)
      return Tile_Position
   is (Carthage.Handles.Tiles.Get (Get (This).Tile).Position);

   function Next_Position
     (This : Stack_Handle)
      return Tile_Position
   is (Carthage.Handles.Tiles.Get (Get (This).Next_Tile).Position);

   function First_Order
     (Stack : Stack_Handle'Class)
      return Stack_Order_Record
   is (Get (Stack_Handle (Stack)).Orders.First_Element);

   function Can_Enter
     (This : Stack_Handle;
      Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Boolean
   is (for all Reference of Get (This).Assets (1 .. This.Asset_Count) =>
          Carthage.Handles.Assets.Get (Reference).Can_Enter
       (Carthage.Handles.Planets.Get (This.Planet).World,
          Tile.Reference));

   function Movement_Duration
     (This : Stack_Handle;
      Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Duration
   is (Duration (7.0 * 24.0 * 60.0 * 60.0
       * This.Movement_Cost (Tile)
       / Non_Negative_Real (This.Movement)));

   function Asset_Count (This : Stack_Handle) return Stack_Asset_Count
   is (Get (This).Size);

   function Asset (This  : Stack_Handle;
                   Index : Stack_Asset_Index)
                   return Asset_Reference
   is (Get (This).Assets (Index));

   ---------------
   -- Add_Asset --
   ---------------

   procedure Add_Asset
     (This  : Stack_Handle;
      Asset : Asset_Reference)
   is
      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
      begin
         Rec.Size := Rec.Size + 1;
         Rec.Assets (Rec.Size) := Asset;
      end Update;

   begin
      Stack_Vector.Update (This.Reference, Update'Access);
   end Add_Asset;

   --------------------
   -- Clear_Movement --
   --------------------

   procedure Clear_Movement
     (Stack : Stack_Handle'Class)
   is
      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
      begin
         Rec.Path.Clear;
      end Update;

   begin
      Stack_Vector.Update (Stack.Reference, Update'Access);
   end Clear_Movement;

   --------------------
   -- Clear_Property --
   --------------------

   procedure Clear_Property
     (This     : in out Stack_Handle;
      Property : Stack_Property)
   is
      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
      begin
         Rec.Properties (Property) := False;
      end Update;

   begin
      Stack_Vector.Update (This.Reference, Update'Access);
   end Clear_Property;

   ------------------
   -- Create_Stack --
   ------------------

   function Create_Stack
     (Owner     : House_Reference;
      Planet    : Planet_Reference;
      Tile      : Tile_Reference)
      return Stack_Reference
   is
      Reference : Stack_Reference;
      Rec       : constant Stack_Record :=
                    Stack_Record'
                      (Identifier    => Next_Identifier,
                       Active        => True,
                       Orbital       => Tile = Null_Tile_Reference,
                       Moving        => False,
                       Properties    => (others => False),
                       House         => Owner,
                       Planet        => Planet,
                       Tile          => Tile,
                       Next_Tile     => Null_Tile_Reference,
                       Departed_At   => Carthage.Calendar.Clock,
                       Move_Duration => 0.0,
                       Size          => 0,
                       Assets        => (others => Null_Asset_Reference),
                       Path          => <>,
                       Orders        => <>);
   begin
      Stack_Vector.Append (Rec, Reference);
      if Tile /= Null_Tile_Reference then
         Carthage.Handles.Tiles.Get (Tile).Add_Stack (Reference);
      end if;
      return Reference;
   end Create_Stack;

   ----------------------
   -- Current_Movement --
   ----------------------

   function Current_Movement
     (This : Stack_Handle)
      return Array_Of_Positions
   is
      Last : Natural := 0;
      Result : Array_Of_Positions (1 .. Natural (Get (This).Path.Length));
   begin
      for Step of Get (This).Path loop
         Last := Last + 1;
         Result (Last) := Carthage.Handles.Tiles.Get (Step).Position;
      end loop;
      return Result;
   end Current_Movement;

   ------------------------
   -- Delete_First_Order --
   ------------------------

   procedure Delete_First_Order
     (Stack : Stack_Handle'Class)
   is
      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
      begin
         Rec.Orders.Delete_First;
      end Update;

   begin
      Stack_Vector.Update (Stack.Reference, Update'Access);
   end Delete_First_Order;

   ------------------
   -- Delete_Stack --
   ------------------

   procedure Delete_Stack (Stack : Stack_Handle) is

      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
      begin
         Rec.Path.Clear;
         Rec.Orders.Clear;
         Rec.Properties := (others => False);
         Rec.Size := 0;
         Rec.Assets := (others => Null_Asset_Reference);
         Rec.House := Null_House_Reference;
         Rec.Planet := Null_Planet_Reference;
         Rec.Tile := Null_Tile_Reference;
         Rec.Active := False;
      end Update;

   begin
      for I in 1 .. Stack.Asset_Count loop
         Carthage.Handles.Assets.Get (Stack.Asset (I)).Delete_Asset;
      end loop;
      Stack_Vector.Update (Stack.Reference, Update'Access);
   end Delete_Stack;

   -----------------
   -- Description --
   -----------------

   function Description
     (This : Stack_Handle)
      return String
   is
      Planet_Tag : constant String :=
                     Carthage.Handles.Planets.Get (This.Planet).Tag;
   begin
      return This.Owner.Tag
        & " stack size"
        & This.Asset_Count'Image
        & (if This.Is_Orbital
           then " orbiting "
           & Planet_Tag
           else " at "
           & Carthage.Handles.Tiles.Position_Image
             (This.Current_Position)
           & " on " & Planet_Tag);
   end Description;

   ---------------------------
   -- Execute_Movement_Step --
   ---------------------------

   procedure Execute_Movement_Step
     (Stack : Stack_Handle'Class)
   is
      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
      begin
         Carthage.Handles.Tiles.Get (Rec.Tile).Remove_Stack (Stack.Reference);
         Rec.Tile := Rec.Path.First_Element;
         Rec.Path.Delete_First;
         Carthage.Handles.Tiles.Get (Rec.Tile).Add_Stack (Stack.Reference);
      end Update;

   begin
      Stack_Vector.Update (Stack.Reference, Update'Access);
   end Execute_Movement_Step;

   ---------------
   -- Find_Path --
   ---------------

   function Find_Path
     (This    : Stack_Handle;
      Tile    : Carthage.Handles.Tiles.Tile_Handle)
      return Array_Of_Positions
   is
      use Carthage.Handles.Planets;
      use Carthage.Handles.Tiles;

      function Passable
        (Tile : Tile_Reference)
         return Boolean
      is (This.Can_Enter (Get (Tile)));

      function Move_Cost
        (Tile : Tile_Reference)
         return Non_Negative_Real
      is (This.Movement_Cost (Get (Tile)));

   begin
      return Get (This.Planet).Find_Path
        (Start    => Get (Get (This).Tile).Position,
         Finish   => Tile.Position,
         Passable => Passable'Access,
         Cost     => Move_Cost'Access);
   end Find_Path;

   ---------------------------
   -- For_All_Ground_Stacks --
   ---------------------------

   procedure For_All_Ground_Stacks
     (Process : not null access procedure (Stack : Stack_Handle))
   is
   begin
      for Reference in 1 .. Stack_Vector.Last_Index loop
         if Stack_Vector.Element (Reference).Active
           and then not Stack_Vector.Element (Reference).Orbital
         then
            Process (Get (Reference));
         end if;
      end loop;
   end For_All_Ground_Stacks;

   --------------------
   -- For_All_Stacks --
   --------------------

   procedure For_All_Stacks
     (Process : not null access procedure (Stack : Stack_Handle))
   is
   begin
      for Reference in 1 .. Stack_Vector.Last_Index loop
         if Stack_Vector.Element (Reference).Active then
            Process (Get (Reference));
         end if;
      end loop;
   end For_All_Stacks;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Stack_Vector.Read (Stream);
   end Load;

   ------------------
   -- Move_To_Tile --
   ------------------

   procedure Move_To_Tile
     (This  : Stack_Handle;
      Tile  : Carthage.Handles.Tiles.Tile_Handle)
   is

      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
      begin
         Rec.Orders.Append (Stack_Order_Record'
                              (Order_Type  => Move_To_Tile,
                               Destination => Tile.Reference));
      end Update;

   begin
      This.Log ("moving to " & Tile.Description);
      Stack_Vector.Update (This.Reference, Update'Access);
   end Move_To_Tile;

   --------------
   -- Movement --
   --------------

   function Movement
     (This : Stack_Handle)
      return Natural
   is
   begin
      return Result : Natural := Natural'Last do
         for Index in 1 .. This.Asset_Count loop
            declare
               M : constant Natural :=
                     Carthage.Handles.Assets.Get
                       (This.Asset (Index)).Movement;
            begin
               Result := Natural'Min (Result, M);
            end;
         end loop;
      end return;
   end Movement;

   -------------------
   -- Movement_Cost --
   -------------------

   function Movement_Cost
     (This : Stack_Handle;
      Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Non_Negative_Real
   is
      Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                 Carthage.Handles.Planets.Get (Get (This).Planet);
   begin
      return Result : Non_Negative_Real := 0.0 do
         for Index in 1 .. This.Asset_Count loop
            declare
               Asset : constant Carthage.Handles.Assets.Asset_Handle :=
                         Carthage.Handles.Assets.Get (This.Asset (Index));
               M     : constant Non_Negative_Real :=
                         Asset.Movement_Cost
                           (World => Planet.World,
                            Tile  => Tile.Reference);
            begin
               Result := Real'Max (Result, M);
            end;
         end loop;
      end return;
   end Movement_Cost;

   -----------------------
   -- Movement_Progress --
   -----------------------

   function Movement_Progress
     (This : Stack_Handle)
      return Unit_Real
   is
      use type Carthage.Calendar.Time;
      Elapsed : constant Duration :=
                  Carthage.Calendar.Clock
                    - Get (This).Departed_At;
   begin
      return Unit_Clamp
        (Real (Elapsed) / Real (Get (This).Move_Duration));
   end Movement_Progress;

   --------------
   -- Quantity --
   --------------

   overriding function Quantity
     (This     : Stack_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type
   is
      use type Carthage.Quantities.Quantity_Type;
   begin
      return Total : Carthage.Quantities.Quantity_Type :=
        Carthage.Quantities.Zero
      do
         for I in 1 .. This.Asset_Count loop
            declare
               Asset : constant Carthage.Handles.Assets.Asset_Handle :=
                         Carthage.Handles.Assets.Get (This.Asset (I));
            begin
               if Asset.Resource_Cargo.Reference = Resource.Reference then
                  Total := Total + Asset.Resource_Quantity;
               end if;
            end;
         end loop;
      end return;
   end Quantity;

   ------------------
   -- Remove_Asset --
   ------------------

   procedure Remove_Asset
     (This  : Stack_Handle;
      Asset : Asset_Reference)
   is
      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
         Found : Boolean := False;
      begin
         for I in 1 .. Rec.Size loop
            if Found then
               Rec.Assets (I - 1) := Rec.Assets (I);
            elsif Rec.Assets (I) = Asset then
               Found := True;
            end if;
         end loop;
         if Found then
            Rec.Size := Rec.Size - 1;
         end if;
      end Update;

   begin
      Stack_Vector.Update (This.Reference, Update'Access);
   end Remove_Asset;

   ------------------------
   -- Remove_Dead_Assets --
   ------------------------

   procedure Remove_Dead_Assets
     (This : Stack_Handle)
   is
      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
         use Carthage.Handles.Assets;
         Target : Stack_Asset_Count := 0;
      begin
         for I in 1 .. Rec.Size loop
            if Get (Rec.Assets (I)).Alive then
               Target := Target + 1;
               if Target /= I then
                  Rec.Assets (Target) := Rec.Assets (I);
               end if;
            end if;
         end loop;
         Rec.Size := Target;
      end Update;

   begin
      Stack_Vector.Update (This.Reference, Update'Access);
   end Remove_Dead_Assets;

   --------------------------------
   -- Remove_Empty_Ground_Stacks --
   --------------------------------

   procedure Remove_Empty_Ground_Stacks is

      procedure Remove_If_Empty (Stack : Stack_Handle);

      ---------------------
      -- Remove_If_Empty --
      ---------------------

      procedure Remove_If_Empty (Stack : Stack_Handle) is
      begin
         if Stack.Is_Ground and then Stack.Asset_Count = 0 then
            Stack.Delete_Stack;
         end if;
      end Remove_If_Empty;

   begin
      For_All_Stacks (Remove_If_Empty'Access);
   end Remove_Empty_Ground_Stacks;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Stack_Vector.Write (Stream);
   end Save;

   procedure Set_Movement_Path
     (Stack : Stack_Handle'Class;
      Path  : Array_Of_Positions)
   is
      Tiles : Tile_Lists.List;

      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
      begin
         Rec.Path := Tiles;
         Rec.Next_Tile := Tiles.First_Element;
      end Update;

      Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                 Carthage.Handles.Planets.Get (Stack.Planet);
   begin
      for Position of Path loop
         Tiles.Append (Planet.Get_Tile (Position).Reference);
      end loop;

      Stack_Vector.Update (Stack.Reference, Update'Access);
   end Set_Movement_Path;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (This     : in out Stack_Handle;
      Property : Stack_Property)
   is
      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
      begin
         Rec.Properties (Property) := True;
      end Update;

   begin
      Stack_Vector.Update (This.Reference, Update'Access);
   end Set_Property;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (This     : Stack_Handle;
      Item     : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
   is
   begin
      for I in 1 .. This.Asset_Count loop
         declare
            Asset : constant Carthage.Handles.Assets.Asset_Handle :=
                      Carthage.Handles.Assets.Get
                        (This.Asset (I));
         begin
            if Asset.Resource_Cargo.Reference = Item.Reference then
               Asset.Set_Quantity (Quantity);
               return;
            end if;
         exception
            when E : others =>
               Asset.Log ("cannot update quantity: "
                          & Ada.Exceptions.Exception_Message (E));
         end;
      end loop;

      declare
         Asset : constant Carthage.Handles.Assets.Asset_Handle :=
                   Carthage.Handles.Assets.Create.New_Asset
                     (Unit    => Carthage.Handles.Units.Cargo_Pod,
                      Owner   => This.Owner,
                      Stack   => This.Reference,
                      XP      => Carthage.Handles.Assets.Green,
                      Loyalty => Loyalty_Type'Last,
                      Health  => Health_Type'Last);
      begin
         Asset.Set_Resource (Item);
         Asset.Add_Quantity (Quantity);
      end;
   end Set_Quantity;

   ----------
   -- Spot --
   ----------

   function Spot
     (This : Stack_Handle)
      return Natural
   is
   begin
      return Result : Natural := 0 do
         for Index in 1 .. This.Asset_Count loop
            declare
               M : constant Natural :=
                     Carthage.Handles.Assets.Get
                       (This.Asset (Index)).Spot;
            begin
               Result := Natural'Max (Result, M);
            end;
         end loop;
      end return;
   end Spot;

   -------------------------
   -- Start_Next_Movement --
   -------------------------

   procedure Start_Next_Movement
     (Stack : Stack_Handle'Class;
      Clock : Carthage.Calendar.Time)
   is

      Move_Time : constant Duration :=
                    Stack.Movement_Duration
                      (Carthage.Handles.Tiles.Get
                         (Get (Stack_Handle (Stack)).Path.First_Element));

      procedure Update (Rec : in out Stack_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stack_Record) is
      begin
         Rec.Next_Tile := Rec.Path.First_Element;
         Rec.Move_Duration := Move_Time;
         Rec.Departed_At := Clock;
      end Update;

   begin
      Stack_Vector.Update (Stack.Reference, Update'Access);
   end Start_Next_Movement;

   -----------
   -- Total --
   -----------

   function Total
     (This  : Stack_Handle;
      Value : not null access
        function (Asset : Carthage.Handles.Assets.Asset_Handle) return Integer)
      return Integer
   is
      Rec : Stack_Record renames Get (This);
   begin
      return Result : Integer := 0 do
         for I in 1 .. Rec.Size loop
            Result := Result
              + Value (Carthage.Handles.Assets.Get (Rec.Assets (I)));
         end loop;
      end return;
   end Total;

   --------------------
   -- Total_Strength --
   --------------------

   function Total_Strength
     (This : Stack_Handle)
      return Natural
   is

      function Asset_Strength
        (Asset : Carthage.Handles.Assets.Asset_Handle)
         return Integer;

      --------------------
      -- Asset_Strength --
      --------------------

      function Asset_Strength
        (Asset : Carthage.Handles.Assets.Asset_Handle)
         return Integer
      is
         use Carthage.Handles.Units;
         Unit : constant Unit_Handle := Asset.Unit;
      begin
         return Strength : Integer := 0 do
            for Weapon in Weapon_Category loop
               if Unit.Has_Attack (Weapon) then
                  Strength := Strength + Unit.Strength (Weapon);
               end if;
            end loop;
         end return;
      end Asset_Strength;

   begin
      return Total (This, Asset_Strength'Access);
   end Total_Strength;

end Carthage.Handles.Stacks;
