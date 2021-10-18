with Hira.Db;

with Hira.Asset;
with Hira.House;
with Hira.Movement_Step;
with Hira.Planet;
with Hira.Stack_Order;

with Carthage.Calendar;
with Carthage.Logging;

with Carthage.Handles.Planets;
with Carthage.Worlds;
with Carthage.Handles.Units;

package body Carthage.Handles.Stacks is

   procedure Add_Order
     (This           : Stack_Class;
      Move_To_Tile   : Carthage.Handles.Tiles.Tile_Handle := Carthage.Handles.Tiles.Empty_Tile;
      Move_To_Asset  : Hira.Asset.Asset_Handle := Hira.Asset.Empty_Handle;
      Move_To_Planet : Carthage.Handles.Planets.Planet_Handle := Hira.Planet.Empty_Handle);

   ---------------
   -- Add_Order --
   ---------------

   procedure Add_Order
     (This           : Stack_Class;
      Move_To_Tile   : Carthage.Handles.Tiles.Tile_Handle := Carthage.Handles.Tiles.Empty_Tile;
      Move_To_Asset  : Hira.Asset.Asset_Handle := Hira.Asset.Empty_Handle;
      Move_To_Planet : Carthage.Handles.Planets.Planet_Handle := Hira.Planet.Empty_Handle)
   is
      Order_Index : constant Positive := This.Last_Order + 1;
      Order       : constant Hira.Stack_Order.Stack_Order_Class :=
                      Hira.Stack_Order.Get_By_Stack_Order (This, Order_Index);
   begin
      if Order.Has_Element then
         Order.Update_Stack_Order
           .Set_Move_To_Tile (Carthage.Handles.Tiles.Get_Hira_Tile (Move_To_Tile))
           .Set_Move_To_Asset (Move_To_Asset)
           .Set_Move_To_Planet (Move_To_Planet)
           .Done;
      else
         Hira.Stack_Order.Create
           (Stack          => This,
            Index          => Order_Index,
            Move_To_Tile   => Carthage.Handles.Tiles.Get_Hira_Tile (Move_To_Tile),
            Move_To_Asset  => Move_To_Asset,
            Move_To_Planet => Move_To_Planet);
      end if;
      This.Update_Stack.Set_Last_Order (Order_Index).Done;
   end Add_Order;

   -----------------
   -- Asset_Count --
   -----------------

   function Asset_Count (This : Stack_Class) return Natural is
   begin
      return Hira.Asset.Length (Hira.Asset.Select_By_Stack (This));
   end Asset_Count;

   --------------------
   -- Clear_Property --
   --------------------

   procedure Clear_Property
     (This : in out Stack_Class;
      Property : Stack_Property)
   is null;
   --  begin
   --     This.Properties (Property) := False;
   --  end Clear_Property;

   ----------------------
   -- Current_Movement --
   ----------------------

   function Current_Movement
     (This : Stack_Class)
      return Array_Of_Positions
   is
      Result : Array_Of_Positions (1 .. 100);
      Index  : Natural := 0;
   begin
      for Step of
        Hira.Movement_Step.Select_Movement_Step_Bounded_By_Index
          (This, This.First_Step, This.Last_Step)
      loop
         Index := Index + 1;
         Result (Index) :=
           Carthage.Handles.Tiles.Get_Position
             (Carthage.Handles.Tiles.Get_Tile (Step.Tile));
      end loop;
      return Result (1 .. Index);
   end Current_Movement;

   ------------------
   -- Delete_Stack --
   ------------------

   procedure Delete_Stack (Stack : Stack_Class) is
      --  use type Carthage.Handles.Tiles.Any_Reference;
   begin
      --  if Stack.Tile /= null then
      --     Stack.Tile.Update.Remove_Stack
      --       (Db.Get (Stack.Index));
      --  end if;

      for Asset of Hira.Asset.Select_By_Stack (Stack) loop
         Carthage.Handles.Assets.Delete_Asset (Asset);
      end loop;

      Stack.Update_Stack
        .Set_House (Hira.House.Empty_Handle)
        .Set_Planet (Hira.Planet.Empty_Handle)
        .Set_Tile (Hira.Tile.Empty_Handle)
        .Set_Size (0)
        .Set_First_Step (1)
        .Set_Last_Step (0)
        .Set_First_Order (1)
        .Set_Last_Order (0)
        .Done;

   end Delete_Stack;

   ---------------
   -- Find_Path --
   ---------------

   function Find_Path
     (This    : Stack_Class;
      Tile    : Carthage.Handles.Tiles.Tile_Handle)
      return Array_Of_Positions
   is
      function Passable
        (Tile : Carthage.Handles.Tiles.Tile_Handle)
         return Boolean
      is (Movement_Cost (This, Tile) > 0.0);

      function Move_Cost
        (Tile : Carthage.Handles.Tiles.Tile_Handle)
                  return Float
      is (Movement_Cost (This, Tile));

   begin
      return Carthage.Handles.Planets.Find_Path
        (This     => This.Planet,
         Start    => Carthage.Handles.Tiles.Get_Position (This.Tile),
         Finish   => Carthage.Handles.Tiles.Get_Position (Tile),
         Passable => Passable'Access,
         Cost     => Move_Cost'Access);
   end Find_Path;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : Stack_Class) return Boolean
   is
   begin
      return not Hira.Asset.First_By_Stack (This).Has_Element;
   end Is_Empty;

   -------------
   -- Is_Full --
   -------------

   function Is_Full (This : Stack_Class) return Boolean is
   begin
      return Hira.Asset.Length (Hira.Asset.Select_By_Stack (This))
        >= Maximum_Stack_Size;
   end Is_Full;

   ---------
   -- Log --
   ---------

   procedure Log
     (This    : Stack_Class;
      Message : String)
   is
   begin
      Carthage.Logging.Log
        (Description (This) & ": " & Message);
   end Log;

   ------------------
   -- Move_To_Tile --
   ------------------

   procedure Move_To_Tile
     (This  : Stack_Class;
      Tile  : Carthage.Handles.Tiles.Tile_Handle)
   is
   begin
      Log (This, "moving to " & Carthage.Handles.Tiles.Description (Tile));
      Add_Order (This, Move_To_Tile => Tile);
   end Move_To_Tile;

   --------------
   -- Movement --
   --------------

   function Movement
     (This : Stack_Class)
      return Natural
   is
   begin
      if Is_Empty (This) then
         return 0;
      else
         return Result : Natural := Natural'Last do
            for Asset of Hira.Asset.Select_By_Stack (This) loop
               declare
                  M : constant Natural := Carthage.Handles.Assets.Movement (Asset);
               begin
                  if M < Result then
                     Result := M;
                  end if;
               end;
            end loop;
         end return;
      end if;
   end Movement;

   -------------------
   -- Movement_Cost --
   -------------------

   function Movement_Cost
     (This : Stack_Class;
      Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Float
   is
      World   : constant Carthage.Worlds.World_Class :=
                  This.Planet.Category;
      Terrain : constant Carthage.Handles.Tiles.Terrain_Layer_Array :=
                  Carthage.Handles.Tiles.Get_Terrain_Layers (Tile);
      Lowest  : Float := Float'Last;
      Road    : constant Boolean :=
                  Carthage.Handles.Tiles.Has_Road (Tile);
   begin
      if Is_Empty (This) then
         return 0.0;
      end if;

      for Asset of Hira.Asset.Select_By_Stack (This) loop
         declare
            Category : constant Carthage.Handles.Units.Unit_Category :=
                         Asset.Unit.Category;
            Cost     : Float := 1.0;
         begin
            for T of Terrain loop
               Cost := Cost
                 * Carthage.Worlds.Movement_Multiplier (World, T, Category);
            end loop;
            if Road then
               Cost := Cost
                 * Carthage.Worlds.Road_Multiplier (World, Category);
            end if;

            Lowest := Float'Min (Lowest, Cost);
            exit when Lowest = 0.0;
         end;
      end loop;

      return Lowest;
   end Movement_Cost;

   -----------------------
   -- Movement_Duration --
   -----------------------

   function Movement_Duration
     (This : Stack_Class;
      Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Duration
   is
   begin
      return Duration (7.0 * 24.0 * 60.0 * 60.0
                       * Movement_Cost (This, Tile)
                       / Float (Movement (This)));
   end Movement_Duration;

   -----------------------
   -- Movement_Progress --
   -----------------------

   function Movement_Progress
     (This : Stack_Class)
      return Float
   is
   begin
      if This.Moving then
         declare
            use Carthage.Calendar;
            D : constant Duration :=
                  Clock - This.Departed_At;
         begin
            return Float (D) / This.Move_Duration;
         end;
      else
         return 0.0;
      end if;
   end Movement_Progress;

   ---------------
   -- Next_Tile --
   ---------------

   function Next_Tile
     (This : Stack_Class)
      return Tile_Position
   is
   begin
      return Carthage.Handles.Tiles.Get_Position
        (Hira.Movement_Step.Get_By_Movement_Step
           (This, This.First_Step)
         .Tile);
   end Next_Tile;

   ------------------------
   -- Remove_Dead_Assets --
   ------------------------

   procedure Remove_Dead_Assets
     (This : Stack_Class)
   is
   begin
      for Asset of
        Hira.Asset.Select_By_Stack
          (This)
      loop
         if not Asset.Active then
            declare
               A : constant Hira.Asset.Asset_Handle := Asset;
            begin
               A.Update_Asset.Set_Stack (Hira.Stack.Empty_Handle).Done;
            end;
         end if;
      end loop;
   end Remove_Dead_Assets;

   --------------------------------
   -- Remove_Empty_Ground_Stacks --
   --------------------------------

   procedure Remove_Empty_Ground_Stacks is

   begin
      for Stack of Hira.Stack.Select_By_Orbital (False) loop
         if Is_Empty (Stack) then
            Delete_Stack (Stack);
         end if;
      end loop;
   end Remove_Empty_Ground_Stacks;

   -----------------
   -- Scan_Stacks --
   -----------------

   procedure Scan_Stacks
     (Process : not null access procedure (Stack : Stack_Class))
   is
   begin
      for Stack of Hira.Stack.Select_By_Active (True) loop
         Process (Stack);
      end loop;
   end Scan_Stacks;

   -----------------
   -- Set_Manager --
   -----------------

   procedure Set_Manager
     (This : Stack_Class;
      Manager : not null access Stack_Manager_Interface'Class)
   is
   begin
      null;
   end Set_Manager;

   ------------------
   -- Set_Property --
   ------------------

   procedure Set_Property
     (This : in out Stack_Class;
      Property : Stack_Property)
   is
   begin
      null;
   end Set_Property;

   ----------
   -- Spot --
   ----------

   function Spot
     (This : Stack_Class)
      return Natural
   is
   begin
      if Is_Empty (This) then
         return 0;
      else
         return Result : Natural := 0 do
            for Asset of Hira.Asset.Select_By_Stack (This) loop
               declare
                  D : constant Natural := Asset.Unit.Spot;
               begin
                  if D > Result then
                     Result := D;
                  end if;
               end;
            end loop;
         end return;
      end if;
   end Spot;

   -----------
   -- Total --
   -----------

   function Total
     (This : Stack_Class;
      Value : not null access
        function (Asset : Carthage.Handles.Assets.Asset_Handle) return Integer)
      return Integer
   is
   begin
      return T : Integer := 0 do
         for Asset of Hira.Asset.Select_By_Stack (This) loop
            T := T + Value (Asset);
         end loop;
      end return;
   end Total;

   --------------------
   -- Total_Strength --
   --------------------

   function Total_Strength
     (This : Stack_Class)
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
         Unit : constant Carthage.Handles.Units.Unit_Class := Asset.Unit;
      begin
         return Strength : Integer := 0 do
            for Weapon in Hira.Db.Weapon_Category loop
               if Carthage.Handles.Units.Has_Attack (Unit, Weapon) then
                  Strength := Strength
                    + Carthage.Handles.Units.Strength (Unit, Weapon);
               end if;
            end loop;
         end return;
      end Asset_Strength;

   begin
      return Total (This, Asset_Strength'Access);
   end Total_Strength;

end Carthage.Handles.Stacks;
