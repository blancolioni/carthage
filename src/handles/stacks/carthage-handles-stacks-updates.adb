with Carthage.Calendar;
with Carthage.Combat;

with Carthage.Handles.Planets;
with Carthage.Handles.Resources;

with Carthage.Messages.Resources;

package body Carthage.Handles.Stacks.Updates is

   type Stack_Manager is access all Stack_Manager_Interface'Class;

   procedure Start_Movement
     (This    : Stack_Handle;
      Manager : Stack_Manager)
     with Unreferenced;

   procedure Execute_Update
     (This    : Stack_Handle;
      Manager : Stack_Manager);
   pragma Unreferenced (Execute_Update);

   procedure Consumption (Stack : Stack_Handle) is
      use Carthage.Quantities;
      Resource : constant Carthage.Handles.Resources.Resource_Handle :=
                   Carthage.Handles.Resources.Food;
      Available : constant Quantity_Type := Stack.Quantity (Resource);
      Required  : Quantity_Type := Zero;
   begin
      for I in 1 .. Stack.Asset_Count loop
         declare
            Asset : constant Assets.Asset_Handle :=
                      Carthage.Handles.Assets.Get (Stack.Asset (I));
         begin
            if Asset.Unit.Eat > Zero then
               Required := Required + Scale (Asset.Unit.Eat, 0.1);
            end if;
         end;
      end loop;

      if Required > Zero then
         declare
            Minimum : constant Quantity_Type := Scale (Required, 5.0);
            Like    : constant Quantity_Type := Scale (Minimum, 2.0);
            Consume : Quantity_Type;
         begin
            Stack.Take (Resource, Required, Consume);

            Stack.Log ("food: require " & Show (Required)
                       & "; available " & Show (Available)
                       & "; minimum " & Show (Minimum)
                       & "; like " & Show (Like)
                       & "; consumed " & Show (Consume));
            if Available < Minimum then
               Stack.Log ("requesting " & Show (Like - Available) & " food");
               declare
                  Message : constant Carthage.Messages.Message_Interface'Class
                    := Carthage.Messages.Resources.Required
                      (House    => Stack.Owner,
                       Tile     => Stack.Current_Tile,
                       Resource => Resource,
                       Quantity => Like - Available,
                       Minimum  => Required);
               begin
                  Message.Send;
               end;
            end if;
         end;
      end if;
   end Consumption;

   --------------------
   -- Execute_Update --
   --------------------

   procedure Execute_Update
     (This    : Stack_Handle;
      Manager : Stack_Manager)
   is

      Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                 Carthage.Handles.Planets.Get (This.Planet);

      procedure Check_Hostile
        (Tile        : Carthage.Handles.Tiles.Tile_Handle;
         Has_Hostile : out Boolean;
         Hostile     : out Stack_Handle);

      procedure Move
        (To   : Tile_Position;
         Stop : out Boolean);

      procedure Attack
        (Hostile : Stack_Handle);

      ------------
      -- Attack --
      ------------

      procedure Attack
        (Hostile : Stack_Handle)
      is
      begin
         This.Log
           ("strength"
            & Natural'Image (This.Total_Strength)
            & " at " & This.Current_Tile.Description
            & " attacking hostile " & Hostile.Description
            & " strength"
            & Natural'Image (Hostile.Total_Strength)
            & " in target tile " & Hostile.Current_Tile.Description);

         Carthage.Combat.New_Battle
           (Attacker => This,
            Defender => Hostile,
            Planet   => Carthage.Handles.Planets.Get (This.Planet),
            Tile     => This.Current_Tile);

      end Attack;

      -------------------
      -- Check_Hostile --
      -------------------

      procedure Check_Hostile
        (Tile        : Carthage.Handles.Tiles.Tile_Handle;
         Has_Hostile : out Boolean;
         Hostile     : out Stack_Handle)
      is
         procedure Check_Stack
           (Reference : Stack_Reference);

         -------------------
         -- Check_Hostile --
         -------------------

         procedure Check_Stack
           (Reference : Stack_Reference)
         is
            Check : constant Stack_Handle := Get (Reference);
         begin
            if This.Owner.At_War_With (Check.Owner) then
               if Manager = null then
                  This.Log
                    ("hostile " & Check.Description
                     & " on " & Check.Current_Tile.Description
                     & " but no manager");
               else
                  Has_Hostile := True;
                  Hostile     := Check;
               end if;
            end if;
         end Check_Stack;

      begin
         Has_Hostile := False;
         Tile.Scan_Stacks (Check_Stack'Access);
      end Check_Hostile;

      ----------
      -- Move --
      ----------

      procedure Move
        (To   : Tile_Position;
         Stop : out Boolean)
      is
         Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                  Planet.Get_Tile (To);

         Cost : constant Non_Negative_Real := This.Movement_Cost (Tile);

         Hostile     : Carthage.Handles.Stacks.Stack_Handle;
         Has_Hostile : Boolean;
      begin

         if Cost = 0.0 then
            This.Log
              ("stopping because we cannot move to "
               & Tile.Description);
            Stop := True;
            return;
         end if;

         Check_Hostile (Tile, Has_Hostile, Hostile);

         if Has_Hostile then
            Attack (Hostile);
            Stop := True;
            return;
         end if;

         Stop := False;
         This.Execute_Movement_Step;

         declare
            use Carthage.Handles.Planets;
            Spotted : Surface_Tiles;
         begin
            Planet.Get_Tiles
              (This.Current_Tile, 0, This.Spot + 1,
               null, Spotted);
            for I in 1 .. Tile_Count (Spotted) loop
               declare
                  Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                           Get_Tile (Spotted, I);

                  procedure Check_Hostile
                    (Reference : Stack_Reference);

                  -------------------
                  -- Check_Hostile --
                  -------------------

                  procedure Check_Hostile
                    (Reference : Stack_Reference)
                  is
                     Check      : constant Stack_Handle := Get (Reference);
                     Check_Stop : Boolean;
                  begin
                     if This.Owner.At_War_With (Check.Owner) then
                        if Manager = null then
                           This.Log
                             ("hostile " & Check.Description
                              & " on " & Check.Current_Tile.Description
                              & " but no manager");
                        else
                           Manager.On_Hostile_Spotted
                             (This, Check, Check_Stop);
                        end if;
                        Stop := Stop or else Check_Stop;
                     end if;
                  end Check_Hostile;

               begin
                  Tile.Set_Currently_Visible_To (This.Owner);
                  Tile.Scan_Stacks (Check_Hostile'Access);
               end;
            end loop;
         end;

      end Move;

   begin

      if This.Has_Movement then

         declare
            Stop : Boolean;
         begin
            Move (This.Next_Position, Stop);

            if Stop then
               This.Log ("stopping at " & This.Current_Tile.Description);
               This.Clear_Movement;
               Manager.On_Movement_Ended (This);
            elsif not This.Has_Movement then
               This.Log ("end of path at " & This.Current_Tile.Description);
               Manager.On_Movement_Ended (This);
            else
               This.Log ("waypoint at " & This.Current_Tile.Description);

               declare
                  Next_Tile   : constant Carthage.Handles.Tiles.Tile_Handle :=
                                  This.Next_Tile;
                  Has_Hostile : Boolean;
                  Hostile     : Carthage.Handles.Stacks.Stack_Handle;
               begin
                  Check_Hostile (Next_Tile, Has_Hostile, Hostile);
                  if Has_Hostile then
                     Attack (Hostile);
                     Stop := True;
                     This.Clear_Movement;
                  else
                     This.Start_Next_Movement (Carthage.Calendar.Clock);

                     --  else
                  --     This.Update.Next_Tile_Cost :=
                  --       This.Movement_Cost
                  --         (This.Planet.Tile
                  --            (Path (Path_Index)));
                  end if;
               end;
            end if;
         end;
      end if;
   end Execute_Update;

   ----------
   -- Look --
   ----------

   procedure Look (Stack : Stack_Handle) is
      Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                 Carthage.Handles.Planets.Get (Stack.Planet);
   begin
      if Stack.Is_Orbital then
         if Stack.Has_Assets then
            Planet.Set_Seen_By (Stack.Owner.Reference);
         end if;
      else
         declare
            Spot : constant Positive :=
                     Integer'Max (Stack.Spot / 2, 1);

            procedure Reveal_Tile (Tile : Carthage.Handles.Tiles.Tile_Handle);

            -----------------
            -- Reveal_Tile --
            -----------------

            procedure Reveal_Tile
              (Tile : Carthage.Handles.Tiles.Tile_Handle)
            is
            begin
               Tile.Set_Currently_Visible_To (Stack.Owner);
            end Reveal_Tile;

         begin
            Planet.Scan_Neighbours_Within
              (Start    => Stack.Current_Tile.Position,
               Distance => Spot,
               Process  => Reveal_Tile'Access);
         end;
      end if;
   end Look;

   --------------------
   -- Start_Movement --
   --------------------

   procedure Start_Movement
     (This    : Stack_Handle;
      Manager : Stack_Manager)
   is
      pragma Unreferenced (Manager);
      use Carthage.Handles.Tiles;

      Order : constant Stack_Order_Record := This.First_Order;
      Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                 Carthage.Handles.Planets.Get (This.Planet);

      function Passable
        (Tile : Tile_Reference)
         return Boolean
      is (This.Can_Enter (Get (Tile)));

      function Move_Cost
        (Tile : Tile_Reference)
         return Non_Negative_Real
      is (This.Movement_Cost (Get (Tile)));

   begin
      case Order.Order_Type is
         when Move_To_Tile =>
            declare
               Path : constant Array_Of_Positions :=
                        Planet.Find_Path
                          (Start    => This.Current_Tile.Position,
                           Finish   =>
                             Carthage.Handles.Tiles.Get
                               (Order.Destination).Position,
                           Passable => Passable'Access,
                           Cost     => Move_Cost'Access);
            begin
               if Path'Length = 0 then
                  This.Clear_Movement;
               else
                  This.Log
                    ("moving to " & Get (Order.Destination).Description
                     & ": path length ="
                     & Natural'Image (Path'Length));
                  This.Set_Movement_Path (Path);
               end if;
            end;
      end case;
   end Start_Movement;

end Carthage.Handles.Stacks.Updates;
