with WL.Random;

with Reiko.Updates;

with Carthage.Calendar;
with Carthage.Combat;

with Carthage.Handles.Planets;

package body Carthage.Handles.Stacks.Updates is

   type Stack_Manager is access all Stack_Manager_Interface'Class;

   type Stack_Update is
     new Reiko.Root_Update_Type with
      record
         Start   : Boolean;
         Stack   : Stack_Handle;
         Manager : Stack_Manager;
      end record;

   overriding function Name
     (Update : Stack_Update)
      return String
   is (Update.Stack.Log_Identifier & ": "
       & Update.Stack.Description);

   overriding procedure Execute
     (Update : Stack_Update);

   procedure Start_Movement
     (This    : Stack_Handle;
      Manager : Stack_Manager);

   procedure Execute_Update
     (This    : Stack_Handle;
      Manager : Stack_Manager);

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Stack_Update)
   is
   begin
      if Update.Start then
         Update.Stack.Log ("starting movement");
         Start_Movement (Update.Stack, Update.Manager);
      else
         Update.Stack.Log ("executing movement");
         Execute_Update (Update.Stack, Update.Manager);
      end if;

      if Update.Stack.Has_Movement then
         Update.Stack.Log ("scheduling next arrival");
         Reiko.Updates.Add_Update
           (Update       =>
              Stack_Update'
                (Reiko.Root_Update_Type with
                 Start   => False,
                 Stack   => Update.Stack,
                 Manager => Update.Manager),
            Update_Delay =>
              Reiko.Reiko_Duration
                (Update.Stack.Movement_Duration
                     (Update.Stack.Next_Tile)));
      end if;
   end Execute;

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

   ------------------
   -- Start_Update --
   ------------------

   procedure Start_Update
     (Stack   : Stack_Handle;
      Manager : not null access Stack_Manager_Interface'Class)
   is
      use Carthage.Calendar;
      Delay_Duration : constant Duration :=
        Hours (4)
        + Duration (WL.Random.Random_Number (1, 7200));
   begin
      Stack.Log
        ("starting update: delay "
         & Carthage.Calendar.Image (Delay_Duration, True));
      declare
         Update : constant Stack_Update :=
                    Stack_Update'
                      (Reiko.Root_Update_Type with
                       Start   => True,
                       Stack   => Stack,
                       Manager => Stack_Manager (Manager));
      begin
         Reiko.Updates.Add_Update
           (Update       => Update,
            Update_Delay => Reiko.Reiko_Duration (Delay_Duration));
      end;
   end Start_Update;

   --  begin
   --     pragma Compile_Time_Warning
   --       (Standard.True, "Start_Update unimplemented");
   --     raise Program_Error with "Unimplemented procedure Start_Update";
   --  end Start_Update;

end Carthage.Handles.Stacks.Updates;
