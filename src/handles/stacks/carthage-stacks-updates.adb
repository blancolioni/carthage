with WL.Random;

with Reiko.Updates;

with Hira.Movement_Step;
with Hira.Stack_Order;

with Carthage.Calendar;
with Carthage.Logging;

with Carthage.Combat;
with Carthage.Handles.Houses;
with Carthage.Handles.Planets;
--  with Carthage.Handles.Units;

package body Carthage.Handles.Stacks.Updates is

   type Stack_Manager is access all Stack_Manager_Interface'Class;

   function Next_Arrival_Time
     (This : Stack_Class)
      return Carthage.Calendar.Time
     with Pre => Has_Movement (This), Unreferenced;

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
   is (Description (Update.Stack));

   overriding procedure Execute
     (Update : Stack_Update);

   procedure Start_Movement
     (This : Stack_Class);

   procedure Execute
     (This    : Stack_Class;
      Manager : Stack_Manager);

   procedure Set_Path
     (This   : Stack_Class;
      Path   : Array_Of_Positions);

   procedure Clear_Path
     (This : Stack_Class);

   ----------------
   -- Clear_Path --
   ----------------

   procedure Clear_Path
     (This : Stack_Class)
   is
   begin
      This.Update_Stack
        .Set_First_Step (1)
        .Set_Last_Step (0)
        .Done;
   end Clear_Path;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Stack_Update)
   is
   begin
      if Update.Start then
         Start_Movement (Update.Stack);
      else
         Execute (Update.Stack, Update.Manager);
      end if;

      if Has_Movement (Update.Stack) then
         Update.Stack.Update_Stack
           .Set_Departed_At (Carthage.Calendar.Clock)
           .Set_Move_Duration
             (Float
                (Movement_Duration
                   (Update.Stack, Update.Stack.Next_Tile)))
             .Done;
         Reiko.Updates.Add_Update
           (Update       =>
              Stack_Update'(Update with delta Start => False),
            Update_Delay => Reiko.Reiko_Duration (Update.Stack.Move_Duration));
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This    : Stack_Class;
      Manager : Stack_Manager)
   is

      --  function Match
      --    (S : not null access constant Any_Instance)
      --     return Boolean
      --  is (S.Tag = This.Tag);
      --
      --  Ref         : constant Any_Reference :=
      --                  Any_Reference
      --                    (Stack.Tile.Find_Stack
      --                       (Match'Access));

      procedure Check_Hostile
        (Tile        : Carthage.Handles.Tiles.Tile_Handle;
         Has_Hostile : out Boolean;
         Hostile     : out Carthage.Handles.Stacks.Stack_Handle);

      procedure Move
        (Tile : Carthage.Handles.Tiles.Tile_Handle;
         Stop : out Boolean);

      procedure Attack
        (Hostile : Carthage.Handles.Stacks.Stack_Handle);

      ------------
      -- Attack --
      ------------

      procedure Attack
        (Hostile : Carthage.Handles.Stacks.Stack_Handle)
      is
      begin
         Carthage.Logging.Log
           (Description (This)
            & ": "
            & "strength"
            & Natural'Image (Total_Strength (This))
            & " at " & Carthage.Handles.Tiles.Description (This.Tile)
            & " attacking hostile " & Description (Hostile)
            & " strength"
            & Natural'Image (Total_Strength (Hostile))
            & " in target tile "
            & Carthage.Handles.Tiles.Description (Hostile.Tile));

         Carthage.Combat.New_Battle
           (This, Hostile, This.Planet,
            Carthage.Handles.Tiles.Get_Tile (This.Tile));
      end Attack;

      -------------------
      -- Check_Hostile --
      -------------------

      procedure Check_Hostile
        (Tile        : Carthage.Handles.Tiles.Tile_Handle;
         Has_Hostile : out Boolean;
         Hostile     : out Carthage.Handles.Stacks.Stack_Handle)
      is
         procedure Check_Stack
           (Check : Carthage.Handles.Stacks.Stack_Handle);

         -------------------
         -- Check_Hostile --
         -------------------

         procedure Check_Stack
           (Check : Carthage.Handles.Stacks.Stack_Handle)
         is
         begin
            if Carthage.Handles.Houses.At_War_With (This.House, Check.House) then
               if This.Manager = "" then
                  Log
                    (This,
                     "hostile " & Description (Check)
                     & " on " & Carthage.Handles.Tiles.Description (Check.Tile)
                     & " but no manager");
               else
                  Has_Hostile := True;
                  Hostile     := Check.To_Stack_Handle;
               end if;
            end if;
         end Check_Stack;
      begin
         Has_Hostile := False;
         Carthage.Handles.Tiles.Scan_Stacks (Tile, Check_Stack'Access);
      end Check_Hostile;

      ----------
      -- Move --
      ----------

      procedure Move
        (Tile : Carthage.Handles.Tiles.Tile_Handle;
         Stop : out Boolean)
      is

         Cost : constant Float := Carthage.Handles.Stacks.Movement_Cost (This, Tile);

         Hostile     : Carthage.Handles.Stacks.Stack_Handle;
         Has_Hostile : Boolean;
      begin

         if Cost = 0.0 then
            Log
              (This,
               "stopping because we cannot move to "
               & Carthage.Handles.Tiles.Description (Tile));
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
         This.Update_Stack
           .Set_Tile (Carthage.Handles.Tiles.Get_Hira_Tile (Tile))
             .Done;

         declare
            use Carthage.Handles.Planets;
            Spotted : Surface_Tiles;
         begin
            Carthage.Handles.Planets.Get_Tiles
              (This         => This.Planet,
               Origin       => Carthage.Handles.Tiles.Get_Tile (This.Tile),
               Min_Distance => 0,
               Max_Distance => Spot (This) + 1,
               Test         => null,
               Tiles        => Spotted);

            for I in 1 .. Tile_Count (Spotted) loop
               declare
                  Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                           Get_Tile (Spotted, I);

                  procedure Check_Hostile
                    (Check : Stack_Class);

                  -------------------
                  -- Check_Hostile --
                  -------------------

                  procedure Check_Hostile
                    (Check : Stack_Class)
                  is
                     Check_Stop : Boolean;
                  begin
                     if Carthage.Handles.Houses.At_War_With
                       (This.House, Check.House)
                     then
                        if This.Manager = "" then
                           Log
                             (This,
                              "hostile " & Description (Check)
                              & " on "
                              & Carthage.Handles.Tiles.Description (Check.Tile)
                              & " but no manager");
                        else
                           Manager.On_Hostile_Spotted
                             (This, Check, Check_Stop);
                        end if;
                        Stop := Stop or else Check_Stop;
                     end if;
                  end Check_Hostile;

               begin
                  Carthage.Handles.Tiles.Set_Currently_Visible_To (Tile, This.House);
                  Carthage.Handles.Tiles.Scan_Stacks (Tile, Check_Hostile'Access);
               end;
            end loop;
         end;

      end Move;

   begin

      if This.Last_Step >= This.First_Step then
         declare
            Path_Index : Positive := This.First_Step;
            Movement_Step : constant Hira.Movement_Step.Movement_Step_Class :=
                              Hira.Movement_Step.Get_By_Movement_Step
                                (This, Path_Index);
            Stop       : Boolean := False;
         begin
            Move (Carthage.Handles.Tiles.Get_Tile (Movement_Step.Tile), Stop);
            Path_Index := Path_Index + 1;

            if Stop then
               Log (This, "stopping at "
                    & Carthage.Handles.Tiles.Description (This.Tile));
               Clear_Path (This);
               --                 Stack.Manager.On_Movement_Ended (Stack);
            elsif Path_Index > This.Last_Step then
               Log (This, "end of path at "
                         & Carthage.Handles.Tiles.Description (This.Tile));
               Clear_Path (This);
               Manager.On_Movement_Ended (This);
            else
               Log (This, "waypoint at "
                         & Carthage.Handles.Tiles.Description (This.Tile));
               This.Update_Stack.Set_First_Step (Path_Index).Done;

               declare
                  Next_Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                                Carthage.Handles.Tiles.Get_Tile
                                  (Hira.Movement_Step.Get_By_Movement_Step
                                     (This, Path_Index)
                                   .Tile);
                  Has_Hostile : Boolean;
                  Hostile     : Carthage.Handles.Stacks.Stack_Handle;
               begin
                  Check_Hostile (Next_Tile, Has_Hostile, Hostile);
                  if Has_Hostile then
                     Attack (Hostile);
                     Stop := True;
                     Clear_Path (This);
                  else
                     declare
                        use Hira.Movement_Step;
                        Next_Step : constant Movement_Step_Class :=
                                      Get_By_Movement_Step
                                        (This, Path_Index);
                        Next_Tile : constant Hira.Tile.Tile_Class :=
                                      Next_Step.Tile;
                        --  Next_Cost : constant Float :=
                        --                Movement_Cost (This, Next_Tile);
                     begin
                        This.Update_Stack
                          .Set_Next_Tile (Next_Tile)
                          .Set_Departed_At (Carthage.Calendar.Clock)
                          .Set_Move_Duration
                            (Float
                               (Movement_Duration (This, Next_Tile)))
                          .Done;
                     end;
                  end if;
               end;
            end if;
         end;
      end if;
   end Execute;

   -----------------------
   -- Next_Arrival_Time --
   -----------------------

   function Next_Arrival_Time
     (This : Stack_Class)
      return Carthage.Calendar.Time
   is
      use Carthage.Calendar;
   begin
      return Clock +
        Movement_Duration (This, This.Next_Tile);
   end Next_Arrival_Time;

   --------------
   -- Set_Path --
   --------------

   procedure Set_Path
     (This   : Stack_Class;
      Path   : Array_Of_Positions)
   is
   begin
      for I in Path'Range loop
         declare
            Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                     Carthage.Handles.Planets.Get_Tile (This.Planet, Path (I));
            Step : constant Hira.Movement_Step.Movement_Step_Class :=
                     Hira.Movement_Step.Get_By_Movement_Step
                       (This, I);
         begin
            if Step.Has_Element then
               Step.Update_Movement_Step
                 .Set_Tile (Carthage.Handles.Tiles.Get_Hira_Tile (Tile))
                 .Set_Progress (0.0)
                 .Done;
            else
               Hira.Movement_Step.Create
                 (Stack    => This,
                  Index    => I,
                  Tile     => Carthage.Handles.Tiles.Get_Hira_Tile (Tile),
                  Progress => 0.0);
            end if;
         end;
      end loop;
      This.Update_Stack
        .Set_First_Step (Path'First)
        .Set_Last_Step (Path'Last)
        .Done;
   end Set_Path;

   --------------------
   -- Start_Movement --
   --------------------

   procedure Start_Movement
     (This : Stack_Class)
   is
      function Passable
        (Tile : Carthage.Handles.Tiles.Tile_Handle)
         return Boolean
      is (Can_Enter (This, Tile));

      function Move_Cost
        (Tile : Carthage.Handles.Tiles.Tile_Handle)
         return Float
      is (Movement_Cost (This, Tile));

      Order : constant Hira.Stack_Order.Stack_Order_Class :=
                Hira.Stack_Order.Get_By_Stack_Order
                  (This, This.First_Order);
   begin
      if not Order.Has_Element then
         null;
      elsif Order.Move_To_Tile.Has_Element then
         declare
            Path : constant Array_Of_Positions :=
                     Carthage.Handles.Planets.Find_Path
                       (This.Planet,
                        Carthage.Handles.Tiles.Get_Position (This.Tile),
                        Carthage.Handles.Tiles.Get_Position (Order.Move_To_Tile),
                        Passable'Access,
                        Move_Cost'Access);
         begin
            if Path'Length = 0 then
               Clear_Path (This);
            else
               Log
                 (This,
                  "moving to "
                  & Carthage.Handles.Tiles.Description (Order.Move_To_Tile)
                  & ": path length ="
                  & Natural'Image (Path'Length));
               Set_Path (This, Path);
            end if;
         end;
      elsif Order.Move_To_Asset.Has_Element then
         null;
      elsif Order.Move_To_Planet.Has_Element then
         null;
      end if;
   end Start_Movement;

   ------------------
   -- Start_Update --
   ------------------

   procedure Start_Update
     (Stack   : Stack_Class;
      Manager : not null access Stack_Manager_Interface'Class)
   is
      use Carthage.Calendar;
      Delay_Duration : constant Duration :=
                         Hours (4)
                         + Duration (WL.Random.Random_Number (1, 7200));
      Update         : constant Stack_Update :=
                         Stack_Update'
                           (Reiko.Root_Update_Type with
                            Start   => True,
                            Stack   => Stack.To_Stack_Handle,
                            Manager => Stack_Manager (Manager));
   begin
      Reiko.Updates.Add_Update
        (Update       => Update,
         Update_Delay => Reiko.Reiko_Duration (Delay_Duration));
   end Start_Update;

end Carthage.Handles.Stacks.Updates;
