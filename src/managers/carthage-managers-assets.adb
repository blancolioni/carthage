with Carthage.Handles.Assets.Create;
with Carthage.Handles.Assets.Moves;
with Carthage.Handles.Galaxy;
with Carthage.Handles.Stacks.Create;
with Carthage.Handles.Stacks.Updates;
with Carthage.Handles.Units;

package body Carthage.Managers.Assets is

   type Ground_Asset_Manager_Record is
     new Root_Asset_Manager_Record with
      record
         Planet       : Carthage.Handles.Planets.Planet_Handle;
         Stacks       : Managed_Stack_List.List;
         Stack_Maps   : Managed_Stack_Maps.Map;
      end record;

   type Ground_Asset_Manager_Type is
     access all Ground_Asset_Manager_Record'Class;

   overriding function Name
     (Manager : Ground_Asset_Manager_Record)
      return String
   is (Manager.House.Tag & " " & Manager.Planet.Tag
       & " ground asset manager");

   overriding function Description
     (Manager : Ground_Asset_Manager_Record)
      return String
   is (Manager.House.Tag & " " & Manager.Planet.Tag
       & " ground asset manager");

   overriding procedure Load_Assets
     (Manager : in out Ground_Asset_Manager_Record);

   overriding procedure On_Hostile_Spotted
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : Carthage.Handles.Stacks.Stack_Handle'Class;
      Hostile : Carthage.Handles.Stacks.Stack_Handle'Class;
      Stop    : out Boolean);

   overriding procedure On_Hostile_Spotted
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : Carthage.Handles.Stacks.Stack_Handle'Class;
      Hostile : Carthage.Handles.Stacks.Stack_Handle'Class)
      is null;

   overriding procedure On_Stack_Removed
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : Carthage.Handles.Stacks.Stack_Handle'Class);

   overriding procedure On_Movement_Ended
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : Carthage.Handles.Stacks.Stack_Handle'Class);

   overriding function Have_Immediate_Capacity
     (Manager : Ground_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   overriding function Check_Goal
     (Manager : not null access Ground_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   type Space_Asset_Manager_Record is
     new Root_Asset_Manager_Record with
      record
         null;
      end record;

   type Space_Asset_Manager_Type is
     access all Space_Asset_Manager_Record'Class;

   overriding function Name
     (Manager : Space_Asset_Manager_Record)
      return String
   is (Manager.House.Tag
       & " space asset manager");

   overriding function Description
     (Manager : Space_Asset_Manager_Record)
      return String
   is (Manager.House.Tag
       & " space asset manager");

   overriding function Have_Immediate_Capacity
     (Manager : Space_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   overriding function Check_Goal
     (Manager : not null access Space_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   overriding procedure Load_Assets
     (Manager : in out Space_Asset_Manager_Record);

   overriding procedure On_Hostile_Spotted
     (Manager : in out Space_Asset_Manager_Record;
      Stack   : Carthage.Handles.Stacks.Stack_Handle'Class;
      Hostile : Carthage.Handles.Stacks.Stack_Handle'Class;
      Stop    : out Boolean)
   is null;

   overriding procedure On_Hostile_Spotted
     (Manager : in out Space_Asset_Manager_Record;
      Stack   : Carthage.Handles.Stacks.Stack_Handle'Class;
      Hostile : Carthage.Handles.Stacks.Stack_Handle'Class)
      is null;

   ----------------
   -- Check_Goal --
   ----------------

   overriding function Check_Goal
     (Manager : not null access Ground_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is
      Asset_Goal : Asset_Manager_Goal renames Asset_Manager_Goal (Goal);

      function Best_Available
        (List   : Asset_Classification_List.List;
         Target : Tile_Position)
         return Managed_Asset_List.Cursor;

      --------------------
      -- Best_Available --
      --------------------

      function Best_Available
        (List   : Asset_Classification_List.List;
         Target : Tile_Position)
         return Managed_Asset_List.Cursor
      is
         Best_Asset    : Managed_Asset_List.Cursor :=
                           Managed_Asset_List.No_Element;
         Best_Distance : Natural := Natural'Last;

      begin
         for Cursor of List loop
            declare
               Managed_Asset : Managed_Asset_Record renames
                                 Manager.Assets (Cursor);
               Managed_Stack : Managed_Stack_Record renames
                                 Manager.Stacks (Managed_Asset.Stack);
               Stack         : constant Handles.Stacks.Stack_Handle :=
                                 Managed_Stack.Stack;
               Planet        : constant Handles.Planets.Planet_Handle :=
                                 Handles.Planets.Get
                                   (Managed_Stack.Stack.Planet);
            begin
               if Managed_Stack.Goal.Is_Empty
                 and then Stack.Can_Enter (Planet.Get_Tile (Target))
               then
                  declare
                     Position : constant Tile_Position :=
                                  Stack.Current_Tile.Position;
                     Distance : constant Natural :=
                                  Planet.Hex_Distance (Position, Target);
                  begin
                     if not Managed_Asset_List.Has_Element (Best_Asset)
                       or else Distance < Best_Distance
                     then
                        Best_Asset := Cursor;
                        Best_Distance := Distance;
                     end if;
                  end;
               end if;
            end;
         end loop;
         return Best_Asset;
      end Best_Available;

   begin
      case Asset_Goal.Class is
         when None =>
            return True;

         when Recon =>
            declare
               Asset_Cursor : constant Managed_Asset_List.Cursor :=
                                Best_Available (Manager.Spotters,
                                                Asset_Goal.Tile.Position);
            begin
               if not Managed_Asset_List.Has_Element (Asset_Cursor) then
                  return False;
               end if;

               declare
                  use Carthage.Handles.Assets;
                  use Carthage.Handles.Stacks;
                  Managed_Asset : Managed_Asset_Record renames
                                    Manager.Assets (Asset_Cursor);
                  Asset         : constant Asset_Handle :=
                                    Managed_Asset.Asset;
                  Stack_Cursor  : Managed_Stack_List.Cursor :=
                                    Managed_Asset.Stack;
                  Stack         : Stack_Handle :=
                                   Manager.Stacks (Stack_Cursor).Stack;
               begin
                  if Stack.Asset_Count > 1 then
                     declare
                        New_Stack : constant Stack_Handle :=
                                      Handles.Stacks.Create.New_Ground_Stack
                                        (Manager, Stack.Owner, Stack.Planet,
                                         Stack.Current_Tile);
                     begin
                        Stack.Remove_Asset (Asset.Reference);
                        New_Stack.Add_Asset (Asset.Reference);
                        Manager.Stacks.Append
                          (Managed_Stack_Record'
                             (Stack => New_Stack,
                              Goal  => <>));
                        Stack_Cursor := Manager.Stacks.Last;
                        Manager.Assets (Asset_Cursor).Stack := Stack_Cursor;
                        Manager.Stack_Maps.Insert (New_Stack.Identifier,
                                                   Stack_Cursor);
                        Stack := New_Stack;
                     end;
                  end if;

                  Asset.Log
                    ("assigned to recon of "
                     & Asset_Goal.Tile.Description);
                  Manager.Stacks (Stack_Cursor).Goal.Replace_Element
                    (Asset_Goal);
                  Stack.Move_To_Tile (Asset_Goal.Tile);
                  Carthage.Handles.Stacks.Updates.Start_Update
                    (Stack, Manager);
                  return True;
               end;
            end;

         when Capture =>
            declare
               Closest_Stack : Managed_Stack_List.Cursor :=
                                 Managed_Stack_List.No_Element;
               Smallest_D    : Natural := Natural'Last;
               Current_Str   : Natural := 0;
               Required_Str  : constant Natural :=
                                 Asset_Goal.Parameters.Strength;
            begin
               Manager.Log ("checking capture goal; required strength"
                            & Natural'Image (Required_Str));

               for Position in Manager.Stacks.Iterate loop
                  declare
                     Stack : constant Carthage.Handles.Stacks.Stack_Handle :=
                               Manager.Stacks (Position).Stack;
                     Str   : constant Natural :=
                               Carthage.Handles.Stacks.Total_Strength (Stack);
                     D     : constant Natural :=
                               (if Stack.Current_Tile.Has_Element
                                then Manager.Planet.Hex_Distance
                                  (Stack.Current_Tile.Position,
                                   Asset_Goal.Tile.Position)
                                else Natural'Last);
                  begin
                     if not Stack.Current_Tile.Has_Element then
                        Manager.Log ("no tile: " & Stack.Description);
                     elsif Manager.Stacks (Position).Goal.Is_Empty then
                        if (D < Smallest_D
                            and then Str >= Required_Str)
                          or else (D / 2 < Smallest_D
                                   and then Str >= Required_Str
                                   and then Str * 2 / 3 > Current_Str)
                        then
                           Manager.Log
                             ("strength" & Str'Image & "; distance" & D'Image);
                           declare
                              Path : constant Array_Of_Positions :=
                                       Carthage.Handles.Stacks.Find_Path
                                         (Stack, Asset_Goal.Tile);
                           begin
                              if Carthage.Handles.Stacks.Has_Property
                                (Stack, Carthage.Handles.Stacks.Defender)
                              then
                                 Manager.Log ("skipping defender");
                              elsif Path'Length > 1 then
                                 Closest_Stack := Position;
                                 Smallest_D := D;
                                 Current_Str := Str;
                              else
                                 Manager.Log ("no path");
                              end if;
                           end;
                        end if;
                     end if;
                  end;
               end loop;

               if Managed_Stack_List.Has_Element (Closest_Stack) then
                  declare
                     Stack : constant Carthage.Handles.Stacks.Stack_Handle :=
                               Managed_Stack_List.Element
                                 (Closest_Stack).Stack;
                  begin
                     Manager.Log
                       (Manager.Description
                        & ": assigned to capture of "
                        & Asset_Goal.Tile.Description);
                     for I in 1 .. Stack.Asset_Count loop
                        declare
                           use Carthage.Handles.Assets;
                           Asset : constant Asset_Handle :=
                                     Get (Stack.Asset (I));
                        begin
                           Manager.Log ("asset: " & Asset.Unit.Tag
                                        & " move"
                                        & Asset.Movement'Image);
                        end;
                     end loop;

                     Manager.Stacks (Closest_Stack).Goal.Replace_Element
                       (Asset_Goal);
                     Carthage.Handles.Stacks.Move_To_Tile
                       (Manager.Stacks (Closest_Stack).Stack,
                        Asset_Goal.Tile);
                     Carthage.Handles.Stacks.Updates.Start_Update
                       (Stack, Manager);
                  end;
                  return True;
               else
                  return False;
               end if;
            end;

         when Transfer =>

            declare
               Stack : constant Carthage.Handles.Stacks.Stack_Handle :=
                         Carthage.Handles.Stacks.Create.New_Ground_Stack
                           (Manager => Manager,
                            Owner   => Asset_Goal.City_1.Owner,
                            Planet  => Manager.Planet.Reference,
                            Tile    =>
                              Carthage.Handles.Tiles.Get
                                (Asset_Goal.City_1.Tile));
               Asset : constant Carthage.Handles.Assets.Asset_Handle :=
                 Carthage.Handles.Assets.Create.New_Asset
                   (Unit    => Carthage.Handles.Units.Cargo_Pod,
                    Owner   => Asset_Goal.City_1.Owner,
                    Stack   => Stack.Reference);
               Stack_Cursor : Managed_Stack_List.Cursor;
            begin
               Manager.Stacks.Append
                 (Managed_Stack_Record'
                    (Stack => Stack,
                     Goal  => <>));
               Stack_Cursor := Manager.Stacks.Last;

               Manager.Assets.Append
                 (Managed_Asset_Record'
                    (Asset  => Asset,
                     Stack  => Stack_Cursor,
                     Planet => Manager.Planet,
                     Tile   =>
                       Carthage.Handles.Tiles.Get
                         (Asset_Goal.City_1.Tile),
                     Goal   =>
                       Asset_Manager_Goal_Holders.To_Holder (Asset_Goal)));
               Manager.Stack_Maps.Insert
                 (Stack.Identifier, Stack_Cursor);

               Asset.Log
                  ("assigned to transfer resources"
                  & " from "
                  & Carthage.Handles.Cities.Description (Asset_Goal.City_1)
                  & " to "
                  & Carthage.Handles.Cities.Description (Asset_Goal.City_2));

               declare
                  procedure Transfer_Item
                    (Resource : Carthage.Handles.Resources.Resource_Handle;
                     Quantity : Positive);

                  -------------------
                  -- Transfer_Item --
                  -------------------

                  procedure Transfer_Item
                    (Resource : Carthage.Handles.Resources.Resource_Handle;
                     Quantity : Positive)
                  is
                     Available_Quantity : constant Natural :=
                                            Asset_Goal.City_1.Whole_Quantity
                                              (Resource);
                     Transfer_Quantity : constant Natural :=
                                           Natural'Min
                                              (Available_Quantity,
                                               Quantity);
                  begin
                     Asset.Log
                       (Resource.Tag & ":" & Quantity'Image
                        & Available_Quantity'Image
                        & Transfer_Quantity'Image);
                     if Transfer_Quantity > 0 then
                        Asset_Goal.City_1.Remove (Resource, Transfer_Quantity);
                        Carthage.Handles.Assets.Add
                          (This     => Asset,
                           Resource => Resource,
                           Quantity => Transfer_Quantity);
                     end if;
                  end Transfer_Item;

               begin
                  Asset_Goal.Stock.Scan_Stock (Transfer_Item'Access);
               end;

               Manager.Stacks (Stack_Cursor).Goal.Replace_Element
                 (Asset_Goal);
               Carthage.Handles.Stacks.Move_To_Tile
                 (This => Stack,
                  Tile => Carthage.Handles.Tiles.Get (Asset_Goal.City_2.Tile));
               Carthage.Handles.Stacks.Updates.Start_Update (Stack, Manager);
               return True;
            end;

      end case;
   end Check_Goal;

   ----------------
   -- Check_Goal --
   ----------------

   overriding function Check_Goal
     (Manager : not null access Space_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is
      Asset_Goal : Asset_Manager_Goal renames Asset_Manager_Goal (Goal);

      function Best_Available
        (Target : Carthage.Handles.Planets.Planet_Handle)
         return Managed_Asset_List.Cursor;

      --------------------
      -- Best_Available --
      --------------------

      function Best_Available
        (Target : Carthage.Handles.Planets.Planet_Handle)
         return Managed_Asset_List.Cursor
      is
         Best_Asset    : Managed_Asset_List.Cursor :=
                           Managed_Asset_List.No_Element;
         Best_Distance : Natural := Natural'Last;

      begin
         for Cursor in Manager.Assets.Iterate loop
            declare
               Managed_Asset : Managed_Asset_Record renames
                                 Manager.Assets (Cursor);
            begin
               if Managed_Asset.Goal.Is_Empty then
                  declare
                     use Carthage.Handles.Assets;
                     use Carthage.Handles.Planets;
                     use Carthage.Handles.Stacks;
                     Asset   : constant Asset_Handle := Managed_Asset.Asset;
                     Stack   : constant Stack_Handle := Get (Asset.Stack);
                     Current : constant Planet_Handle := Get (Stack.Planet);
                     Launch_Cost : constant Natural :=
                                     (if Stack.Is_Orbital
                                      then 0 else 1);
                     Distance : constant Natural :=
                                  Carthage.Handles.Galaxy.Jump_Count
                                    (Current, Target)
                                    + Launch_Cost;
                  begin
                     if not Managed_Asset_List.Has_Element (Best_Asset)
                       or else Distance < Best_Distance
                     then
                        Best_Asset := Cursor;
                        Best_Distance := Distance;
                     end if;
                  end;
               end if;
            end;
         end loop;
         return Best_Asset;
      end Best_Available;

   begin
      case Asset_Goal.Class is
         when None =>
            return True;
         when Recon =>
            declare
               Asset_Cursor : constant Managed_Asset_List.Cursor :=
                                Best_Available (Asset_Goal.Planet);
            begin
               if not Managed_Asset_List.Has_Element (Asset_Cursor) then
                  return False;
               end if;

               declare
                  use Carthage.Handles.Assets;
                  use Carthage.Handles.Planets;
                  use Carthage.Handles.Stacks;
                  Asset   : constant Asset_Handle :=
                              Managed_Asset_List.Element (Asset_Cursor)
                              .Asset;
                  Stack   : constant Stack_Handle := Get (Asset.Stack);
                  Planet  : constant Planet_Handle := Get (Stack.Planet);

                  procedure Set_Goal
                    (Item : in out Managed_Asset_Record);

                  --------------
                  -- Set_Goal --
                  --------------

                  procedure Set_Goal
                    (Item : in out Managed_Asset_Record)
                  is
                  begin
                     Item.Goal.Replace_Element (Asset_Goal);
                  end Set_Goal;

               begin
                  Asset.Log
                    ("assigned to recon of " & Asset_Goal.Planet.Tag);
                  Manager.Assets.Update_Element
                    (Asset_Cursor,
                     Set_Goal'Access);
                  Carthage.Handles.Assets.Moves.Start_Jump
                    (Asset, Planet, Asset_Goal.Planet);
                  return True;
               end;
            end;

         when Capture =>
            return True;

         when Transfer =>
            return True;

      end case;
   end Check_Goal;

   -------------------------------
   -- Get_Resource_Requirements --
   -------------------------------

   overriding procedure Get_Resource_Requirements
     (Manager : in out Root_Asset_Manager_Record;
      Minimum : in out Resource_Stock'Class;
      Desired : in out Resource_Stock'Class)
   is
      Food : constant Carthage.Handles.Resources.Resource_Handle :=
               Carthage.Handles.Resources.Food;
      Minimum_Food : Resource_Quantity := 0.0;
      Desired_Food : Resource_Quantity;
   begin

      for Managed_Asset of Manager.Assets loop
         Minimum_Food := Minimum_Food
           + Resource_Quantity (Managed_Asset.Asset.Unit.Eat);
      end loop;

      Desired_Food := Minimum_Food * 2.0;

      Minimum.Add (Food, Minimum_Food / 20.0);
      Desired.Add (Food, Desired_Food / 20.0);

   end Get_Resource_Requirements;

   --------------------------
   -- Ground_Asset_Manager --
   --------------------------

   function Ground_Asset_Manager
     (Meta_Manager : Stack_Meta_Manager_Access;
      House        : Carthage.Handles.Houses.House_Handle;
      Planet       : Carthage.Handles.Planets.Planet_Handle)
      return Manager_Type
   is
      Manager : constant Ground_Asset_Manager_Type :=
                  new Ground_Asset_Manager_Record;
   begin
      Manager.Meta_Manager := Meta_Manager;
      Manager.House := House;
      Manager.Planet := Planet;
      Manager.Initialize;

      --  for Stack of Manager.Stacks loop
      --     declare
      --     S : constant Carthage.Handles.Stacks.Stack_Handle := Stack.Stack;
      --     begin
      --        S.Update_Stack.Set_Manager (Manager.Name).Done;
      --     end;
      --  end loop;

      Add_Manager (Manager);

      return Manager_Type (Manager);
   end Ground_Asset_Manager;

   -----------------------------
   -- Have_Immediate_Capacity --
   -----------------------------

   overriding function Have_Immediate_Capacity
     (Manager : Ground_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is
      Asset_Goal : Asset_Manager_Goal renames Asset_Manager_Goal (Goal);

      function Have_Capacity
        (List : Asset_Classification_List.List)
         return Boolean;

      -------------------
      -- Have_Capacity --
      -------------------

      function Have_Capacity
        (List : Asset_Classification_List.List)
         return Boolean
      is
      begin
         for Position of List loop
            declare
               Managed_Asset : Managed_Asset_Record renames
                                 Manager.Assets (Position);
               Managed_Stack : Managed_Stack_Record renames
                                 Manager.Stacks (Managed_Asset.Stack);
            begin
               Manager.Log ("checking: " & Managed_Asset.Asset.Long_Name);
               if not Managed_Stack.Goal.Is_Empty then
                  Manager.Log ("  already has goal: "
                               & Managed_Stack.Goal.Element.Show);
               end if;
               if not Managed_Asset.Asset.Can_Enter
                 (Manager.Planet.World, Asset_Goal.Tile.Reference)
               then
                  Manager.Log ("cannot enter tile "
                               & Asset_Goal.Tile.Description);
               end if;

               if Managed_Stack.Goal.Is_Empty
                 and then Managed_Asset.Asset.Can_Enter
                   (Manager.Planet.World, Asset_Goal.Tile.Reference)
               then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Have_Capacity;

   begin
      case Asset_Goal.Class is
         when None =>
            return True;
         when Recon =>
            return Have_Capacity (Manager.Spotters);
         when Capture =>
            return False;
         when Transfer =>
            return True;
      end case;
   end Have_Immediate_Capacity;

   -----------------------------
   -- Have_Immediate_Capacity --
   -----------------------------

   overriding function Have_Immediate_Capacity
     (Manager : Space_Asset_Manager_Record;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is
      Asset_Goal : Asset_Manager_Goal renames Asset_Manager_Goal (Goal);

   begin
      case Asset_Goal.Class is
         when None =>
            return True;
         when Recon =>
            for Managed_Asset of Manager.Assets loop
               if Managed_Asset.Goal.Is_Empty then
                  return True;
               end if;
            end loop;
            return False;
         when Capture =>
            return False;
         when Transfer =>
            return False;
      end case;
   end Have_Immediate_Capacity;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Manager : not null access Root_Asset_Manager_Record)
   is

      procedure Add_Asset_Classification
        (Asset_Cursor : Managed_Asset_List.Cursor;
         To_List      : in out Asset_Classification_List.List;
         Rate         : not null access
           function (Asset : Carthage.Handles.Assets.Asset_Handle)
         return Natural);

      ------------------------------
      -- Add_Asset_Classification --
      ------------------------------

      procedure Add_Asset_Classification
        (Asset_Cursor : Managed_Asset_List.Cursor;
         To_List      : in out Asset_Classification_List.List;
         Rate         : not null access
           function (Asset : Carthage.Handles.Assets.Asset_Handle)
         return Natural)
      is
         New_Asset      : constant Carthage.Handles.Assets.Asset_Handle :=
           Managed_Asset_List.Element
             (Asset_Cursor).Asset;
         New_Rating     : constant Natural :=
           Rate (New_Asset);
      begin
         if New_Rating = 0 then
            null;
         elsif To_List.Is_Empty then
            To_List.Append (Asset_Cursor);
         else
            declare
               Current_First  : constant Handles.Assets.Asset_Handle :=
                                  Managed_Asset_List.Element
                                    (To_List.First_Element).Asset;
               Current_Rating : constant Natural :=
                                  Rate (Current_First);
            begin
               if New_Rating = Current_Rating then
                  To_List.Append (Asset_Cursor);
               elsif New_Rating > Current_Rating then
                  To_List.Clear;
                  To_List.Append (Asset_Cursor);
               end if;
            end;
         end if;
      end Add_Asset_Classification;

   begin

      Root_Asset_Manager_Record'Class (Manager.all).Load_Assets;

      declare
         use Carthage.Handles.Assets;
         function Rate_Movement (Asset : Asset_Handle) return Natural
         is (if Is_Ground_Asset (Asset) then Asset.Movement else 0);

         function Rate_Spot (Asset : Asset_Handle) return Natural
         is (if Is_Ground_Asset (Asset) then Asset.Unit.Spot else 0);

         function Rate_Ground_Cargo (Asset : Asset_Handle) return Natural
         is (if Is_Ground_Asset (Asset)
             then Asset.Unit.Cargo_Capacity
             else 0);

      begin
         for Position in Manager.Assets.Iterate loop
            Add_Asset_Classification
              (Position, Manager.Movers,
               Rate_Movement'Access);
            Add_Asset_Classification
              (Position, Manager.Spotters,
               Rate_Spot'Access);
            Add_Asset_Classification
              (Position, Manager.Ground_Cargo,
               Rate_Ground_Cargo'Access);
         end loop;

         for Spotter of Manager.Spotters loop
            Manager.Idle_Spotters.Include (Spotter.Asset.Identifier);
         end loop;

      end;

   end Initialize;

   -----------------
   -- Load_Assets --
   -----------------

   overriding procedure Load_Assets
     (Manager : in out Ground_Asset_Manager_Record)
   is

      procedure Add_Stack
        (Reference : Carthage.Handles.Stack_Reference);

      ---------------
      -- Add_Stack --
      ---------------

      procedure Add_Stack
        (Reference : Carthage.Handles.Stack_Reference)
      is
         Stack : constant Carthage.Handles.Stacks.Stack_Handle :=
                   Carthage.Handles.Stacks.Get (Reference);
         Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                    Carthage.Handles.Planets.Get (Stack.Planet);
      begin
         Manager.Stacks.Append
           (Managed_Stack_Record'
              (Stack        => Stack,
               Goal         => <>));

         Manager.Stack_Maps.Insert
           (Stack.Identifier, Manager.Stacks.Last);

         for Index in 1 .. Stack.Asset_Count loop
            declare
               Asset : constant Carthage.Handles.Assets.Asset_Handle :=
                         Carthage.Handles.Assets.Get
                           (Stack.Asset (Index));
            begin
               Manager.Assets.Append
                 (Managed_Asset_Record'
                    (Asset  => Asset,
                     Stack  => Manager.Stacks.Last,
                     Planet => Planet,
                     Tile   => Stack.Current_Tile,
                     Goal   => <>));

            --  if not Stack.Has_Property (Carthage.Handles.Stacks.Defender)
            --    and then (Stack.Asset (I).Unit.Is_Sceptre
            --              or else Stack.Asset (I).Unit.Is_Noble)
            --  then
            --  Stack.Update.Set_Property (Carthage.Handles.Stacks.Defender);
            --  end if;
            end;
         end loop;
      end Add_Stack;

   begin

      Manager.Planet.For_All_Owned_Stacks (Manager.House.Reference,
                                           Add_Stack'Access);

   end Load_Assets;

   -----------------
   -- Load_Assets --
   -----------------

   overriding procedure Load_Assets
     (Manager : in out Space_Asset_Manager_Record)
   is

      procedure Check_Asset (Asset : Carthage.Handles.Assets.Asset_Handle);

      -----------------
      -- Check_Asset --
      -----------------

      procedure Check_Asset (Asset : Carthage.Handles.Assets.Asset_Handle) is
         use Carthage.Handles.Stacks;
         use Carthage.Handles.Planets;
      begin
         if Asset.Is_Space_Asset then
            declare
               Stack : constant Stack_Handle := Get (Asset.Stack);
            begin
               Manager.Assets.Append
                 (Managed_Asset_Record'
                    (Asset  => Asset,
                     Stack  => Managed_Stack_List.No_Element,
                     Planet => Get (Stack.Planet),
                     Tile   => Stack.Current_Tile,
                     Goal   => <>));
            end;
         end if;
      end Check_Asset;

   begin
      Carthage.Handles.Assets.For_All_Owned_Assets
        (Manager.House.Reference, Check_Asset'Access);
   end Load_Assets;

   ------------------------
   -- On_Hostile_Spotted --
   ------------------------

   overriding procedure On_Hostile_Spotted
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : Carthage.Handles.Stacks.Stack_Handle'Class;
      Hostile : Carthage.Handles.Stacks.Stack_Handle'Class;
      Stop    : out Boolean)
   is
      use Carthage.Handles.Stacks;
   begin
      Manager.Log
        (Stack.Identifier & " spotted hostile "
         & Carthage.Handles.Stacks.Description (Hostile)
         & " at "
         & Hostile.Current_Tile.Description);
      Stop := False;

      for S of Manager.Stacks loop
         if S.Stack.Identifier = Stack.Identifier then
            if not S.Goal.Is_Empty
              and then S.Goal.Element.Class /= Capture
            then
               Manager.Log ("stopping because goal class is "
                            & Goal_Class'Image (S.Goal.Element.Class));
               S.Goal.Clear;
               Stop := True;
            end if;
            exit;
         end if;
      end loop;

      Manager.Meta_Manager.On_Hostile_Spotted (Stack, Hostile);
   end On_Hostile_Spotted;

   -----------------------
   -- On_Movement_Ended --
   -----------------------

   overriding procedure On_Movement_Ended
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : Carthage.Handles.Stacks.Stack_Handle'Class)
   is
      Position : constant Managed_Stack_List.Cursor :=
        Manager.Stack_Maps.Element (Stack.Identifier);
      Rec      : constant Managed_Stack_Record := Manager.Stacks (Position);
   begin
      if Rec.Goal.Is_Empty then
         return;
      end if;

      declare
         Goal     : constant Asset_Manager_Goal :=
                      Asset_Manager_Goal (Rec.Goal.Element);

         procedure Log (Message : String);

         ---------
         -- Log --
         ---------

         procedure Log (Message : String) is
         begin
            Manager.Log (Carthage.Handles.Stacks.Description (Stack)
                         & ": " & Message);
         end Log;
      begin
         case Goal.Class is
            when None =>
               Log ("movement ended with no goal");
            when Recon =>
               Log ("recon ended");
            when Capture =>
               Log ("capture movement ended");
            when Transfer =>
               Log ("transfer movement ended");

               declare
                  procedure Deliver_Resource
                    (Resource : Carthage.Handles.Resources.Resource_Handle;
                     Quantity : Positive);

                  ---------------------
                  -- Report_Resource --
                  ---------------------

                  procedure Deliver_Resource
                    (Resource : Carthage.Handles.Resources.Resource_Handle;
                     Quantity : Positive)
                  is
                  begin
                     Log ("deliver" & Quantity'Image
                          & " " & Resource.Tag
                          & " to "
                          & Carthage.Handles.Cities.Description (Goal.City_2));
                     Goal.City_2.Add
                       (Item     => Resource,
                        Quantity =>
                          Natural'Min
                            (Quantity, Stack.Whole_Quantity (Resource)));
                  end Deliver_Resource;

               begin
                  Goal.Stock.Scan_Stock (Deliver_Resource'Access);
               end;

               Log ("deleting stack");
               Ground_Asset_Manager_Record'Class (Manager)
                 .On_Stack_Removed (Stack);
               Carthage.Handles.Stacks.Delete_Stack (Stack);
         end case;
      end;
   end On_Movement_Ended;

   ----------------------
   -- On_Stack_Removed --
   ----------------------

   overriding procedure On_Stack_Removed
     (Manager : in out Ground_Asset_Manager_Record;
      Stack   : Carthage.Handles.Stacks.Stack_Handle'Class)
   is
      Position : Managed_Stack_List.Cursor :=
                   Manager.Stack_Maps.Element (Stack.Identifier);
   begin
      Manager.Stacks.Delete (Position);
      Manager.Stack_Maps.Delete (Stack.Identifier);
   end On_Stack_Removed;

   --------------------------------
   -- Planet_Reconnaissance_Goal --
   --------------------------------

   function Planet_Reconnaissance_Goal
     (Planet : Carthage.Handles.Planets.Planet_Handle)
      return Carthage.Goals.Goal_Record'Class
   is
      Goal : constant Asset_Manager_Goal :=
               Asset_Manager_Goal'
                 (Carthage.Goals.Goal_Record with
                  Priority   => Default_Priority (Recon),
                  Class      => Recon,
                  Planet     => Planet,
                  Parameters => (Speed => High, Spot => High, others => <>),
                  others     => <>);
   begin
      return Goal;
   end Planet_Reconnaissance_Goal;

   -------------------------
   -- Space_Asset_Manager --
   -------------------------

   function Space_Asset_Manager
     (Meta_Manager : Stack_Meta_Manager_Access;
      House        : Carthage.Handles.Houses.House_Handle)
      return Manager_Type
   is
      Manager : constant Space_Asset_Manager_Type :=
                  new Space_Asset_Manager_Record;
   begin
      Manager.Meta_Manager := Meta_Manager;
      Manager.House := House;
      Manager.Initialize;

      Add_Manager (Manager);

      return Manager_Type (Manager);
   end Space_Asset_Manager;

   -------------------
   -- Take_Resource --
   -------------------

   overriding procedure Take_Resource
     (From     : in out Root_Asset_Manager_Record;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : in out Resource_Quantity)
   is
   begin
      Quantity :=
        Resource_Quantity'Min
          (Quantity, From.Resources.Quantity (Resource));
      From.Resources.Remove (Resource, Quantity);
   end Take_Resource;

   -----------------------
   -- Tile_Capture_Goal --
   -----------------------

   function Tile_Capture_Goal
     (Tile     : Carthage.Handles.Tiles.Tile_Handle;
      Strength : Natural)
      return Carthage.Goals.Goal_Record'Class
   is
      Goal : constant Asset_Manager_Goal :=
               Asset_Manager_Goal'
                 (Carthage.Goals.Goal_Record with
                  Priority   => Default_Priority (Capture),
                  Class      => Capture,
                  Planet     => <>,
                  Tile       => Tile,
                  City_1     => <>,
                  City_2     => <>,
                  Stock      => <>,
                  Parameters =>
                    (Speed    => Low, Spot => Low, Cargo => Low,
                     Military => High, Strength => Strength));
   begin
      return Goal;
   end Tile_Capture_Goal;

   ------------------------------
   -- Tile_Reconnaissance_Goal --
   ------------------------------

   function Tile_Reconnaissance_Goal
     (Tile    : Carthage.Handles.Tiles.Tile_Handle)
      return Carthage.Goals.Goal_Record'Class
   is
      Goal : constant Asset_Manager_Goal :=
               Asset_Manager_Goal'
                 (Carthage.Goals.Goal_Record with
                  Priority   => Default_Priority (Recon),
                  Class      => Recon,
                  Planet     => <>,
                  Tile       => Tile,
                  City_1     => <>,
                  City_2     => <>,
                  Stock      => <>,
                  Parameters => (Speed => High, Spot => High, others => <>));
   begin
      return Goal;
   end Tile_Reconnaissance_Goal;

   -------------------------
   -- Transfer_Cargo_Goal --
   -------------------------

   function Transfer_Cargo_Goal
     (From, To : Carthage.Handles.Cities.City_Handle;
      Cargo    : Resource_Stock'Class)
      return Carthage.Goals.Goal_Record'Class
   is
      Goal : constant Asset_Manager_Goal :=
        Asset_Manager_Goal'
          (Carthage.Goals.Goal_Record with
           Priority   => Default_Priority (Transfer),
           Class      => Transfer,
           Planet     => <>,
           Tile       => Carthage.Handles.Tiles.Get (From.Tile),
           City_1     => From,
           City_2     => To,
           Stock      => Resource_Stock (Cargo),
           Parameters => (Cargo => High, Speed => Medium, others => <>));
   begin
      return Goal;
   end Transfer_Cargo_Goal;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Manager : not null access Root_Asset_Manager_Record)
      return Duration
   is
   begin
      Root_Asset_Manager_Record'Class (Manager.all).Check_Goals;
      return Root_Asset_Manager_Record'Class (Manager.all)
        .Average_Update_Frequency;
   end Update;

end Carthage.Managers.Assets;
