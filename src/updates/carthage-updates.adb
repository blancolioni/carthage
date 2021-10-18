with Ada.Calendar;
with Ada.Containers.Indefinite_Holders;

with WL.Heaps;

with Hira.City;

with Carthage.Cities;
with Carthage.Houses;
with Carthage.Planets;
with Carthage.Stacks;
--  with Carthage.Structures;
with Carthage.Tiles;

package body Carthage.Updates is

   Current_Time_Acceleration : Duration := 0.0;
   Last_Update               : Ada.Calendar.Time;

   package Update_Holders is
      new Ada.Containers.Indefinite_Holders (Update_Interface'Class);

   package Update_Queues is
     new WL.Heaps
       (Carthage.Calendar.Time, Update_Holders.Holder,
        Carthage.Calendar.">", Update_Holders."=");

   Update_Queue              : Update_Queues.Heap;

   -------------------------
   -- Before_First_Update --
   -------------------------

   procedure Before_First_Update is

      procedure Reset_Planet_State
        (Planet : Carthage.Planets.Planet_Class);

      procedure Add_Planet_Maps (House : Carthage.Houses.House_Class);

      procedure City_Look
        (City : Carthage.Cities.City_Class);

      procedure Find_Agora
        (City : Carthage.Cities.City_Class);

      procedure Stack_Look
        (Stack : Carthage.Stacks.Stack_Class);

      procedure Set_Planet_Owner
        (Palace : Carthage.Cities.City_Class);

      ---------------------
      -- Add_Planet_Maps --
      ---------------------

      procedure Add_Planet_Maps (House : Carthage.Houses.House_Class) is

         procedure Add_Planet (Planet : Carthage.Planets.Planet_Class);

         ----------------
         -- Add_Planet --
         ----------------

         procedure Add_Planet (Planet : Carthage.Planets.Planet_Class) is
         begin
            Carthage.Planets.Set_Explored_By (Planet, House);
         end Add_Planet;

      begin
         Carthage.Houses.Scan_Known_Planets (House, Add_Planet'Access);
      end Add_Planet_Maps;

      ---------------
      -- City_Look --
      ---------------

      procedure City_Look
        (City : Carthage.Cities.City_Class)
      is
         Tiles : Carthage.Planets.Surface_Tiles;
      begin
         Carthage.Planets.Get_Tiles
           (This => City.Planet,
            Origin => City.Tile,
            Min_Distance => 0,
            Max_Distance => 6,
            Test         => null,
            Tiles        => Tiles);

         for I in 1 .. Carthage.Planets.Tile_Count (Tiles) loop
            declare
               Tile : constant Carthage.Tiles.Tile_Class :=
                        Carthage.Planets.Get_Tile (Tiles, I);
            begin
               Carthage.Tiles.Set_Currently_Visible_To
                 (Tile, City.Owner);
            end;
         end loop;

      end City_Look;

      ----------------
      -- Find_Agora --
      ----------------

      procedure Find_Agora
        (City : Carthage.Cities.City_Class)
      is

         Minimum_Distance : Natural := Natural'Last;
         Closest_Agora    : Carthage.Cities.City_Handle :=
                                 Hira.City.Empty_Handle;

         procedure Check (Check_City : Carthage.Cities.City_Class);

         -----------
         -- Check --
         -----------

         procedure Check
           (Check_City : Carthage.Cities.City_Class)
         is
            D : constant Natural :=
                  Carthage.Planets.Hex_Distance
                    (This => City.Planet,
                     From => Carthage.Tiles.Position (City.Tile),
                     To   => Carthage.Tiles.Position (Check_City.Tile));
         begin
            if Check_City.Structure.Agora
              and then not Carthage.Houses.At_War_With
                (City.Owner, Check_City.Owner)
              and then D < Minimum_Distance
            then
               Closest_Agora := Check_City.To_City_Handle;
               Minimum_Distance := D;
            end if;
         end Check;

      begin

         if not Carthage.Cities.Is_Agora (City) then
            for Check_City of Hira.City.Select_By_Planet (City.Planet) loop
               Check (Check_City);
            end loop;

            if Closest_Agora.Has_Element then
               City.Update_City.Set_Agora (Closest_Agora).Done;
            end if;
         end if;

      end Find_Agora;

      ------------------------
      -- Reset_Planet_State --
      ------------------------

      procedure Reset_Planet_State
        (Planet : Carthage.Planets.Planet_Class)
      is
      begin
         Carthage.Planets.Clear_Visibility (Planet);
      end Reset_Planet_State;

      ----------------------
      -- Set_Planet_Owner --
      ----------------------

      procedure Set_Planet_Owner
        (Palace : Carthage.Cities.City_Class)
      is
      begin
         Palace.Planet.Update_Planet.Set_Owner (Palace.Owner).Done;
      end Set_Planet_Owner;

      ----------------
      -- Stack_Look --
      ----------------

      procedure Stack_Look
        (Stack : Carthage.Stacks.Stack_Class)
      is
         use type Carthage.Stacks.Asset_Count;
         Tiles : Carthage.Planets.Surface_Tiles;
      begin

         if Stack.Has_Tile then
            Stack.Planet.Get_Tiles
              (Origin       => Stack.Tile,
               Min_Distance => 0,
               Max_Distance => 6,
               Test         => null,
               Tiles        => Tiles);

            for I in 1 .. Carthage.Planets.Tile_Count (Tiles) loop
               declare
                  Tile : constant Carthage.Tiles.Any_Reference :=
                           Carthage.Planets.Get_Tile (Tiles, I);
               begin
                  Tile.Update.Set_Currently_Visible_To (Stack.Owner);
               end;
            end loop;
         elsif Stack.Is_Orbiting
           and then Stack.Count > 0
             and then not Stack.Planet.Seen_By (Stack.Owner)
         then
            Stack.Planet.Update.Set_Seen_By (Stack.Owner);
         end if;
      end Stack_Look;

   begin

      Carthage.Planets.Scan (Reset_Planet_State'Access);

      Carthage.Houses.Scan (Add_Planet_Maps'Access);

      Carthage.Cities.Scan_Cities
        (Carthage.Structures.Get ("palace"),
         Set_Planet_Owner'Access);

      Carthage.Cities.Scan_Cities
        (City_Look'Access);

      Carthage.Cities.Scan_Cities
        (Find_Agora'Access);

      Carthage.Stacks.Scan_Stacks
        (Stack_Look'Access);

      Last_Update := Ada.Calendar.Clock;

   end Before_First_Update;

   -----------
   -- Queue --
   -----------

   procedure Queue
     (Item       : Update_Interface'Class;
      Next_Event : Carthage.Calendar.Time)
   is
   begin
      Update_Queue.Insert (Next_Event, Update_Holders.To_Holder (Item));
   end Queue;

   -----------
   -- Queue --
   -----------

   procedure Queue
     (Item        : Update_Interface'Class;
      Event_Delay : Duration)
   is
      use Carthage.Calendar;
   begin
      Queue (Item, Clock + Event_Delay);
   end Queue;

   ---------------------------
   -- Set_Time_Acceleration --
   ---------------------------

   procedure Set_Time_Acceleration (Factor : Duration) is
   begin
      Current_Time_Acceleration := Factor;
   end Set_Time_Acceleration;

   ------------
   -- Update --
   ------------

   procedure Update is
      use Carthage.Calendar;
      use type Ada.Calendar.Time;
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Time_Since_Last_Update : constant Duration :=
                                 Now - Last_Update;
   begin
      Advance (Time_Since_Last_Update * Current_Time_Acceleration);
      Last_Update := Now;

      while Update_Queue.First_Key <= Clock loop
         declare
            Update : constant Update_Interface'Class :=
                       Update_Queue.First_Element.Element;
         begin
            Update_Queue.Delete_First;
            Update.Activate;
         end;
      end loop;

   end Update;

--
--        procedure Execute_Round
--          (Battle : in out Carthage.Combat.Battle_Record);
--
--        -------------------
--        -- Execute_Round --
--        -------------------
--
--        procedure Execute_Round
--          (Battle : in out Carthage.Combat.Battle_Record)
--        is
--           use Carthage.Combat;
--        begin
--           Attacker (Battle).Log ("attacking " & Defender (Battle).Name);
--           for Weapon in Carthage.Units.Weapon_Category loop
--              declare
--                 Round : constant Carthage.Combat.Attack_Record_Array :=
--                           Carthage.Combat.Attack_Round (Battle, Weapon);
--              begin
--                 for Attack of Round loop
--                    Attacker (Battle).Log (Image (Attack));
--                 end loop;
--              end;
--           end loop;
--        end Execute_Round;
--
--     begin
--        Ada.Text_IO.Put_Line
--          ("Update: "
--           & Carthage.Calendar.Image (Carthage.Calendar.Clock));
--        Carthage.Logging.Log ("starting update");
--        Carthage.Managers.Start_Manager_Turns;
--        Carthage.Managers.Execute_Manager_Turns;
--        Carthage.Cities.Updates.Execute_Orders;
--        Carthage.Cities.Updates.Execute_Production;
--        Carthage.Stacks.Updates.Execute_Orders;
--        Carthage.Houses.Scan (Carthage.Houses.Log_Status'Access);
--        Carthage.Combat.Scan_Battles (Execute_Round'Access);
--        Carthage.Stacks.Remove_Empty_Ground_Stacks;
--        Carthage.Logging.Log ("update complete");
--        Carthage.Calendar.Advance (Carthage.Calendar.Days (1));
--     end Update;

end Carthage.Updates;
