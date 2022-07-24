--  with Ada.Calendar;
--  with Ada.Containers.Indefinite_Holders;
with Ada.Text_IO;

--  with WL.Heaps;
with WL.String_Sets;

with Carthage.Handles.Cities;
with Carthage.Handles.Houses;
with Carthage.Handles.Planets;
with Carthage.Handles.Resources;
with Carthage.Handles.Stacks;
with Carthage.Handles.Structures;
with Carthage.Handles.Tiles;

with Carthage.Handles.Cities.Updates;
with Carthage.Handles.Houses.Updates;
with Carthage.Handles.Stacks.Updates;

with Carthage.Managers.Resources;

package body Carthage.Updates is

   -------------------------
   -- Before_First_Update --
   -------------------------

   procedure Before_First_Update is

      procedure Reset_Planet_State
        (Planet : Carthage.Handles.Planets.Planet_Handle);

      procedure Add_Planet_Maps (House : Carthage.Handles.Houses.House_Handle);

      procedure City_Look
        (City : Carthage.Handles.Cities.City_Handle);

      procedure Find_Agora
        (City : Carthage.Handles.Cities.City_Handle);

      procedure Stack_Look
        (Stack : Carthage.Handles.Stacks.Stack_Handle);

      procedure Set_Planet_Owner
        (Palace : Carthage.Handles.Cities.City_Handle);

      procedure Add_Planet_Managers
        (House  : Carthage.Handles.Houses.House_Handle);

      -------------------------
      -- Add_Planet_Managers --
      -------------------------

      procedure Add_Planet_Managers
        (House  : Carthage.Handles.Houses.House_Handle)
      is
         Processed_Planets : WL.String_Sets.Set;

         procedure Check_City_Planet
           (City : Carthage.Handles.Cities.City_Handle);

         procedure Check_Stack_Planet
           (Stack : Carthage.Handles.Stacks.Stack_Handle);

         -----------------------
         -- Check_City_Planet --
         -----------------------

         procedure Check_City_Planet
           (City : Carthage.Handles.Cities.City_Handle)
         is
         begin
            if City.Owner.Tag = House.Tag then
               declare
                  use Carthage.Handles.Planets;
                  Planet : constant Planet_Handle := Get (City.Planet);
               begin
                  if not Processed_Planets.Contains (Planet.Tag) then
                     Carthage.Managers.Resources.Create_Planet_Resource_Manager
                       (House  => House,
                        Planet => Planet);
                     Processed_Planets.Include (Planet.Tag);
                  end if;
               end;
            end if;
         end Check_City_Planet;

         ------------------------
         -- Check_Stack_Planet --
         ------------------------

         procedure Check_Stack_Planet
           (Stack : Carthage.Handles.Stacks.Stack_Handle)
         is
         begin
            if Stack.Has_Assets and then Stack.Owner.Tag = House.Tag then
               declare
                  use Carthage.Handles.Planets;
                  Planet : constant Planet_Handle := Get (Stack.Planet);
               begin
                  if not Processed_Planets.Contains (Planet.Tag) then
                     Carthage.Managers.Resources.Create_Planet_Resource_Manager
                       (House  => House,
                        Planet => Planet);
                     Processed_Planets.Include (Planet.Tag);
                  end if;
               end;
            end if;
         end Check_Stack_Planet;

      begin
         Carthage.Handles.Cities.For_All_Cities (Check_City_Planet'Access);
         Carthage.Handles.Stacks.For_All_Stacks (Check_Stack_Planet'Access);
      end Add_Planet_Managers;

      ---------------------
      -- Add_Planet_Maps --
      ---------------------

      procedure Add_Planet_Maps
        (House : Carthage.Handles.Houses.House_Handle)
      is

         procedure Add_Planet
           (Reference : Carthage.Handles.Planet_Reference);

         ----------------
         -- Add_Planet --
         ----------------

         procedure Add_Planet
           (Reference : Carthage.Handles.Planet_Reference)
         is
            Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                       Carthage.Handles.Planets.Get (Reference);
         begin
            Planet.Set_Explored_By (House.Reference);
         end Add_Planet;

      begin
         House.Scan_Known_Planets (Add_Planet'Access);
      end Add_Planet_Maps;

      ---------------
      -- City_Look --
      ---------------

      procedure City_Look
        (City : Carthage.Handles.Cities.City_Handle)
      is
         Tiles : Carthage.Handles.Planets.Surface_Tiles;
         Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                    Carthage.Handles.Planets.Get (City.Planet);
         Tile   : constant Carthage.Handles.Tiles.Tile_Handle :=
                    Carthage.Handles.Tiles.Get (City.Tile);
      begin
         Carthage.Handles.Planets.Get_Tiles
           (This         => Planet,
            Origin       => Tile,
            Min_Distance => 0,
            Max_Distance => 6,
            Test         => null,
            Tiles        => Tiles);

         for I in 1 .. Carthage.Handles.Planets.Tile_Count (Tiles) loop
            declare
               Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                        Carthage.Handles.Planets.Get_Tile (Tiles, I);
            begin
               Tile.Set_Currently_Visible_To (City.Owner);
            end;
         end loop;

      end City_Look;

      ----------------
      -- Find_Agora --
      ----------------

      procedure Find_Agora
        (City : Carthage.Handles.Cities.City_Handle)
      is

         Minimum_Distance : Natural := Natural'Last;
         Closest_Agora    : Carthage.Handles.Cities.City_Handle :=
                              Carthage.Handles.Cities.Empty_Handle;
         procedure Check
           (Reference : Carthage.Handles.City_Reference);

         -----------
         -- Check --
         -----------

         procedure Check
           (Reference : Carthage.Handles.City_Reference)
         is
            Check_City : constant Carthage.Handles.Cities.City_Handle :=
                           Carthage.Handles.Cities.Get (Reference);
            D : constant Natural :=
                  Carthage.Handles.Planets.Get (City.Planet)
                  .Hex_Distance
                     (From => City.Tile,
                      To   => Check_City.Tile);
         begin
            if Carthage.Handles.Structures.Get (Check_City.Structure).Is_Agora
              and then not City.Owner.At_War_With (Check_City.Owner)
              and then D < Minimum_Distance
            then
               Closest_Agora := Check_City;
               Minimum_Distance := D;
            end if;
         end Check;

      begin

         if not City.Is_Agora then

            Carthage.Handles.Planets.Get (City.Planet)
              .For_All_Cities (Check'Access);

            if Closest_Agora.Has_Element then
               City.Set_Agora (Closest_Agora);
            end if;
         end if;

      end Find_Agora;

      ------------------------
      -- Reset_Planet_State --
      ------------------------

      procedure Reset_Planet_State
        (Planet : Carthage.Handles.Planets.Planet_Handle)
      is
      begin
         Carthage.Handles.Planets.Clear_Visibility (Planet);
      end Reset_Planet_State;

      ----------------------
      -- Set_Planet_Owner --
      ----------------------

      procedure Set_Planet_Owner
        (Palace : Carthage.Handles.Cities.City_Handle)
      is
      begin
         Carthage.Handles.Planets.Get (Palace.Planet)
           .Set_Owner (Palace.Owner.Reference);
      end Set_Planet_Owner;

      ----------------
      -- Stack_Look --
      ----------------

      procedure Stack_Look
        (Stack : Carthage.Handles.Stacks.Stack_Handle)
      is
         Tiles : Carthage.Handles.Planets.Surface_Tiles;
         Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                    Carthage.Handles.Planets.Get (Stack.Planet);
      begin

         if Stack.Is_Ground then
            Planet.Get_Tiles
              (Origin       => Stack.Current_Tile,
               Min_Distance => 0,
               Max_Distance => 6,
               Test         => null,
               Tiles        => Tiles);

            for I in 1 .. Carthage.Handles.Planets.Tile_Count (Tiles) loop
               declare
                  Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                           Carthage.Handles.Planets.Get_Tile (Tiles, I);
               begin
                  Tile.Set_Currently_Visible_To (Stack.Owner);
               end;
            end loop;
         elsif Stack.Is_Orbital
           and then Stack.Has_Assets
             and then not Planet.Seen_By (Stack.Owner.Reference)
         then
            Planet.Set_Seen_By (Stack.Owner.Reference);
         end if;
      end Stack_Look;

   begin

      Ada.Text_IO.Put_Line ("Initializing");

      Carthage.Handles.Planets.For_All_Planets (Reset_Planet_State'Access);

      Carthage.Handles.Houses.For_All_Houses (Add_Planet_Maps'Access);

      Carthage.Handles.Cities.For_All_Cities_With_Structure
        (Carthage.Handles.Structures.Get ("palace").Reference,
         Set_Planet_Owner'Access);

      Carthage.Handles.Cities.For_All_Cities
        (City_Look'Access);

      Carthage.Handles.Cities.For_All_Cities
        (Find_Agora'Access);

      Carthage.Handles.Stacks.For_All_Stacks
        (Stack_Look'Access);

      Carthage.Handles.Houses.For_All_Houses (Add_Planet_Managers'Access);

   end Before_First_Update;

   ------------------
   -- Daily_Update --
   ------------------

   procedure Daily_Update is

      procedure Execute_City_Production
        (Resource : Carthage.Handles.Resources.Resource_Handle);

      -----------------------------
      -- Execute_City_Production --
      -----------------------------

      procedure Execute_City_Production
        (Resource : Carthage.Handles.Resources.Resource_Handle)
      is
      begin
         Carthage.Handles.Cities.For_All_Producers
           (Resource => Resource,
            Process  =>
              Carthage.Handles.Cities.Updates.Execute_City_Production'Access);
      end Execute_City_Production;

   begin
      Carthage.Handles.Cities.For_All_Harvesters
        (Carthage.Handles.Cities.Updates.Execute_Harvester_Production'Access);
      Carthage.Handles.Resources.For_All_Resources
        (Execute_City_Production'Access);
      Carthage.Handles.Cities.For_All_Cities
        (Carthage.Handles.Cities.Updates.Execute_City_Consumption'Access);
      Carthage.Handles.Stacks.For_All_Ground_Stacks
        (Carthage.Handles.Stacks.Updates.Consumption'Access);
      Carthage.Handles.Houses.For_All_Houses
        (Carthage.Handles.Houses.Updates.Finish_Update'Access);
   end Daily_Update;

end Carthage.Updates;
