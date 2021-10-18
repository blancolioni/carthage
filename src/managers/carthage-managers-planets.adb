with Ada.Containers.Doubly_Linked_Lists;

with WL.Random;
with WL.String_Maps;

with Carthage.Calendar;

with Carthage.Handles.Cities;
with Carthage.Handles.Stacks;
with Carthage.Handles.Structures;
with Carthage.Handles.Tiles;

with Carthage.Managers.Assets;
with Carthage.Managers.Cities;

package body Carthage.Managers.Planets is

   package List_Of_Tiles is
     new Ada.Containers.Doubly_Linked_Lists
       (Carthage.Handles.Tiles.Tile_Handle, Carthage.Handles.Tiles."=");

   package Stack_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Carthage.Handles.Stacks.Stack_Handle,
        Carthage.Handles.Stacks."=");

   type Planet_Area_Class is (Sea, Continent);

   type Planet_Area_Record is
      record
         Class           : Planet_Area_Class;
         Tiles           : List_Of_Tiles.List;
         Internal_Border : List_Of_Tiles.List;
         External_Border : List_Of_Tiles.List;
      end record;

   package Planet_Area_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Planet_Area_Record);

   type Tile_Info_Record is
      record
         Tile                 : Carthage.Handles.Tiles.Tile_Handle;
         Nearest_Seen         : Carthage.Handles.Tiles.Tile_Handle;
         Nearest_Explored     : Carthage.Handles.Tiles.Tile_Handle;
         Nearest_Controlled   : Carthage.Handles.Tiles.Tile_Handle;
         Interest             : Integer := 0;
         Continent            : Planet_Area_Lists.Cursor :=
                                  Planet_Area_Lists.No_Element;
         Area_Internal_Border : Boolean;
         Area_External_Border : Boolean;
         Controlled           : Boolean;
         Explored             : Boolean;
         Seen                 : Boolean;
         Targeted             : Boolean;
      end record;

   type Tile_Info_Array is array (Tile_X, Tile_Y) of Tile_Info_Record;

   type Goal_Class is (Explore_Surface);

--     type Planet_Manager_Goal is
--       new Carthage.Goals.Goal_Record with
--        record
--           Class : Goal_Class;
--        end record;
--
--     overriding function Show
--       (Goal : Planet_Manager_Goal)
--        return String
--     is ("planet goal: " & Goal.Class'Image);

   function Default_Priority
     (Class : Goal_Class)
      return Carthage.Goals.Goal_Priority
   is (case Class is
          when Explore_Surface => 20)
        with Unreferenced;

   package Identifier_Sets is
     new WL.String_Maps (Boolean);

   package City_Manager_Maps is
     new WL.String_Maps (Manager_Type);

   type Planet_Manager_Record is
     new Root_Manager_Type with
      record
         Owned                : Boolean;
         Active               : Boolean;
         Planet               : Carthage.Handles.Planets.Planet_Handle;
         City_Managers        : City_Manager_Maps.Map;
         Ground_Asset_Manager : Manager_Type;
         Areas                : Planet_Area_Lists.List;
         Controlled_Tiles     : List_Of_Tiles.List;
         Explored_Tiles       : List_Of_Tiles.List;
         Seen_Tiles           : List_Of_Tiles.List;
         Unseen_Tiles         : List_Of_Tiles.List;
         Target_Tiles         : List_Of_Tiles.List;
         Hostile_Tiles        : List_Of_Tiles.List;
         Active_Targets       : List_Of_Tiles.List;
         Tile_Info            : Tile_Info_Array;
         Hostile_Stacks       : Stack_Lists.List;
         Spotted_Hostiles     : Identifier_Sets.Map;
         Palace               : Carthage.Handles.Cities.City_Handle;
         Shield               : Carthage.Handles.Cities.City_Handle;
      end record;

   type Planet_Manager_Type is access all Planet_Manager_Record'Class;

   overriding function Name
     (Manager : Planet_Manager_Record)
      return String
   is ("planet/" & Manager.Planet.Short_Name);

   overriding function Target_Id
     (Manager : Planet_Manager_Record)
      return String
   is (Manager.House.Tag & "--" & Manager.Planet.Tag);

   overriding procedure On_Hostile_Spotted
     (Manager : in out Planet_Manager_Record;
      Spotter : Carthage.Handles.Stacks.Stack_Handle'Class;
      Hostile : Carthage.Handles.Stacks.Stack_Handle'Class);

   overriding function Average_Update_Frequency
     (Manager : Planet_Manager_Record)
      return Duration
   is (Carthage.Calendar.Days (1));

   overriding procedure Initialize
     (Manager : not null access Planet_Manager_Record);

   overriding function Update
     (Manager : not null access Planet_Manager_Record)
      return Duration;

   procedure Scan_Unexplored_Tiles
     (Manager : in out Planet_Manager_Record'Class);

   function Create_Planet_Manager
     (House  : Carthage.Handles.Houses.House_Handle;
      Planet : Carthage.Handles.Planets.Planet_Handle;
      Meta   : Stack_Meta_Manager_Access;
      Active : Boolean)
      return Manager_Type;

   function Create_Active_Planet_Manager
     (House  : Carthage.Handles.Houses.House_Handle;
      Planet : Carthage.Handles.Planets.Planet_Handle;
      Meta   : Stack_Meta_Manager_Access)
      return Manager_Type
   is (Create_Planet_Manager (House, Planet, Meta, True));

   function Create_Passive_Planet_Manager
     (House  : Carthage.Handles.Houses.House_Handle;
      Planet : Carthage.Handles.Planets.Planet_Handle;
      Meta   : Stack_Meta_Manager_Access)
      return Manager_Type
   is (Create_Planet_Manager (House, Planet, Meta, False));

   ---------------------------
   -- Create_Planet_Manager --
   ---------------------------

   function Create_Planet_Manager
     (House  : Carthage.Handles.Houses.House_Handle;
      Planet : Carthage.Handles.Planets.Planet_Handle;
      Meta   : Stack_Meta_Manager_Access;
      Active : Boolean)
      return Manager_Type
   is

      Manager : constant Planet_Manager_Type :=
                  new Planet_Manager_Record;

      Group : constant Carthage.Managers.Cities.City_Trade_Group :=
                Carthage.Managers.Cities.New_Trade_Group;

      procedure Add_City_Manager
        (Reference : Carthage.Handles.City_Reference);

      ----------------------
      -- Add_City_Manager --
      ----------------------

      procedure Add_City_Manager
        (Reference : Carthage.Handles.City_Reference)
      is
         City : constant Carthage.Handles.Cities.City_Handle :=
                  Carthage.Handles.Cities.Get (Reference);
      begin
         if City.Owner.Tag = House.Tag then
            Manager.City_Managers.Insert
              (City.Identifier,
               Carthage.Managers.Cities.Create_City_Manager
                 (House  => House,
                  Group  => Group,
                  City   => City,
                  Ground => Manager.Ground_Asset_Manager));
         end if;
      end Add_City_Manager;

   begin
      Manager.House := House;
      Manager.Planet := Planet;
      Manager.Active := Active;

      Manager.Log ("creating "
                   & (if Active then "active" else "passive")
                   & " manager"
                   & ": seen = "
                   & (if Manager.Planet.Seen_By (Manager.House.Reference)
                     then "true" else "false")
                   & "; explored = "
                   & (if Manager.Planet.Explored_By (Manager.House.Reference)
                     then "true" else "false"));

      if Manager.Planet.Seen_By (Manager.House.Reference) then
         Manager.Ground_Asset_Manager :=
           Carthage.Managers.Assets.Ground_Asset_Manager
             (Meta, House, Planet);

         Planet.For_All_Cities (Add_City_Manager'Access);
      end if;

      Manager.Log ("initializing");

      Manager.Initialize;
      Add_Manager (Manager);

      Manager.Log ("manager created");
      return Manager_Type (Manager);
   end Create_Planet_Manager;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Manager : not null access Planet_Manager_Record)
   is
      procedure Classify_Tiles;

      procedure Scan_Area
        (Start : Tile_Position);

      --------------------
      -- Classify_Tiles --
      --------------------

      procedure Classify_Tiles is
      begin
         for Y in Tile_Y loop
            for X in Tile_X loop
               declare
                  use type Carthage.Handles.Houses.House_Handle;

                  Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                           Manager.Planet.Get_Tile (X, Y);
                  City : constant Carthage.Handles.Cities.City_Handle :=
                           (if Tile.Has_City
                            then Carthage.Handles.Cities.Get (Tile.Get_City)
                            else Carthage.Handles.Cities.Empty_Handle);

                  Info : Tile_Info_Record :=
                           Tile_Info_Record'
                             (Tile                 => Tile,
                              Nearest_Seen         => <>,
                              Nearest_Explored     => <>,
                              Nearest_Controlled   => <>,
                              Interest             => 0,
                              Continent            =>
                                Planet_Area_Lists.No_Element,
                              Area_Internal_Border => False,
                              Area_External_Border => False,
                              Controlled           => False,
                              Explored             => False,
                              Seen                 => False,
                              Targeted             => False);

                  function At_War_With
                    (Enemy : Carthage.Handles.Stack_Reference)
                  return Boolean
                  is (Manager.House.At_War_With
                      (Carthage.Handles.Stacks.Get (Enemy).Owner));

               begin
                  if Carthage.Handles.Tiles.Currently_Visible_To
                    (Tile, Manager.House)
                  then

                     if Tile.Has_Stack (At_War_With'Access) then
                        Manager.Log
                          (Manager.House.Tag
                           & ": hostile at "
                           & Carthage.Handles.Tiles.Position_Image
                             (Tile.Position));
                        Info.Interest := 1_000
                          + Natural
                          (Carthage.Handles.Stacks.Get
                             (Tile.Find_Stack
                                  (At_War_With'Access)).Asset_Count)
                            + WL.Random.Random_Number (1, 100);
                        Manager.Hostile_Tiles.Append (Tile);
                     end if;

                     Manager.Controlled_Tiles.Append (Tile);
                     Info.Nearest_Seen := Tile;
                     Info.Nearest_Explored := Tile;
                     Info.Nearest_Controlled := Tile;
                  end if;

                  if Tile.Explored_By (Manager.House) then
                     if City.Has_Element
                       and then City.Owner /= Manager.House
                       and then Manager.House.At_War_With (City.Owner)
                     then
                        Manager.Log ("hostile city at "
                                     & Tile.Description);
                        Info.Interest := 600
                          + WL.Random.Random_Number (1, 100);
                     end if;

                     if City.Has_Element then
                        declare
                           use Carthage.Handles.Structures;
                           Structure : constant Structure_Handle :=
                                         Get (City.Structure);
                        begin
                           if Structure.Is_Palace then
                              Manager.Palace := City;
                           elsif Structure.Is_Shield then
                              Manager.Shield := City;
                           end if;
                        end;
                     end if;

                     Manager.Explored_Tiles.Append (Tile);
                     Info.Nearest_Explored := Tile;
                     Info.Nearest_Controlled := Tile;

                  elsif Tile.Seen_By (Manager.House) then
                     Manager.Seen_Tiles.Append (Tile);
                     Info.Nearest_Seen := Tile;
                     Info.Interest := WL.Random.Random_Number (1, 100);
                  else
                     Manager.Unseen_Tiles.Append (Tile);
                     declare
                        use Carthage.Handles.Planets;
                        Ns : Surface_Tiles;
                     begin
                        Get_Tiles
                          (Manager.Planet, Tile, 1, 3, null, Ns);
                        Info.Interest := 100;
                        for I in 1 .. Tile_Count (Ns) loop
                           if Get_Tile (Ns, I).Currently_Visible_To
                             (Manager.House)
                           then
                              Info.Interest := Info.Interest + 6
                                + WL.Random.Random_Number (1, 6);
                           end if;
                        end loop;
                     end;

                  end if;
                  Manager.Tile_Info (X, Y) := Info;
               end;
            end loop;
         end loop;

         for Y in Tile_Y loop
            for X in Tile_X loop
               declare
                  Info : Tile_Info_Record renames Manager.Tile_Info (X, Y);
               begin
                  if not Planet_Area_Lists.Has_Element (Info.Continent) then
                     Scan_Area ((X, Y));
                  end if;
               end;
            end loop;
         end loop;

         declare
            Count : Natural := 0;
         begin
            for Area of Manager.Areas loop
               if Area.Class = Continent then
                  Count := Count + 1;
               end if;
            end loop;

            Manager.Log ("found"
                         & Count'Image
                         & " continent"
                         & (if Count = 1 then "" else "s"));
         end;

         if Manager.Owned then
            declare
               use Carthage.Handles.Tiles;
               Palace_Tile     : constant Carthage.Handles.Tiles.Tile_Handle :=
                                   Carthage.Handles.Tiles.Get
                                     (Carthage.Handles.Cities.Get
                                        (Manager.Planet.Palace).Tile);
               Palace_Position : constant Tile_Position :=
                                   Palace_Tile.Position;
               Home_Continent  : constant Planet_Area_Lists.Cursor :=
                                   Manager.Tile_Info
                                     (Palace_Position.X, Palace_Position.Y)
                                     .Continent;
            begin
               for Tile of Manager.Areas (Home_Continent).Internal_Border loop
                  Manager.Tile_Info (Tile.Position.X, Tile.Position.Y).Interest
                    := 100 + WL.Random.Random_Number (1, 100);
               end loop;
            end;
         end if;

      end Classify_Tiles;

      ---------------
      -- Scan_Area --
      ---------------

      procedure Scan_Area
        (Start : Tile_Position)
      is
         use Carthage.Handles.Planets, Carthage.Handles.Tiles;
         Start_Tile : constant Tile_Handle := Manager.Planet.Get_Tile (Start);
         Class      : constant Planet_Area_Class :=
                        (if Is_Water (Start_Tile)
                         then Sea else Continent);

         Border : array (Tile_X, Tile_Y) of Boolean :=
                    (others => (others => False));

         function Is_Member
           (Tile : Carthage.Handles.Tiles.Tile_Handle)
            return Boolean
         is (case Class is
                when Sea       => Is_Water (Tile),
                when Continent => not Is_Water (Tile));

         procedure Process (Tile : Carthage.Handles.Tiles.Tile_Handle);

         -------------
         -- Process --
         -------------

         procedure Process (Tile : Carthage.Handles.Tiles.Tile_Handle) is
            Ns           : constant Carthage.Handles.Planets.Array_Of_Tiles :=
                             Neighbour_Tiles (Manager.Planet, Tile.Position);
            Area         : Planet_Area_Record renames
                             Manager.Areas (Manager.Areas.Last);
            Found_Border : Boolean := False;
         begin
            Manager.Tile_Info (Tile.Position.X, Tile.Position.Y).Continent :=
              Manager.Areas.Last;
            Area.Tiles.Append (Tile);

            for N of Ns loop
               declare
                  X : constant Tile_X := N.Position.X;
                  Y : constant Tile_Y := N.Position.Y;
               begin
                  if not Border (X, Y) and then not Is_Member (N) then
                     Area.External_Border.Append (N);
                     Border (X, Y) := True;
                     Manager.Tile_Info (X, Y).Area_External_Border := True;
                     if not Found_Border then
                        Found_Border := True;
                        Area.Internal_Border.Append (Tile);
                        Manager.Tile_Info
                          (Tile.Position.X, Tile.Position.Y)
                            .Area_Internal_Border := True;
                     end if;
                  end if;
               end;
            end loop;

         end Process;

      begin
         Manager.Areas.Append (Planet_Area_Record'(Class, others => <>));

         Scan_Connected_Tiles
           (This    => Manager.Planet,
            Start   => Start,
            Test    => Is_Member'Access,
            Process => Process'Access);
      end Scan_Area;

      use Carthage.Handles;

   begin
      Manager.Owned :=
        Manager.Planet.Has_Owner
        and then Manager.Planet.Owner = Manager.House.Reference;

      Manager.Controlled_Tiles.Clear;
      Manager.Explored_Tiles.Clear;
      Manager.Seen_Tiles.Clear;
      Manager.Unseen_Tiles.Clear;
      Manager.Hostile_Tiles.Clear;

      if Manager.Active
        and then Manager.Planet.Seen_By (Manager.House.Reference)
      then
         Manager.Log ("classifying tiles");
         Classify_Tiles;
         Manager.Log ("scanning tiles");
         Manager.Scan_Unexplored_Tiles;
      end if;

   end Initialize;

   ------------------------
   -- On_Hostile_Spotted --
   ------------------------

   overriding procedure On_Hostile_Spotted
     (Manager : in out Planet_Manager_Record;
      Spotter : Carthage.Handles.Stacks.Stack_Handle'Class;
      Hostile : Carthage.Handles.Stacks.Stack_Handle'Class)
   is
      pragma Unreferenced (Spotter);
      Goal : constant Carthage.Goals.Goal_Record'Class :=
               Carthage.Managers.Assets.Tile_Capture_Goal
                 (Hostile.Current_Tile,
                  Carthage.Handles.Stacks.Total_Strength (Hostile));
   begin
      Manager.Log
        (Manager.Ground_Asset_Manager.Description
         & ": adding goal: " & Goal.Show);
      Manager.Ground_Asset_Manager.Add_Goal (Goal);
   end On_Hostile_Spotted;

   ---------------------------
   -- Scan_Unexplored_Tiles --
   ---------------------------

   procedure Scan_Unexplored_Tiles
     (Manager : in out Planet_Manager_Record'Class)
   is
      Tiles : List_Of_Tiles.List renames Manager.Active_Targets;

      function Higher_Interest
        (Left, Right : Carthage.Handles.Tiles.Tile_Handle)
         return Boolean;

      ---------------------
      -- Higher_Interest --
      ---------------------

      function Higher_Interest
        (Left, Right : Carthage.Handles.Tiles.Tile_Handle)
         return Boolean
      is
         L : constant Tile_Position := Left.Position;
         R : constant Tile_Position := Right.Position;
      begin
         return Manager.Tile_Info (L.X, L.Y).Interest
           > Manager.Tile_Info (R.X, R.Y).Interest;
      end Higher_Interest;

      package Sorting is
        new List_Of_Tiles.Generic_Sorting
          (Higher_Interest);

   begin
      Manager.Log
        (Manager.House.Tag & ": looking for unexplored tiles");

      Tiles.Clear;

      for Unseen of Manager.Unseen_Tiles loop
         Tiles.Append (Unseen);
      end loop;
      for Seen of Manager.Seen_Tiles loop
         Tiles.Append (Seen);
      end loop;

      Manager.Log
        (Manager.House.Tag & ":" & Natural'Image (Natural (Tiles.Length))
         & " tiles found");

      Sorting.Sort (Tiles);

      Manager.Log ("tiles sorted; looking for interesting tiles");

      while not Tiles.Is_Empty loop

         declare
            Tile     : constant Carthage.Handles.Tiles.Tile_Handle :=
                         Tiles.First_Element;
            --  Pos      : constant Tile_Position :=
            --               Carthage.Handles.Tiles.Get_Position (Tile);
            New_List : List_Of_Tiles.List;
         begin

            Manager.Log (Tile.Description
                         & " "
                         & (if Tile.Seen_By (Manager.House)
                           then "s" else "-")
                         & (if Tile.Explored_By (Manager.House)
                           then "e" else "-")
                         & (if Tile.Currently_Visible_To (Manager.House)
                           then "v" else "-")
                         & (if Tile.Has_Stacks
                           then " " & Carthage.Handles.Stacks.Get
                             (Tile.First_Stack)
                           .Description
                             else "")
                        );

            declare
               Goal : constant Carthage.Goals.Goal_Record'Class :=
                        Carthage.Managers.Assets.Tile_Reconnaissance_Goal
                          (Tile);
            begin
               if Manager.Ground_Asset_Manager
                 .Have_Immediate_Capacity (Goal)
               then
                  Manager.Ground_Asset_Manager.Add_Goal (Goal);
               else
                  Manager.Log ("no more recon capacity");
                  exit;
               end if;
            end;

            for Check_Tile of Tiles loop
               if Carthage.Handles.Planets.Hex_Distance
                 (Manager.Planet, Tile.Position, Check_Tile.Position)
                 > 6
               then
                  New_List.Append (Check_Tile);
               end if;
            end loop;

            Tiles := New_List;
         end;

      end loop;

      Manager.Log ("finished tile scan");

   end Scan_Unexplored_Tiles;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Manager : not null access Planet_Manager_Record)
      return Duration
   is
   begin
      Manager.Log ("updating for " & Manager.House.Tag);
      Manager.Check_Goals;

      if Manager.Palace.Has_Element then
         declare
            Minimum, Desired : Resource_Stock;
         begin
            Manager.Ground_Asset_Manager.Get_Resource_Requirements
              (Minimum, Desired);
            Manager.Log ("ground asset desired resources");

            declare
               procedure Report_Stock
                 (Resource : Carthage.Handles.Resources.Resource_Handle);

               ------------------
               -- Report_Stock --
               ------------------

               procedure Report_Stock
                 (Resource : Carthage.Handles.Resources.Resource_Handle)
               is
                  Quantity : constant Natural :=
                               Desired.Whole_Quantity (Resource);
               begin
                  if Quantity > 0 then
                     Manager.Log
                       (Resource.Tag & ":" & Quantity'Image);
                  end if;
               end Report_Stock;

            begin
               Carthage.Handles.Resources.For_All_Resources
                 (Report_Stock'Access);
            end;

            Manager.Ground_Asset_Manager.Transfer_Resources
              (Manager.Palace, Desired);
         end;
      end if;

      return Manager.Average_Update_Frequency;
   end Update;

end Carthage.Managers.Planets;
