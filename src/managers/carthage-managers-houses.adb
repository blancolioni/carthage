with WL.String_Sets;

with Carthage.Handles.Galaxy;

with Carthage.Managers.Assets;
with Carthage.Managers.Planets;

package body Carthage.Managers.Houses is

   type Passive_House_Manager_Record is
     new House_Manager_Record with null record;

   overriding function Name
     (Manager : Passive_House_Manager_Record)
      return String
   is ("passive/" & Manager.House.Tag);

   type Noble_House_Manager_Record is
     new House_Manager_Record with null record;

   overriding function Name
     (Manager : Noble_House_Manager_Record)
      return String
   is ("noble/" & Manager.House.Tag);

   overriding procedure Initialize
     (Manager : not null access Noble_House_Manager_Record);

   overriding function Planet_Manager
     (Manager : not null access Noble_House_Manager_Record;
      Planet  : Carthage.Handles.Planets.Planet_Handle)
      return Manager_Type
   is (Carthage.Managers.Planets.Create_Active_Planet_Manager
       (Manager.House, Planet, Manager));

   overriding procedure On_Hostile_Spotted
     (Manager : in out Noble_House_Manager_Record;
      Spotter : Carthage.Handles.Stacks.Stack_Handle'Class;
      Hostile : Carthage.Handles.Stacks.Stack_Handle'Class);

     --     overriding procedure Check_Goals
     --       (Manager : in out Noble_House_Manager_Record);

   type League_House_Manager_Record is
     new House_Manager_Record with null record;

   overriding function Name
     (Manager : League_House_Manager_Record)
      return String
   is ("league");

   type Church_House_Manager_Record is
     new House_Manager_Record with null record;

   overriding function Name
     (Manager : Church_House_Manager_Record)
      return String
   is ("church");

   --     ----------------------
--     -- Add_Capture_Goal --
--     ----------------------
--
--     procedure Add_Planet_Capture_Goal
--       (Manager : in out House_Manager_Record;
--        Planet  : Carthage.Handles.Planets.Planet_Handle)
--     is
--     begin
--        Manager.House.Log ("goal: scan " & Planet.Name);
--  --        Manager.Goals.Append
--  --          (House_Manager_Goal'
--  --             (Carthage.Goals.Goal_Record with
--  --                  Priority => Default_Priority (Capture_Planet),
--  --                  Class    => Capture_Planet,
--  --                  Planet   => Planet));
--     end Add_Planet_Capture_Goal;
--
--     --------------------------
--     -- Add_Planet_Scan_Goal --
--     --------------------------
--
--     procedure Add_Planet_Scan_Goal
--       (Manager : in out House_Manager_Record;
--        Planet  : Carthage.Handles.Planets.Planet_Handle)
--     is
--     begin
--        Manager.Goals.Append
--          (House_Manager_Goal'
--             (Carthage.Goals.Goal_Record with
--                  Priority => Default_Priority (Planet_Scan),
--                  Class    => Planet_Scan,
--                  Planet => Planet));
--     end Add_Planet_Scan_Goal;
--
--     ----------------------------------
--     -- Add_Surface_Exploration_Goal --
--     ----------------------------------
--
--     procedure Add_Surface_Exploration_Goal
--       (Manager : in out House_Manager_Record;
--        Planet  : Carthage.Handles.Planets.Planet_Handle)
--     is
--     begin
--        Manager.House.Log ("goal: explore surface of " & Planet.Name);
--        Manager.Goals.Append
--          (House_Manager_Goal'
--             (Carthage.Goals.Goal_Record with
--                  Priority => Default_Priority (Explore_Surface),
--                  Class    => Explore_Surface,
--                  Planet   => Planet));
--     end Add_Surface_Exploration_Goal;
--
--     ----------------
--     -- Check_Goal --
--     ----------------
--
--     overriding function Check_Goal
--       (Manager : House_Manager_Record;
--        Goal    : Carthage.Goals.Goal_Record'Class)
--        return Boolean
--     is
--        House_Goal : House_Manager_Goal renames House_Manager_Goal (Goal);
--     begin
--        case House_Goal.Class is
--           when None =>
--              return True;
--           when Explore_Surface =>
--              declare
--                 Info : Managed_Planet_Record renames
--                          Manager.Planets (House_Goal.Planet.Identifier);
--              begin
--                 Info.Planet_Manager.Add_Surface_Exploration_Goal;
--              end;
--              return True;
--           when Planet_Scan =>
--              --  Manager.Space_Assets.Add_Planet_Scan_Goal (Goal.Planet);
--              return True;
--           when Capture_Planet =>
--              --  complicated!
--              return True;
--        end case;
--     end Check_Goal;
--
--     -----------------
--     -- Check_Goals --
--     -----------------
--
--     overriding procedure Check_Goals
--       (Manager : in out House_Manager_Record)
--     is
--     begin
--        Manager_Record (Manager).Check_Goals;
--
--        for Planet of Manager.Planets loop
--           Planet.Planet_Manager.Check_Goals;
--        end loop;
--     end Check_Goals;
--
--     -----------------
--     -- Check_Goals --
--     -----------------
--
--     overriding procedure Check_Goals
--       (Manager : in out Noble_House_Manager_Record)
--     is
--     begin
--        House_Manager_Record (Manager).Check_Goals;
--     end Check_Goals;

   --------------------------
   -- Create_House_Manager --
   --------------------------

   procedure Create_House_Manager
     (House  : Carthage.Handles.Houses.House_Handle)
   is
      use all type Carthage.Handles.Houses.House_Category;
      Manager : House_Manager_Type;
   begin
      case House.Category is
         when Noble =>
            Manager := new Noble_House_Manager_Record;
         when League =>
            Manager := new League_House_Manager_Record;
         when Church =>
            Manager := new Church_House_Manager_Record;
         when Rebels =>
            Manager := new Passive_House_Manager_Record;
         when Imperial =>
            Manager := new Passive_House_Manager_Record;
         when Vau =>
            Manager := new Passive_House_Manager_Record;
         when Symbiot =>
            Manager := new Passive_House_Manager_Record;
      end case;
      Manager.House := House;
      Manager.Space_Assets :=
        Carthage.Managers.Assets.Space_Asset_Manager
          (Stack_Meta_Manager_Access (Manager), Manager.House);

      --  Manager.House.Update_House
      --    .Set_Manager (Manager.Name)
      --    .Done;

      Manager.Initialize;
      Add_Manager (Manager);
   end Create_House_Manager;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Manager : not null access House_Manager_Record)
   is

      procedure Add_Planet_Info
        (Planet : Carthage.Handles.Planets.Planet_Handle);

      procedure Add_Stack (Stack : Carthage.Handles.Stacks.Stack_Handle);

      ---------------------
      -- Add_Planet_Info --
      ---------------------

      procedure Add_Planet_Info
        (Planet : Carthage.Handles.Planets.Planet_Handle)
      is
      begin
         Manager.Planets.Insert
           (Planet.Tag,
            Managed_Planet_Record'
              (Planet         => Planet,
               Planet_Manager =>
                 House_Manager_Type (Manager)
               .Planet_Manager (Planet)));
      end Add_Planet_Info;

      ---------------
      -- Add_Stack --
      ---------------

      procedure Add_Stack (Stack : Carthage.Handles.Stacks.Stack_Handle) is
         use type Carthage.Handles.Houses.House_Handle;
         Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                    Carthage.Handles.Planets.Get (Stack.Planet);
      begin
         if Stack.Owner = Manager.House
           and then Stack.Has_Assets
           and then not Manager.Planets.Contains (Planet.Tag)
         then
            Manager.Log ("found owned stack on " & Planet.Local_Text
                         & ": " & Stack.Description);
            Add_Planet_Info (Planet);
         end if;
      end Add_Stack;

   begin

      Carthage.Handles.Stacks.For_All_Stacks (Add_Stack'Access);

--        for Info of Manager.Planets loop
--           Info.Planet_Manager.Load_Initial_State;
--        end loop;

   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Manager : not null access Noble_House_Manager_Record)
   is

      Goal_Planets : WL.String_Sets.Set;

      procedure Add_Visit_Goal
        (Planet : Carthage.Handles.Planets.Planet_Handle);

      --------------------
      -- Add_Visit_Goal --
      --------------------

      procedure Add_Visit_Goal
        (Planet : Carthage.Handles.Planets.Planet_Handle)
      is
      begin
         if not Planet.Explored_By (Manager.House.Reference)
           and then not Goal_Planets.Contains (Planet.Tag)
         then
            Goal_Planets.Include (Planet.Tag);

            declare
               Goal : constant Carthage.Goals.Goal_Record'Class :=
                        Carthage.Managers.Assets.Planet_Reconnaissance_Goal
                          (Planet);
            begin
               if Manager.Space_Assets.Have_Immediate_Capacity (Goal) then
                  Manager.Log ("  adding goal: " & Goal.Show);
                  Manager.Space_Assets.Add_Goal (Goal);
               end if;
            end;

         end if;
      end Add_Visit_Goal;

   begin
      House_Manager_Record (Manager.all)'Access.Initialize;

      for Info of Manager.Planets loop
         declare
            use type Carthage.Handles.House_Reference;
            Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                       Info.Planet;
         begin
            if Planet.Has_Owner
              and then Planet.Owner = Manager.House.Reference
            then
               Carthage.Handles.Galaxy.Scan_Connections
                 (Planet, Add_Visit_Goal'Access);
            end if;

            if Planet.Explored_By (Manager.House.Reference) then
               null;
--              elsif Planet.Seen_By (Manager.House) then
--                 Manager.Add_Surface_Exploration_Goal (Planet);
--              else
--                 Manager.Add_Planet_Scan_Goal (Planet);
            end if;
         end;
      end loop;
   end Initialize;

   ------------------------
   -- On_Hostile_Spotted --
   ------------------------

   overriding procedure On_Hostile_Spotted
     (Manager : in out Noble_House_Manager_Record;
      Spotter : Carthage.Handles.Stacks.Stack_Handle'Class;
      Hostile : Carthage.Handles.Stacks.Stack_Handle'Class)
   is
      Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                 Carthage.Handles.Planets.Get (Spotter.Planet);
   begin
      if Spotter.Is_Orbital then
         null;
      else
         if not Manager.Hostiles.Contains (Hostile.Identifier) then
            Manager.Log
              (Spotter.Identifier & " spotted "
               & Carthage.Handles.Stacks.Description (Hostile));

            Manager.Planets.Element (Planet.Tag).Planet_Manager
              .On_Hostile_Spotted (Spotter, Hostile);
            Manager.Hostiles.Insert
              (Hostile.Identifier,
               Carthage.Handles.Stacks.Stack_Handle (Hostile));
         end if;
      end if;
   end On_Hostile_Spotted;

   --------------------
   -- Planet_Manager --
   --------------------

   function Planet_Manager
     (Manager : not null access House_Manager_Record;
      Planet  : Carthage.Handles.Planets.Planet_Handle)
      return Manager_Type
   is
   begin
      return Carthage.Managers.Planets.Create_Passive_Planet_Manager
        (Manager.House, Planet, Manager);
   end Planet_Manager;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Manager : not null access House_Manager_Record)
      return Duration
   is
   begin
      Manager.Log ("updating");
      return Carthage.Calendar.Days (1);
   end Update;

end Carthage.Managers.Houses;
