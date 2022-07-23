with Carthage.Quantities;

with Carthage.Goals.Transport;

with Carthage.Handles.Planets;
with Carthage.Handles.Resources;
with Carthage.Handles.Structures;
with Carthage.Handles.Tiles;

package body Carthage.Handles.Managers.Cities is

   Log_Production : constant Boolean := True;

   type City_Manager_Record is
     new Root_Manager_Record with
      record
         City      : City_Reference;
         Structure : Structure_Reference;
         Planet    : Planet_Reference;
         Tiles     : Carthage.Handles.Planets.Surface_Tiles;
         Requests  : Carthage.Handles.Resources.Resource_Stock;
      end record;

   overriding function Name
     (Manager : City_Manager_Record)
      return String
   is (Carthage.Handles.Houses.Get (Manager.House).Tag
       & "/"
       & Carthage.Handles.Planets.Get (Manager.Planet).Tag
       & " "
       & Carthage.Handles.Tiles.Get
         (Carthage.Handles.Cities.Get (Manager.City).Tile).Description
       & " manager");

   overriding procedure On_Activated
     (Manager : in out City_Manager_Record);

   overriding procedure Register
     (Manager : City_Manager_Record);

   procedure Manage_Production
     (Manager : in out City_Manager_Record'Class)
     with Pre => Carthage.Handles.Structures
       .Get (Manager.Structure).Has_Production;

   procedure Manage_Harvest
     (Manager : City_Manager_Record'Class)
     with Pre => Carthage.Handles.Structures
       .Get (Manager.Structure).Is_Harvester;

   -------------------------
   -- Create_City_Manager --
   -------------------------

   procedure Create_City_Manager
     (House            : Carthage.Handles.Houses.House_Handle;
      City             : Carthage.Handles.Cities.City_Handle)
   is
      Rec : City_Manager_Record;
      Structure : constant Carthage.Handles.Structures.Structure_Handle :=
        Carthage.Handles.Structures.Get (City.Structure);
   begin
      Rec.Initialize
        (Class        => Ground,
         Authority    => Production_Management,
         House        => House,
         First_Event  => Carthage.Calendar.Days (7),
         Random_Start => True);
      Rec.City := City.Reference;
      Rec.Structure := City.Structure;
      Rec.Planet := City.Planet;

      if Structure.Is_Harvester then
         Carthage.Handles.Planets.Get (Rec.Planet).Get_Tiles
           (Origin       => Carthage.Handles.Tiles.Get (City.Tile),
            Min_Distance => 0,
            Max_Distance => Structure.Radius + 1,
            Test         => null,
            Tiles        => Rec.Tiles);
      end if;

      declare
         Handle : constant Manager_Handle :=
           Rec.Create_Manager;
      begin
         pragma Unreferenced (Handle);
      end;

   end Create_City_Manager;

   --------------------
   -- Manage_Harvest --
   --------------------

   procedure Manage_Harvest
     (Manager : City_Manager_Record'Class)
   is
      use Carthage.Handles.Cities;
      use Carthage.Handles.Planets;
      use Carthage.Handles.Structures;

      City      : constant City_Handle := Get (Manager.City);
      Structure : constant Structure_Handle := Get (Manager.Structure);
      Tiles     : Surface_Tiles renames Manager.Tiles;
      Harvest   : Carthage.Handles.Resources.Resource_Stock;
   begin

      for I in 1 .. Tile_Count (Tiles) loop
         declare
            Prod : constant Carthage.Handles.Structures.Production_Array :=
                     Structure.Harvest_Production
                       (Get_Tile (Tiles, I).Reference);
         begin
            for Rec of Prod loop
               declare
                  Quantity : constant Carthage.Quantities.Quantity_Type :=
                               Carthage.Quantities.Scale
                                 (Rec.Quantity,
                                  Real (City.Health) / 100.0
                                  * Real (City.Loyalty) / 100.0
                                  / 10.0);
               begin
                  Manager.Information
                    ("harvested "
                     & Carthage.Quantities.Show (Quantity)
                     & " "
                     & Rec.Resource.Local_Text
                     & " from "
                     & Get_Tile (Tiles, I).Description);
                  City.Add (Rec.Resource, Quantity);
                  Harvest.Add (Rec.Resource, Quantity);
               end;
            end loop;
         end;
      end loop;

      declare

         procedure Add_Transport_Goal
           (Resource : Carthage.Handles.Resources.Resource_Handle;
            Quantity : Carthage.Quantities.Quantity_Type);

         ------------------------
         -- Add_Transport_Goal --
         ------------------------

         procedure Add_Transport_Goal
           (Resource : Carthage.Handles.Resources.Resource_Handle;
            Quantity : Carthage.Quantities.Quantity_Type)
         is
         begin
            Get_Manager
              (Class     => Ground,
               Authority => Resource_Management,
               House     => City.Owner.Reference,
               Planet    => City.Planet)
              .Add_Pending_Goal
              (Goal       =>
                 Carthage.Goals.Transport.Resource_Available
                   (Source   => Carthage.Handles.Tiles.Get (City.Tile),
                    Resource => Resource,
                    Quantity => Quantity));
         end Add_Transport_Goal;

      begin
         Harvest.Scan_Stock (Add_Transport_Goal'Access);
      end;

   end Manage_Harvest;

   -----------------------
   -- Manage_Production --
   -----------------------

   procedure Manage_Production
     (Manager : in out City_Manager_Record'Class)
   is
      use Carthage.Handles.Cities;
      use Carthage.Handles.Structures;

      City : constant City_Handle := Get (Manager.City);
      Structure : constant Structure_Handle := Get (Manager.Structure);
      Inputs  : constant Production_Array :=
                  Structure.Production_Inputs;
      Transport : constant Manager_Handle :=
                    Get_Manager
                      (Class     => Ground,
                       Authority => Resource_Management,
                       House     => City.Owner.Reference,
                       Planet    => City.Planet);
   begin
      for Item of Inputs loop
         declare
            use type Carthage.Quantities.Quantity_Type;
            Available : constant Carthage.Quantities.Quantity_Type :=
                          City.Quantity (Item.Resource);
            Requested : constant Carthage.Quantities.Quantity_Type :=
                          Manager.Requests.Quantity (Item.Resource);
            Pipeline  : constant Carthage.Quantities.Quantity_Type :=
                          Available + Requested;
            Required  : constant Carthage.Quantities.Quantity_Type :=
                         Carthage.Quantities.Scale (Item.Quantity, 0.2);
         begin
            if Log_Production then
               Manager.Log (Item.Resource.Tag
                            & ": have "
                            & Carthage.Quantities.Show (Available)
                            & "; require "
                            & Carthage.Quantities.Show (Required));
            end if;

            if Pipeline < Required then
               Add_Pending_Goal
                 (To_Manager => Transport,
                  Goal       =>
                    Carthage.Goals.Transport.Resource_Request
                      (Destination => Carthage.Handles.Tiles.Get (City.Tile),
                       Resource => Item.Resource,
                       Quantity => Required - Pipeline));
               Manager.Requests.Add (Item.Resource, Required - Pipeline);
            end if;
         end;
      end loop;

      declare
         use Carthage.Quantities;
         Quantity : constant Quantity_Type :=
                      Structure.Execute_Production
                        (Stock      => City,
                         House      => City.Owner.Reference,
                         Tile       => City.Tile,
                         Efficiency =>
                           Real (City.Loyalty) / 100.0
                         * Real (City.Health) / 100.0,
                         Factor     => 0.1);
      begin
         if Quantity > Zero then
            City.Add (Structure.Production_Output, Quantity);
         end if;
      end;

   end Manage_Production;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated
     (Manager : in out City_Manager_Record)
   is
   begin
      if Carthage.Handles.Structures
        .Get (Manager.Structure).Has_Production
      then
         Manager.Manage_Production;
      elsif Carthage.Handles.Structures
        .Get (Manager.Structure).Is_Harvester
      then
         Manager.Manage_Harvest;
      end if;
      Manager.Schedule_Next_Update (Wait => Carthage.Calendar.Days (10));
   end On_Activated;

   --------------
   -- Register --
   --------------

   overriding procedure Register
     (Manager : City_Manager_Record)
   is
   begin
      Manager.Save_Manager
        (Planet => Manager.Planet,
         City   => Manager.City);
   end Register;

end Carthage.Handles.Managers.Cities;
