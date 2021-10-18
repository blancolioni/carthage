with Ada.Containers.Doubly_Linked_Lists;

with Carthage.Calendar;
with Carthage.Handles.Planets;
with Carthage.Handles.Resources;
with Carthage.Handles.Structures;

with Carthage.Handles.Cities.Updates;

with Carthage.Managers.Assets;

package body Carthage.Managers.Cities is

   type City_Resource is
      record
         City     : Carthage.Handles.Cities.City_Handle;
         Resource : Carthage.Handles.Resources.Resource_Handle;
      end record;

   package City_Resource_Lists is
     new Ada.Containers.Doubly_Linked_Lists (City_Resource);

   package City_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Carthage.Handles.Cities.City_Handle, Carthage.Handles.Cities."=");

   type City_Request is
      record
         City : Carthage.Handles.Cities.City_Handle;
         Request : Resource_Stock;
      end record;

   package City_Request_Lists is
     new Ada.Containers.Doubly_Linked_Lists (City_Request);

   type City_Trade_Group_Record is
      record
         City_Requests : City_Request_Lists.List;
      end record;

   procedure Set_Request
     (Group    : City_Trade_Group;
      City     : Carthage.Handles.Cities.City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Resource_Quantity);

   function Get_Total_Requests
     (Group    : City_Trade_Group;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Resource_Quantity;

   procedure Scan_Requests
     (Group    : City_Trade_Group;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Process  : not null access
        procedure (City : Carthage.Handles.Cities.City_Handle;
                   Quantity : in out Resource_Quantity));

   type City_Manager_Record is
     new Root_Manager_Type
     and Carthage.Handles.Cities.City_Manager_Interface with
      record
         Planet         : Carthage.Handles.Planets.Planet_Handle;
         Group          : City_Trade_Group;
         City           : Carthage.Handles.Cities.City_Handle;
         Structure      : Carthage.Handles.Structures.Structure_Handle;
         Palace         : Carthage.Handles.Cities.City_Handle;
         Shield         : Carthage.Handles.Cities.City_Handle;
         Agoras         : City_Lists.List;
         Available      : Resource_Stock;
         Ordered        : Resource_Stock;
         Sources        : City_Resource_Lists.List;
         Sinks          : City_Resource_Lists.List;
         City_Requests  : City_Request_Lists.List;
         Ground_Manager : Manager_Type;
      end record;

   type City_Manager_Type is access all City_Manager_Record'Class;

   overriding function Name
     (Manager : City_Manager_Record)
      return String
   is (Manager.City.Description);

   overriding function Target_Id
     (Manager : City_Manager_Record)
      return String
   is (Manager.City.Identifier);

   overriding procedure On_Hostile_Spotted
     (Manager : in out City_Manager_Record;
      Spotter : Carthage.Handles.Stacks.Stack_Handle'Class;
      Hostile : Carthage.Handles.Stacks.Stack_Handle'Class)
   is null;

   overriding function Average_Update_Frequency
     (Manager : City_Manager_Record)
      return Duration
   is (Carthage.Calendar.Days (1));

   overriding procedure Initialize
     (Manager : not null access City_Manager_Record);

   overriding function Update
     (Manager : not null access City_Manager_Record)
      return Duration;

   procedure Create_Resource_Network
     (Manager : in out City_Manager_Record'Class);

   procedure Create_Transfer_Goals
     (Manager   : in out City_Manager_Record'Class;
      Process   : not null access
        procedure (To_City : Carthage.Handles.Cities.City_Handle;
                   Stock   : Resource_Stock));

   -------------------------
   -- Create_City_Manager --
   -------------------------

   function Create_City_Manager
     (House  : Carthage.Handles.Houses.House_Handle;
      Group  : City_Trade_Group;
      City   : Carthage.Handles.Cities.City_Handle;
      Ground : Manager_Type)
      return Manager_Type
   is
      Manager : constant City_Manager_Type :=
                  new City_Manager_Record;
   begin
      Manager.House := House;
      Manager.Planet := Carthage.Handles.Planets.Get (City.Planet);
      Manager.Group := Group;
      Manager.Ground_Manager := Ground;

      declare
         V : Resource_Stock;
      begin
         Group.City_Requests.Append (City_Request'(City    => City,
                                                   Request => V));
      end;

      Manager.City := City;
      Manager.Structure := Carthage.Handles.Structures.Get (City.Structure);
      Manager.Initialize;
      Add_Manager (Manager);
      return Manager_Type (Manager);
   end Create_City_Manager;

   -----------------------------
   -- Create_Resource_Network --
   -----------------------------

   procedure Create_Resource_Network
     (Manager : in out City_Manager_Record'Class)
   is
      use Carthage.Handles.Structures;
      City      : constant Carthage.Handles.Cities.City_Handle := Manager.City;
      Structure : constant Structure_Handle := Get (City.Structure);
      Inputs    : constant Production_Array := Production_Inputs (Structure);
      Outputs   : constant Production_Array := Production_Outputs (Structure);

      procedure Add_Connection
        (Reference : Carthage.Handles.City_Reference);

      --------------------
      -- Add_Connection --
      --------------------

      procedure Add_Connection
        (Reference : Carthage.Handles.City_Reference)
      is

         Connected_City : constant Carthage.Handles.Cities.City_Handle :=
                            Carthage.Handles.Cities.Get (Reference);
         Structure : constant Carthage.Handles.Structures.Structure_Handle :=
                       Carthage.Handles.Structures.Get
                         (Connected_City.Structure);

      begin
         if Structure.Is_Palace then
            Manager.Palace := Connected_City;
         elsif Structure.Is_Shield then
            Manager.Shield := Connected_City;
         elsif Structure.Is_Agora then
            Manager.Agoras.Append (Connected_City);
         end if;

         for Item of Inputs loop
            if Structure.Produces (Item.Resource) then
               Manager.Sources.Append
                 (City_Resource'
                    (City     => Connected_City,
                     Resource => Item.Resource));
            end if;
         end loop;

         for Item of Outputs loop
            if Structure.Consumes (Item.Resource) then
               Manager.Sinks.Append
                 (City_Resource'
                    (City     => Connected_City,
                     Resource => Item.Resource));
            end if;
         end loop;
      end Add_Connection;

   begin
      Manager.Planet.For_All_Owned_Cities
        (Owner   => Manager.House.Reference,
         Process => Add_Connection'Access);
   end Create_Resource_Network;

   ---------------------------
   -- Create_Transfer_Goals --
   ---------------------------

   procedure Create_Transfer_Goals
     (Manager   : in out City_Manager_Record'Class;
      Process   : not null access
        procedure (To_City : Carthage.Handles.Cities.City_Handle;
                   Stock   : Resource_Stock))
   is
      use Carthage.Handles.Structures;

      type City_Transfer is
         record
            City : Carthage.Handles.Cities.City_Handle;
            Stock : Resource_Stock;
         end record;

      package City_Transfer_Lists is
        new Ada.Containers.Doubly_Linked_Lists (City_Transfer);

      Transfers : City_Transfer_Lists.List;

      procedure Add_Transfer
        (To_City  : Carthage.Handles.Cities.City_Handle;
         Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Natural);

      ------------------
      -- Add_Transfer --
      ------------------

      procedure Add_Transfer
        (To_City  : Carthage.Handles.Cities.City_Handle;
         Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Natural)
      is
      begin
         for Transfer of Transfers loop
            if Transfer.City.Identifier = To_City.Identifier then
               Transfer.Stock.Add (Resource, Quantity);
               return;
            end if;
         end loop;

         declare
            Stock : Resource_Stock;
         begin
            Stock.Add (Resource, Quantity);
            Transfers.Append
              (City_Transfer'
                 (City  => To_City,
                  Stock => Stock));
         end;
      end Add_Transfer;

   begin
      for Item of Manager.Structure.Production_Outputs loop
         declare
            Total_Requests : constant Resource_Quantity :=
              Get_Total_Requests (Manager.Group, Item.Resource);
            Available      : constant Resource_Quantity :=
              Manager.Available.Quantity (Item.Resource);
            Factor         : constant Float :=
              (if Available >= Total_Requests
               then 1.0
               else Float (Available)
               / Float (Total_Requests));

            procedure Process_Request
              (To_City  : Carthage.Handles.Cities.City_Handle;
               Quantity : in out Resource_Quantity);

            ---------------------
            -- Process_Request --
            ---------------------

            procedure Process_Request
              (To_City  : Carthage.Handles.Cities.City_Handle;
               Quantity : in out Resource_Quantity)
            is
            begin
               if Factor < 1.0 then
                  Quantity := Resource_Quantity (Factor * Float (Quantity));
               end if;

               declare
                  Whole_Quantity : constant Natural :=
                    Natural
                      (Float'Truncation (Float (Quantity)));
               begin
                  if Whole_Quantity > 0 then
                     Manager.Log
                       ("new goal:"
                        & Natural'Image (Natural (Quantity))
                        & " of"
                        & Natural'Image
                          (Carthage.Handles.Resources.Whole_Quantity
                               (Manager.City, Item.Resource))
                        & " " & Item.Resource.Tag
                        & " to " & To_City.Description);

                     Add_Transfer (To_City, Item.Resource, Whole_Quantity);

                     Manager.Available.Remove (Item.Resource, Whole_Quantity);
                     Carthage.Handles.Cities.Add_Scheduled_Transfer
                       (This     => Manager.City,
                        Item     => Item.Resource,
                        Quantity => Whole_Quantity);

                  end if;
               end;
            end Process_Request;

         begin
            Manager.Log
              ("requested" & Total_Requests'Image
               & " " & Item.Resource.Tag
               & " of" & Available'Image
               & "; factor = "
               & Natural'Image (Natural (Factor * 100.0))
               & "%");

            Scan_Requests (Manager.Group,
                           Item.Resource, Process_Request'Access);
         end;
      end loop;

      for Transfer of Transfers loop
         Process (Transfer.City, Transfer.Stock);
      end loop;

   end Create_Transfer_Goals;

   ------------------------
   -- Get_Total_Requests --
   ------------------------

   function Get_Total_Requests
     (Group    : City_Trade_Group;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Resource_Quantity
   is
   begin
      return Result : Resource_Quantity := 0.0 do
         for Reqs of Group.City_Requests loop
            Result := Result + Reqs.Request.Quantity (Resource);
         end loop;
      end return;
   end Get_Total_Requests;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Manager : not null access City_Manager_Record)
   is
   begin
      Manager.Create_Resource_Network;
   end Initialize;

   ---------------------
   -- New_Trade_Group --
   ---------------------

   function New_Trade_Group return City_Trade_Group is
   begin
      return new City_Trade_Group_Record;
   end New_Trade_Group;

   -------------------
   -- Scan_Requests --
   -------------------

   procedure Scan_Requests
     (Group    : City_Trade_Group;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Process  : not null access
        procedure (City : Carthage.Handles.Cities.City_Handle;
                   Quantity : in out Resource_Quantity))
   is
   begin
      for Item of Group.City_Requests loop
         declare
            Quantity : Resource_Quantity :=
                         Item.Request.Quantity (Resource);
         begin
            if Quantity > 0.0 then
               Process (Item.City, Quantity);
               Item.Request.Set_Quantity (Resource, Quantity);
            end if;
         end;
      end loop;
   end Scan_Requests;

   -----------------
   -- Set_Request --
   -----------------

   procedure Set_Request
     (Group    : City_Trade_Group;
      City     : Carthage.Handles.Cities.City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Resource_Quantity)
   is
      use type Carthage.Handles.Cities.City_Handle;
   begin
      for Item of Group.City_Requests loop
         if Item.City = City then
            Item.Request.Set_Quantity (Resource, Quantity);
            exit;
         end if;
      end loop;
   end Set_Request;

   ------------
   -- Update --
   ------------

   overriding function Update
     (Manager : not null access City_Manager_Record)
      return Duration
   is
      use Carthage.Handles.Cities;
      use Carthage.Handles.Structures;
      use Carthage.Handles.Resources;

      City      : constant City_Handle := Manager.City;
      Structure : constant Structure_Handle := Manager.Structure;

      procedure Save_Quantity
        (Resource : Resource_Handle;
         Quantity : Resource_Quantity);

      -------------------
      -- Save_Quantity --
      -------------------

      procedure Save_Quantity
        (Resource : Resource_Handle;
         Quantity : Resource_Quantity)
      is
      begin
         Manager.Available.Set_Quantity
           (Resource, Quantity);
      end Save_Quantity;

   begin

      Manager.Available.Clear;

      Manager.City.Scan_Stock (Save_Quantity'Access);

      if not Structure.Is_Harvester then

         for Item of Production_Inputs (Structure) loop
            declare
               Have : constant Resource_Quantity :=
                        City.Quantity (Item.Resource);
               Want : constant Resource_Quantity :=
                        2.0 * Item.Quantity;
            begin
               Set_Request
                 (Manager.Group, City, Item.Resource,
                  (if Have >= Want then 0.0 else Want - Have));
            end;
         end loop;
      end if;

      declare
         procedure Create_Goals
           (To_City : Carthage.Handles.Cities.City_Handle;
            Stock   : Resource_Stock);

         ------------------
         -- Create_Goals --
         ------------------

         procedure Create_Goals
           (To_City : Carthage.Handles.Cities.City_Handle;
            Stock   : Resource_Stock)
         is
         begin
            Manager.Log
              ("new goal: transfer resources to "
               & Description (To_City));

            declare
               procedure Show
                 (Resource : Carthage.Handles.Resources.Resource_Handle;
                  Quantity : Positive);

               ----------
               -- Show --
               ----------

               procedure Show
                 (Resource : Carthage.Handles.Resources.Resource_Handle;
                  Quantity : Positive)
               is
               begin
                  Manager.Log
                    (Resource.Tag & ":" & Quantity'Image);
               end Show;

            begin
               Stock.Scan_Stock
                 (Show'Access);
            end;

            Manager.Ground_Manager.Add_Goal
              (Carthage.Managers.Assets.Transfer_Cargo_Goal
                 (From     => City,
                  To       => To_City,
                  Cargo    => Stock));
         end Create_Goals;

      begin
         Manager.Create_Transfer_Goals (Create_Goals'Access);
      end;

      declare

         procedure Buy
           (Resource : Carthage.Handles.Resources.Resource_Handle;
            Quantity : Positive);

         procedure Sell
           (Resource : Carthage.Handles.Resources.Resource_Handle;
            Quantity : Positive);

         procedure Store
           (Resource : Carthage.Handles.Resources.Resource_Handle;
            Quantity : Positive);

         ---------
         -- Buy --
         ---------

         procedure Buy
           (Resource : Carthage.Handles.Resources.Resource_Handle;
            Quantity : Positive)
         is
         begin
            Carthage.Handles.Cities.Buy_Resource (City, Resource, Quantity);
            Manager.Ordered.Add (Resource, Quantity);
         end Buy;

         ----------
         -- Sell --
         ----------

         procedure Sell
           (Resource : Carthage.Handles.Resources.Resource_Handle;
            Quantity : Positive)
         is
         begin
            Sell_Resource (City, Resource, Quantity);
         end Sell;

         -----------
         -- Store --
         -----------

         procedure Store
           (Resource : Carthage.Handles.Resources.Resource_Handle;
            Quantity : Positive)
         is
         begin
            Manager.Log
              ("store" & Quantity'Image & " " & Resource.Tag);
            Transfer_Resource (City, Resource, Quantity, Manager.Palace);
         end Store;

      begin
         if Structure.Is_Harvester then
            if Manager.Palace.Has_Element then
               Manager.Available.Scan_Stock (Store'Access);
            else
               Manager.Available.Scan_Stock (Sell'Access);
            end if;
         else
            declare
               Inputs  : constant Production_Array :=
                           Manager.Structure.Production_Inputs;
               Outputs : constant Production_Array :=
                           Manager.Structure.Production_Outputs;
            begin
               for Item of Inputs loop
                  declare
                     Available : constant Natural :=
                                   Manager.Available.Whole_Quantity
                                     (Item.Resource);
                     Ordered   : constant Natural :=
                                   Manager.Ordered.Whole_Quantity
                                     (Item.Resource);
                     Pipeline  : constant Natural :=
                                   Available + Ordered;
                     Required  : constant Natural :=
                                   2 * Natural (Item.Quantity);
                  begin
                     if Pipeline < Required then
                        Buy (Item.Resource, Required - Pipeline);
                     end if;
                  end;
               end loop;

               for Item of Outputs loop
                  if Manager.Available.Quantity (Item.Resource) > 0.0 then
                     if Manager.Palace.Has_Element then
                        Store (Item.Resource,
                               Manager.Available.Whole_Quantity
                                 (Item.Resource));
                     else
                        Sell (Item.Resource,
                              Manager.Available.Whole_Quantity
                                (Item.Resource));
                     end if;
                  end if;
               end loop;
            end;
         end if;
      end;

      Carthage.Handles.Cities.Updates.Execute_City_Orders
        (City    => Manager.City,
         Manager => Manager);

      if Structure.Has_Production then
         Manager.Log ("executing production");
         Carthage.Handles.Cities.Updates.Execute_City_Production
           (City    => Manager.City,
            Manager => Manager);
      end if;

      return Manager.Average_Update_Frequency;
   end Update;

end Carthage.Managers.Cities;
