with Ada.Containers.Doubly_Linked_Lists;

with Carthage.Money;
with Carthage.Quantities;

with Carthage.Handles.Managers.Assets;
with Carthage.Handles.Managers.Resources;

with Carthage.Handles.Cities;
with Carthage.Handles.Houses.Updates;
with Carthage.Handles.Managers.Cities;
with Carthage.Handles.Resources;
with Carthage.Handles.Structures;
with Carthage.Handles.Tiles;

with Carthage.Goals.Transport;

package body Carthage.Handles.Managers.Planets is

   package City_Lists is
     new Ada.Containers.Doubly_Linked_Lists (City_Reference);

   type Planet_Manager_Record is
     new Root_Manager_Record with
      record
         Planet       : Planet_Reference;
         Owned_Cities : City_Lists.List;
         Agoras       : City_Lists.List;
      end record;

   overriding function Name
     (Manager : Planet_Manager_Record)
      return String
   is (House (Manager).Tag
       & "/" & Carthage.Handles.Planets.Get (Manager.Planet).Tag
       & " manager");

   overriding procedure On_Activated
     (Manager : in out Planet_Manager_Record);

   overriding procedure Register
     (Manager : Planet_Manager_Record);

   ---------------------------
   -- Create_Planet_Manager --
   ---------------------------

   procedure Create_Planet_Manager
     (Planet : Carthage.Handles.Planets.Planet_Handle;
      House  : Carthage.Handles.Houses.House_Handle)
   is

      function Owned_Cities return City_Lists.List;
      function Known_Agoras return City_Lists.List;

      ------------------
      -- Known_Agoras --
      ------------------

      function Known_Agoras return City_Lists.List is

         List : City_Lists.List;

         procedure Save_City
           (Reference : City_Reference);

         ---------------
         -- Save_City --
         ---------------

         procedure Save_City
           (Reference : City_Reference)
         is
            use Carthage.Handles.Cities, Carthage.Handles.Structures;
            Handle    : constant City_Handle := Get (Reference);
            Structure : constant Structure_Handle := Get (Handle.Structure);
         begin
            if Structure.Is_Agora
              and then Carthage.Handles.Tiles
                .Get (Handle.Tile).Explored_By (House)
            then
               List.Append (Reference);
            end if;
         end Save_City;

      begin
         Planet.For_All_Cities
           (Save_City'Access);
         return List;
      end Known_Agoras;

      ------------------
      -- Owned_Cities --
      ------------------

      function Owned_Cities return City_Lists.List is

         List : City_Lists.List;

         procedure Save_City
           (Reference : City_Reference);

         ---------------
         -- Save_City --
         ---------------

         procedure Save_City
           (Reference : City_Reference)
         is
            use Carthage.Handles.Cities, Carthage.Handles.Structures;
            Handle    : constant City_Handle := Get (Reference);
            Structure : constant Structure_Handle := Get (Handle.Structure);
         begin
            if not Structure.Is_Bonus then
               List.Append (Reference);
            end if;
         end Save_City;

      begin
         Planet.For_All_Owned_Cities
           (House.Reference, Save_City'Access);
         return List;
      end Owned_Cities;

      Rec       : Planet_Manager_Record;
      Handle    : Manager_Handle;

   begin
      Rec.Initialize
        (Class  => Ground_Transport,
         House  => House);

      Rec.Planet := Planet.Reference;
      Rec.Owned_Cities := Owned_Cities;
      Rec.Agoras := Known_Agoras;

      Handle := Rec.Create_Manager;

      for City of Rec.Owned_Cities loop
         Carthage.Handles.Managers.Cities.Create_City_Manager
           (House            => House,
            City             =>
              Carthage.Handles.Cities.Get (City),
            Resource_Manager => Handle);
      end loop;

      Carthage.Handles.Managers.Assets.Create_Ground_Asset_Manager
        (House     => House,
         Planet    => Planet,
         Transport => Handle);

      Carthage.Handles.Managers.Resources.Create_Ground_Resource_Manager
        (House  => House,
         Planet => Planet);

      Handle.Log ("created with" & Rec.Owned_Cities.Length'Image
                  & " cities");
   end Create_Planet_Manager;

   ------------------
   -- On_Activated --
   ------------------

   overriding procedure On_Activated
     (Manager : in out Planet_Manager_Record)
   is

      House : constant Carthage.Handles.Houses.House_Handle :=
                Carthage.Handles.Houses.Get (Manager.House);

      Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                 Carthage.Handles.Planets.Get (Manager.Planet);

      Palace : constant Carthage.Handles.Cities.City_Handle :=
                 (if Planet.Has_Palace
                  then Carthage.Handles.Cities.Get (Planet.Palace)
                  else Carthage.Handles.Cities.Empty_Handle);

      procedure Save_Available_Resource
        (Goal     : in out Carthage.Goals.Goal_Record'Class;
         Complete : out Boolean);

      procedure Update_Resource_Request
        (Goal     : in out Carthage.Goals.Goal_Record'Class;
         Complete : out Boolean);

      procedure Report_Resource
        (Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type);

      ---------------------
      -- Report_Resource --
      ---------------------

      procedure Report_Resource
        (Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type)
      is
      begin
         Manager.Log ("available:"
                      & Carthage.Quantities.Show (Quantity)
                      & " " & Resource.Local_Text);
      end Report_Resource;

      -----------------------------
      -- Save_Available_Resource --
      -----------------------------

      procedure Save_Available_Resource
        (Goal     : in out Carthage.Goals.Goal_Record'Class;
         Complete : out Boolean)
      is
         use Carthage.Goals.Transport;
      begin
         Manager.Add (Get_Resource (Goal), Get_Quantity (Goal));
         Complete := True;
      end Save_Available_Resource;

      -----------------------------
      -- Update_Resource_Request --
      -----------------------------

      procedure Update_Resource_Request
        (Goal     : in out Carthage.Goals.Goal_Record'Class;
         Complete : out Boolean)
      is
         use type Carthage.Handles.Houses.House_Handle;
         use Carthage.Money, Carthage.Quantities;
         use Carthage.Goals.Transport;
         Resource : constant Carthage.Handles.Resources.Resource_Handle :=
                      Get_Resource (Goal);
         Required  : constant Quantity_Type := Get_Quantity (Goal);
         Available : Quantity_Type := Manager.Quantity (Resource);
         Missing   : Quantity_Type :=
                       (if Available >= Required then Zero
                        else Required - Available);
      begin

         Manager.Remove
           (Resource, Min (Required, Available));

         if Palace.Has_Element and then Palace.Owner = House then
            declare
               Stored : constant Carthage.Quantities.Quantity_Type :=
                          Palace.Quantity (Resource);
               Taken  : constant Carthage.Quantities.Quantity_Type :=
                          (if Stored <= Missing
                           then Stored
                           else Missing);
            begin
               if Taken > Zero then
                  Manager.Log
                    ("take "
                     & Show (Taken)
                     & " " & Resource.Local_Text
                     & " from "
                     & Palace.Description);
                  Available := Available + Taken;
                  Missing := Missing - Taken;
                  Palace.Remove (Resource, Taken);
               end if;
            end;
         end if;

         for Agora_Reference of Manager.Agoras loop
            exit when Missing = Zero;

            declare
               Agora : constant Carthage.Handles.Cities.City_Handle :=
                         Carthage.Handles.Cities.Get (Agora_Reference);
               Want  : constant Quantity_Type := Missing;
               Have  : constant Quantity_Type :=
                         Agora.Quantity (Resource);
               Buy   : constant Quantity_Type := Min (Want, Have);
               Price : constant Price_Type :=
                         Agora.Agora_Sells_For (Resource);
               Cost  : constant Money_Type := Total (Price, Buy);
            begin
               if Buy > Zero then
                  if Agora.Owner /= House then
                     Manager.Log
                       (Agora.Description
                        & " sells "
                        & Show (Buy)
                        & " " & Resource.Local_Text
                        & " for " & Show (Cost)
                        & " (" & Show (Price) & " each");
                     Carthage.Handles.Houses.Updates.Execute_Payment
                       (From   => House,
                        To     => Agora.Owner,
                        Amount => Cost);
                     Agora.After_Agora_Buys (Resource, Buy);
                  else
                     Manager.Log
                       ("take "
                        & Carthage.Quantities.Show (Buy)
                        & " " & Resource.Local_Text
                        & " from "
                        & Agora.Description);
                  end if;

                  Agora.Remove (Resource, Buy);
                  Manager.Add (Resource, Buy);
                  Available := Available + Buy;
                  Missing := Missing - Buy;
               end if;
            end;
         end loop;

         declare
            Quantity : constant Quantity_Type := Min (Required, Available);
         begin
            if Quantity > Zero then
               Complete := Quantity = Get_Quantity (Goal);
               Set_Quantity (Goal, Get_Quantity (Goal) - Quantity);
               Get_Resource_Destination (Goal).Add_Resource
                 (Owner    => Carthage.Handles.Houses.Get (Manager.House),
                  Resource => Get_Resource (Goal),
                  Quantity => Quantity);

               Manager.Log
                 ("transfer "
                  & Carthage.Quantities.Show (Quantity)
                  & " " & Get_Resource (Goal).Local_Text
                  & " to "
                  & Get_Resource_Destination (Goal).Short_Name);

            end if;
         end;
      end Update_Resource_Request;

   begin

      Manager.Update_Goals
        (Test    => Carthage.Goals.Transport.Is_Resource_Available_Goal'Access,
         Process => Save_Available_Resource'Access);

      Manager.Scan_Stock (Report_Resource'Access);

      Manager.Update_Goals
        (Test     => Carthage.Goals.Transport.Is_Resource_Request_Goal'Access,
         Process  => Update_Resource_Request'Access);

   end On_Activated;

   --------------
   -- Register --
   --------------

   overriding procedure Register
     (Manager : Planet_Manager_Record)
   is
   begin
      Manager.Save_Manager (Planet => Manager.Planet);
   end Register;

end Carthage.Handles.Managers.Planets;
