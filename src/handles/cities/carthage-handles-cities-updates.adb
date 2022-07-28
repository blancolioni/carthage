with Carthage.Handles.Resources;
with Carthage.Handles.Structures;
with Carthage.Handles.Tiles;

with Carthage.Messages.Resources;

with Carthage.Settings;

--  with Carthage.Handles.Houses.Updates;

package body Carthage.Handles.Cities.Updates is

   Log_Harvest : constant Boolean := True;

   ------------------------------
   -- Execute_City_Consumption --
   ------------------------------

   procedure Execute_City_Consumption
     (City : City_Handle)
   is

      procedure Execute (Rec : in out City_Record);

      -------------
      -- Execute --
      -------------

      procedure Execute (Rec : in out City_Record) is
         use Carthage.Quantities;
         Resource : constant Carthage.Handles.Resources.Resource_Handle :=
                      Carthage.Handles.Resources.Food;
         Tile     : constant Carthage.Handles.Tiles.Tile_Handle :=
                      Carthage.Handles.Tiles.Get (Rec.Tile);
         House    : constant Carthage.Handles.Houses.House_Handle :=
                      Carthage.Handles.Houses.Get (Rec.Owner);
         Require  : constant Quantity_Type :=
                      Carthage.Settings.Daily_City_Food;
         Like     : constant Quantity_Type := Scale (Require, 10.0);
         Minimum  : constant Quantity_Type := Scale (Require, 5.0);
         Have     : Quantity_Type;
         Need     : Quantity_Type;
      begin

         Tile.Take_Resource (House, Resource, Require, Have);
         Need := (if Have < Require then Require - Have else Zero);

         Tile.Log
           ("food: require " & Show (Require)
            & "; want " & Show (Like)
            & "; have " & Show (Have)
            & "; need " & Show (Need));

         if Have > Zero then
            City.Owner.Consume_Resource
              (City.Planet, Resource.Reference, Have);
         end if;

         if Have < Minimum then
            Tile.Log ("requesting " & Show (Like - Have) & " food");
            declare
               Message : constant Carthage.Messages.Message_Interface'Class :=
                           Carthage.Messages.Resources.Required
                             (House    => House,
                              Tile     => Tile,
                              Resource => Resource,
                              Quantity => Like - Have,
                              Minimum  => Need);
            begin
               Message.Send;
            end;
         end if;
      end Execute;

   begin
      Update_City_Record (City, Execute'Access);
   end Execute_City_Consumption;

   -------------------------
   -- Execute_City_Orders --
   -------------------------

   procedure Execute_City_Orders
     (City    : City_Handle;
      Manager : not null access City_Manager_Interface'Class)
   is null;

   --  procedure Execute_City_Orders
   --    (City : City_Handle;
   --     Manager : not null access City_Manager_Interface'Class)
   --  is
   --
   --     procedure Execute_Order (Order : City_Order_Record);
   --
   --     procedure Execute_Order (Order : City_Order_Record) is
   --        use Carthage.Handles.Resources;
   --     begin
   --        case Order.Class is
   --           when Buy =>
   --              declare
   --                 Agora          : constant City_Handle :=
   --                                    Get (Order.Other_City);
   --                 Has_Cost       : constant Boolean :=
   --                                    City.Owner.Tag /= Agora.Owner.Tag;
   --                 Resource       : constant Resource_Handle :=
   --                                    Get (Order.Resource);
   --                 Agora_Quantity : constant Natural :=
   --                                    Agora.Whole_Quantity (Resource);
   --                 Quantity       : constant Natural :=
   --                                    Natural'Min (Order.Quantity,
   --                                                 Agora_Quantity);
   --                 Cost           : constant Money_Type :=
   --                                    Money_Type (Quantity)
   --                                      * Agora.Agora_Sells_For (Resource);
   --              begin
   --                 if Quantity > 0 then
   --                    if Has_Cost then
   --                       City.Log
   --                         ("buy" & Quantity'Image & " "
   --                          & Resource.Local_Text
   --                          & " for" & Cost'Image);
   --
   --                       Carthage.Handles.Houses.Updates.Execute_Payment
   --                         (From   => City.Owner,
   --                          To     => Agora.Owner,
   --                          Amount => Cost);
   --
   --                       Carthage.Handles.Houses.Log_Status (City.Owner);
   --                       Carthage.Handles.Houses.Log_Status (Agora.Owner);
   --
   --                       Agora.After_Agora_Transaction
   --                         (Resource        => Resource,
   --                          Quantity_Change => -Quantity);
   --                    end if;
   --
   --                    Agora.Remove (Resource, Quantity);
   --                    City.Add (Resource, Quantity);
   --
   --                    Manager.On_Resource_Arrival
   --                      (This     => City,
   --                       Resource => Resource,
   --                       Quantity => Quantity);
   --                 else
   --                    Log (City,
   --                         "buy" & Quantity'Image & " "
   --                         & Resource.Tag
   --                         & ": agora has none");
   --
   --                 end if;
   --              end;
   --
   --           when Sell =>
   --
   --              declare
   --                 Agora          : constant City_Handle :=
   --                                    Get (Order.Other_City);
   --                 Has_Cost       : constant Boolean :=
   --                                    City.Owner.Tag /= Agora.Owner.Tag;
   --                 Resource       : constant Resource_Handle :=
   --                                    Get (Order.Resource);
   --                 City_Quantity : constant Natural :=
   --                                    City.Whole_Quantity (Resource);
   --                 Quantity       : constant Natural :=
   --                                    Natural'Min (Order.Quantity,
   --                                                 City_Quantity);
   --                 Cost           : constant Money_Type :=
   --                                    Money_Type (Quantity)
   --                                      * Agora.Agora_Buys_For (Resource);
   --              begin
   --                 if Quantity > 0 then
   --                    if Has_Cost then
   --                       City.Log
   --                         ("sell" & Quantity'Image & " "
   --                          & Resource.Local_Text
   --                          & " for" & Cost'Image);
   --
   --                       Carthage.Handles.Houses.Updates.Execute_Payment
   --                         (From   => Agora.Owner,
   --                          To     => City.Owner,
   --                          Amount => Cost);
   --
   --                       Carthage.Handles.Houses.Log_Status (City.Owner);
   --                       Carthage.Handles.Houses.Log_Status (Agora.Owner);
   --
   --                       Agora.After_Agora_Transaction
   --                         (Resource        => Resource,
   --                          Quantity_Change => Quantity);
   --                    end if;
   --
   --                    Agora.Add (Resource, Quantity);
   --                    City.Remove (Resource, Quantity);
   --
   --                 else
   --                    Log (City,
   --                         "sell" & Quantity'Image & " "
   --                         & Resource.Tag
   --                         & ": city has none");
   --
   --                 end if;
   --              end;
   --
   --           when Transfer =>
   --
   --              declare
   --                 Resource       : constant Resource_Handle :=
   --                                    Get (Order.Resource);
   --                 City_Quantity  : constant Natural :=
   --                                    City.Whole_Quantity (Resource);
   --                 Quantity       : constant Natural :=
   --                                    Natural'Min (Order.Quantity,
   --                                                 City_Quantity);
   --                 Destination    : constant City_Handle :=
   --                                    Get (Order.Other_City);
   --              begin
   --                 City.Log
   --                   ("transfer" & Quantity'Image & " "
   --                    & Resource.Tag
   --                    & " to " & Destination.Description);
   --                 Destination.Add (Resource, Quantity);
   --                 City.Remove (Resource, Quantity);
   --              end;
   --        end case;
   --     end Execute_Order;
   --
   --  begin
   --     City.Process_Orders (Execute_Order'Access);
   --  end Execute_City_Orders;

   -----------------------------
   -- Execute_City_Production --
   -----------------------------

   procedure Execute_City_Production
     (City    : City_Handle)
   is
      Structure : constant Carthage.Handles.Structures.Structure_Handle :=
                    Carthage.Handles.Structures.Get (City.Structure);

      procedure Report_Stock
        (Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type);

      ------------------
      -- Report_Stock --
      ------------------

      procedure Report_Stock
        (Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type)
      is
         use Carthage.Quantities;
      begin
         if Quantity > Carthage.Quantities.Zero then
            Log
              (City, Resource.Tag & ":" & Show (Quantity));
         end if;
      end Report_Stock;

   begin
      if Structure.Is_Harvester then
         Execute_Harvester_Production (City);
      else
         City.Execute_Production
           (Efficiency =>
              Real (City.Loyalty) / 100.0
            * Real (City.Health) / 100.0,
            Factor     => Carthage.Settings.Daily_Production_Factor);
      end if;
      Carthage.Handles.Resources.Scan_Stock (City, Report_Stock'Access);
   end Execute_City_Production;

   ----------------------------------
   -- Execute_Harvester_Production --
   ----------------------------------

   procedure Execute_Harvester_Production
     (City : City_Handle)
   is
      Structure : constant Carthage.Handles.Structures.Structure_Handle :=
                    Carthage.Handles.Structures.Get (City.Structure);

      Harvest : Carthage.Handles.Resources.Resource_Stock;

      procedure Harvest_Tile (Tile : Tile_Reference);

      ------------------
      -- Harvest_Tile --
      ------------------

      procedure Harvest_Tile (Tile : Tile_Reference) is
         Prod : constant Carthage.Handles.Structures.Production_Array :=
                  Structure.Harvest_Production (Tile);
      begin
         for Rec of Prod loop
            declare
               use Carthage.Quantities;
               Quantity : constant Quantity_Type :=
                            Carthage.Quantities.Scale
                              (Rec.Quantity,
                               Real (City.Health) / 100.0
                               * Real (City.Loyalty) / 100.0
                               * Carthage.Settings.Daily_Harvest_Factor);
            begin
               Harvest.Add (Rec.Resource, Quantity);
            end;
         end loop;
      end Harvest_Tile;

   begin
      City.Scan_Harvest_Tiles (Harvest_Tile'Access);

      declare
         procedure Add_Stock
           (Resource : Carthage.Handles.Resources.Resource_Handle;
            Quantity : Carthage.Quantities.Quantity_Type);

         ---------------
         -- Add_Stock --
         ---------------

         procedure Add_Stock
           (Resource : Carthage.Handles.Resources.Resource_Handle;
            Quantity : Carthage.Quantities.Quantity_Type)
         is
         begin
            City.Add (Resource, Quantity);
            if Log_Harvest then
               City.Log ("harvested "
                         & Carthage.Quantities.Show (Quantity) & " "
                         & Resource.Local_Text
                         & "; total "
                         & Carthage.Quantities.Show
                           (City.Quantity (Resource)));
            end if;

            City.Owner.Produce_Resource
              (City.Planet, Resource.Reference, Quantity);

            declare
               Message : constant Carthage.Messages.Message_Interface'Class :=
                           Carthage.Messages.Resources.Available
                             (House    => City.Owner,
                              Tile     =>
                                Carthage.Handles.Tiles.Get (City.Tile),
                              Resource => Resource,
                              Quantity => City.Quantity (Resource));
            begin
               Message.Send;
            end;

         end Add_Stock;

      begin
         Harvest.Scan_Stock (Add_Stock'Access);
      end;

   end Execute_Harvester_Production;

end Carthage.Handles.Cities.Updates;
