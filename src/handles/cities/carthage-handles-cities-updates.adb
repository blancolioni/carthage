with Carthage.Handles.Planets;
with Carthage.Handles.Resources;
with Carthage.Handles.Structures;
with Carthage.Handles.Tiles;

with Carthage.Handles.Houses.Updates;

package body Carthage.Handles.Cities.Updates is

   procedure Execute_Harvester_Production
     (City : City_Handle);

   -------------------------
   -- Execute_City_Orders --
   -------------------------

   procedure Execute_City_Orders
     (City : City_Handle;
      Manager : not null access City_Manager_Interface'Class)
   is

      procedure Execute_Order (Order : City_Order_Record);

      procedure Execute_Order (Order : City_Order_Record) is
         use Carthage.Handles.Resources;
      begin
         case Order.Class is
            when Buy =>
               declare
                  Agora          : constant City_Handle :=
                                     Get (Order.Other_City);
                  Has_Cost       : constant Boolean :=
                                     City.Owner.Tag /= Agora.Owner.Tag;
                  Resource       : constant Resource_Handle :=
                                     Get (Order.Resource);
                  Agora_Quantity : constant Natural :=
                                     Agora.Whole_Quantity (Resource);
                  Quantity       : constant Natural :=
                                     Natural'Min (Order.Quantity,
                                                  Agora_Quantity);
                  Cost           : constant Money_Type :=
                                     Money_Type (Quantity)
                                       * Agora.Agora_Sells_For (Resource);
               begin
                  if Quantity > 0 then
                     if Has_Cost then
                        City.Log
                          ("buy" & Quantity'Image & " "
                           & Resource.Local_Text
                           & " for" & Cost'Image);

                        Carthage.Handles.Houses.Updates.Execute_Payment
                          (From   => City.Owner,
                           To     => Agora.Owner,
                           Amount => Cost);

                        Carthage.Handles.Houses.Log_Status (City.Owner);
                        Carthage.Handles.Houses.Log_Status (Agora.Owner);

                        Agora.After_Agora_Transaction
                          (Resource        => Resource,
                           Quantity_Change => -Quantity);
                     end if;

                     Agora.Remove (Resource, Quantity);
                     City.Add (Resource, Quantity);

                     Manager.On_Resource_Arrival
                       (This     => City,
                        Resource => Resource,
                        Quantity => Quantity);
                  else
                     Log (City,
                          "buy" & Quantity'Image & " "
                          & Resource.Tag
                          & ": agora has none");

                  end if;
               end;

            when Sell =>

               declare
                  Agora          : constant City_Handle :=
                                     Get (Order.Other_City);
                  Has_Cost       : constant Boolean :=
                                     City.Owner.Tag /= Agora.Owner.Tag;
                  Resource       : constant Resource_Handle :=
                                     Get (Order.Resource);
                  City_Quantity : constant Natural :=
                                     City.Whole_Quantity (Resource);
                  Quantity       : constant Natural :=
                                     Natural'Min (Order.Quantity,
                                                  City_Quantity);
                  Cost           : constant Money_Type :=
                                     Money_Type (Quantity)
                                       * Agora.Agora_Buys_For (Resource);
               begin
                  if Quantity > 0 then
                     if Has_Cost then
                        City.Log
                          ("sell" & Quantity'Image & " "
                           & Resource.Local_Text
                           & " for" & Cost'Image);

                        Carthage.Handles.Houses.Updates.Execute_Payment
                          (From   => Agora.Owner,
                           To     => City.Owner,
                           Amount => Cost);

                        Carthage.Handles.Houses.Log_Status (City.Owner);
                        Carthage.Handles.Houses.Log_Status (Agora.Owner);

                        Agora.After_Agora_Transaction
                          (Resource        => Resource,
                           Quantity_Change => Quantity);
                     end if;

                     Agora.Add (Resource, Quantity);
                     City.Remove (Resource, Quantity);

                  else
                     Log (City,
                          "sell" & Quantity'Image & " "
                          & Resource.Tag
                          & ": city has none");

                  end if;
               end;

            when Transfer =>

               declare
                  Resource       : constant Resource_Handle :=
                                     Get (Order.Resource);
                  City_Quantity  : constant Natural :=
                                     City.Whole_Quantity (Resource);
                  Quantity       : constant Natural :=
                                     Natural'Min (Order.Quantity,
                                                  City_Quantity);
                  Destination    : constant City_Handle :=
                                     Get (Order.Other_City);
               begin
                  City.Log
                    ("transfer" & Quantity'Image & " "
                     & Resource.Tag
                     & " to " & Destination.Description);
                  Destination.Add (Resource, Quantity);
                  City.Remove (Resource, Quantity);
               end;
         end case;
      end Execute_Order;

   begin
      City.Process_Orders (Execute_Order'Access);
   end Execute_City_Orders;

   -----------------------------
   -- Execute_City_Production --
   -----------------------------

   procedure Execute_City_Production
     (City    : City_Handle;
      Manager : not null access City_Manager_Interface'Class)
   is
      pragma Unreferenced (Manager);

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
      begin
         if Quantity > 0.0 then
            Log
              (City, Resource.Tag & ":" & Quantity'Image);
         end if;
      end Report_Stock;

   begin
      if Structure.Is_Harvester then
         Execute_Harvester_Production (City);
      else
         City.Execute_Production
           (Efficiency =>
              Float (City.Loyalty) / 100.0
            * Float (City.Health) / 100.0,
            Factor     => 0.1);
      end if;
      Carthage.Handles.Resources.Scan_Stock (City, Report_Stock'Access);
   end Execute_City_Production;

   ----------------------------------
   -- Execute_Harvester_Production --
   ----------------------------------

   procedure Execute_Harvester_Production
     (City : City_Handle)
   is
      use Carthage.Handles.Planets;
      Tiles  : Surface_Tiles;
      Planet : constant Planet_Handle := Get (City.Planet);
      Structure : constant Carthage.Handles.Structures.Structure_Handle :=
                    Carthage.Handles.Structures.Get (City.Structure);
   begin
      Planet.Get_Tiles
        (Origin       => Carthage.Handles.Tiles.Get (City.Tile),
         Min_Distance => 0,
         Max_Distance => Structure.Radius + 1,
         Test         => null,
         Tiles        => Tiles);

      for I in 1 .. Tile_Count (Tiles) loop
         declare
            Prod : constant Carthage.Handles.Structures.Production_Array :=
                     Structure.Harvest_Production
                       (Get_Tile (Tiles, I).Reference);
         begin
            for Rec of Prod loop
               declare
                  Quantity : constant Carthage.Quantities.Quantity_Type :=
                               Carthage.Quantities.Quantity_Type
                                 (Float (Rec.Quantity)
                                  * Float (City.Health) / 100.0
                                  * Float (City.Loyalty) / 100.0
                                  / 30.0);
               begin
                  City.Log ("harvested" & Quantity'Image & " "
                            & Rec.Resource.Local_Text);
                  City.Add (Rec.Resource, Quantity);
               end;
            end loop;
         end;
      end loop;

   end Execute_Harvester_Production;

end Carthage.Handles.Cities.Updates;
