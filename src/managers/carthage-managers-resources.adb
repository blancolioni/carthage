with Ada.Containers.Doubly_Linked_Lists;

with Carthage.Handles.Cities;
with Carthage.Handles.Resources;

with Carthage.Messages.Resources;

with Carthage.Money;

package body Carthage.Managers.Resources is

   procedure Update_Map
     (Map : in out Resource_Maps.Map;
      Message : Carthage.Messages.Resources.Resource_Message'Class);

   procedure Update_Quantity_Map
     (Map     : in out Tile_Quantity_Maps.Map;
      Message : Carthage.Messages.Resources.Resource_Message'Class);

   ------------------------------------
   -- Create_Planet_Resource_Manager --
   ------------------------------------

   procedure Create_Planet_Resource_Manager
     (House  : Carthage.Handles.Houses.House_Handle;
      Planet : Carthage.Handles.Planets.Planet_Handle)
   is
      Manager : constant Reference := new Instance'
        (Carthage.Managers.Instance with
         Planet => Planet,
         Resource_Task => new Resource_Manager_Task,
         Resource_Bank =>
           Carthage.Messages.Create_Message_Bank
             (House.Tag & "--" & Planet.Tag & "--resources"),
         Requirements  => <>,
         Supply        => <>);
   begin
      Manager.Initialize (House);
      Manager.Resource_Task.Start (Manager);
      Manager.Save_Manager;
   end Create_Planet_Resource_Manager;

   ----------
   -- Name --
   ----------

   overriding function Name (This : Instance) return String is
   begin
      return This.House.Tag & "/" & This.Planet.Tag & "/resources";
   end Name;

   -------------------------------
   -- Resolve_Resource_Requests --
   -------------------------------

   procedure Resolve_Resource_Requests (This : in out Instance'Class) is

      procedure Resolve
        (Resource     : Carthage.Handles.Resources.Resource_Handle;
         Requirements : in out Tile_Quantity_Maps.Map;
         Supply       : in out Tile_Quantity_Maps.Map);

      -------------
      -- Resolve --
      -------------

      procedure Resolve
        (Resource     : Carthage.Handles.Resources.Resource_Handle;
         Requirements : in out Tile_Quantity_Maps.Map;
         Supply       : in out Tile_Quantity_Maps.Map)
      is
         type Route_Record is
            record
               Distance : Natural;
               Requirement : Tile_Quantity_Maps.Cursor;
               Supply      : Tile_Quantity_Maps.Cursor;
            end record;

         function "<" (Left, Right : Route_Record) return Boolean
         is (Left.Distance < Right.Distance);

         package Route_Lists is
           new Ada.Containers.Doubly_Linked_Lists (Route_Record);

         package Sorting is
           new Route_Lists.Generic_Sorting ("<");

         Routes : Route_Lists.List;

      begin
         for Requirement_Position in Requirements.Iterate loop
            declare
               use Carthage.Quantities;
               use Carthage.Handles.Tiles;
               To       : constant Tile_Handle :=
                            Tile_Quantity_Maps.Key (Requirement_Position);
               Required : Resource_Record renames
                            Requirements (Requirement_Position);
            begin
               if Required.Quantity > Zero then
                  for Supply_Position in Supply.Iterate loop
                     declare
                        From     : constant Tile_Handle :=
                                     Tile_Quantity_Maps.Key (Supply_Position);
                        Supplied : Resource_Record renames
                                     Supply (Supply_Position);
                     begin
                        if Supplied.Quantity > Zero then
                           Routes.Append
                             (Route_Record'
                                (Distance    =>
                                     This.Planet.Hex_Distance (From, To),
                                 Requirement => Requirement_Position,
                                 Supply      => Supply_Position));
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end loop;
         Sorting.Sort (Routes);

         for Route of Routes loop
            declare
               use Carthage.Quantities;
               use Carthage.Handles.Tiles;
               From : constant Tile_Handle :=
                        Tile_Quantity_Maps.Key (Route.Supply);
               To   : constant Tile_Handle :=
                        Tile_Quantity_Maps.Key (Route.Requirement);
               Required : Resource_Record renames
                           Requirements (Route.Requirement);
               Supplied : Resource_Record renames
                           Supply (Route.Supply);
               Quantity : constant Quantity_Type :=
                            Min (Required.Quantity, Supplied.Quantity);
            begin
               if Quantity > Zero then
                  This.Log
                    ("ship " & Show (Quantity) & " " & Resource.Tag
                     & " from "
                     & Position_Image (From.Position)
                     & " to "
                     & Position_Image (To.Position)
                     & "; distance"
                     & Route.Distance'Image
                     & "; required " & Show (Required.Quantity)
                     & "; available " & Show (Supplied.Quantity));

                  From.Remove_Resource (This.House, Resource, Quantity);
                  Supplied.Quantity := @ - Quantity;
                  Supplied.Fulfilled := @ + Quantity;

                  To.Add_Resource (This.House, Resource, Quantity);
                  Required.Quantity := @ - Quantity;
                  Required.Fulfilled := @ + Quantity;
               end if;
            end;
         end loop;

         for Position in Requirements.Iterate loop
            declare
               use Carthage.Quantities;
               Rec : Resource_Record renames Requirements (Position);
               Tile     : constant Carthage.Handles.Tiles.Tile_Handle :=
                            Tile_Quantity_Maps.Key (Position);
            begin
               if Rec.Fulfilled < Rec.Minimum
                 and then Tile.Has_City
               then
                  declare
                     City : constant Carthage.Handles.Cities.City_Handle :=
                              Carthage.Handles.Cities.Get (Tile.Get_City);
                  begin
                     if not City.Is_Agora
                       and then City.Agora.Has_Element
                     then
                        declare
                           Price : constant Carthage.Money.Price_Type :=
                                     City.Agora.Agora_Buys_For
                                       (Resource);
                           Quantity : constant Quantity_Type :=
                                        Min (Rec.Minimum - Rec.Fulfilled,
                                             City.Agora.Quantity (Resource));
                        begin
                           City.Agora.Log
                             ("have "
                              & Show (City.Agora.Quantity (Resource))
                              & " " & Resource.Tag);
                           if Quantity > Zero then
                              This.House.Spend
                                (Carthage.Money.Total (Price, Quantity));
                              City.Agora.Owner.Earn
                                (Carthage.Money.Total (Price, Quantity));
                              Tile.Add_Resource
                                (This.House, Resource, Quantity);
                              City.Agora.Remove
                                (Resource, Quantity);
                              City.Agora.After_Agora_Sells
                                (Resource, Quantity);
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end;
         end loop;

      end Resolve;

   begin
      for Requirement_Position in This.Requirements.Iterate loop
         declare
            Tag : constant String :=
                    Resource_Maps.Key (Requirement_Position);
            Supply_Position : constant Resource_Maps.Cursor :=
                                This.Supply.Find (Tag);
         begin
            if Resource_Maps.Has_Element (Supply_Position) then
               Resolve (Carthage.Handles.Resources.Get (Tag),
                        This.Requirements (Requirement_Position),
                        This.Supply (Supply_Position));
            end if;
         end;
      end loop;
   end Resolve_Resource_Requests;

   ---------------------------
   -- Resource_Manager_Task --
   ---------------------------

   task body Resource_Manager_Task is
      Manager : Reference;
   begin
      accept Start (Manager_Ref : Reference) do
         Manager := Manager_Ref;
      end Start;

      loop
         declare
            use Carthage.Messages.Resources;
            Message : constant Carthage.Messages.Message_Interface'Class :=
                        Manager.Resource_Bank.Next_Message;
         begin
            Manager.Log ("received: " & Message.Description);
            if Is_Resource_Message (Message) then
               declare
                  M : Resource_Message'Class renames
                        Resource_Message'Class (Message);
               begin
                  if M.Is_Resource_Available_Message then
                     Update_Map (Manager.Supply, M);
                  elsif M.Is_Resource_Required_Message then
                     Update_Map (Manager.Requirements, M);
                  else
                     Manager.Log ("invalid message: " & M.Tag);
                  end if;
                  Manager.Resolve_Resource_Requests;
               end;
            else
               Manager.Log ("expected a resource message: " & Message.Tag);
            end if;
         end;
      end loop;
   end Resource_Manager_Task;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop (This : in out Instance) is
   begin
      abort This.Resource_Task.all;
   end Stop;

   ----------------
   -- Update_Map --
   ----------------

   procedure Update_Map
     (Map     : in out Resource_Maps.Map;
      Message : Carthage.Messages.Resources.Resource_Message'Class)
   is
      use Resource_Maps;
      Position : Cursor := Map.Find (Message.Resource.Tag);
      Inserted : Boolean;
   begin
      if not Has_Element (Position) then
         Map.Insert
           (Key      => Message.Resource.Tag,
            New_Item => Tile_Quantity_Maps.Empty_Map,
            Position => Position,
            Inserted => Inserted);
         pragma Assert (Inserted, "cannot insert to resource map");
      end if;

      Update_Quantity_Map
        (Map     => Map (Position),
         Message => Message);

   end Update_Map;

   -------------------------
   -- Update_Quantity_Map --
   -------------------------

   procedure Update_Quantity_Map
     (Map     : in out Tile_Quantity_Maps.Map;
      Message : Carthage.Messages.Resources.Resource_Message'Class)
   is
      use Tile_Quantity_Maps;
      Position : constant Cursor := Map.Find (Message.Tile);
   begin
      if Has_Element (Position) then
         declare
            Rec : Resource_Record renames Map (Position);
         begin
            Rec.Quantity := Message.Quantity;
            Rec.Minimum  := Message.Minimum;
         end;
      else
         Map.Insert
           (Key      => Message.Tile,
            New_Item => Resource_Record'
              (Quantity => Message.Quantity,
               Minimum  => Message.Minimum,
               Fulfilled => Carthage.Quantities.Zero));
      end if;
   end Update_Quantity_Map;

end Carthage.Managers.Resources;
