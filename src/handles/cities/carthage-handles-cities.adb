with Ada.Containers.Vectors;

with Carthage.Handles.Planets;
with Carthage.Handles.Structures;
with Carthage.Handles.Tiles;
with Carthage.Handles.Vectors;

with Carthage.Messages.Resources;

package body Carthage.Handles.Cities is

   package City_Vectors is
     new Carthage.Handles.Vectors
       (Real_City_Reference, City_Record, "city");

   City_Vector : City_Vectors.Vector;

   function Get
     (Handle : City_Handle)
      return City_Vectors.Constant_Reference_Type
   is (City_Vector (Handle.Reference));

   package Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (City_Reference);

   Harvester_List : Reference_Lists.List;

   package Production_Vectors is
     new Ada.Containers.Vectors
       (Real_Resource_Reference, Reference_Lists.List, Reference_Lists."=");

   Production_Vector : Production_Vectors.Vector;

   overriding function Identifier
     (City : City_Handle)
      return Object_Identifier
   is (Get (City).Identifier);

   overriding function Short_Name
     (City : City_Handle)
      return String
   is (City.Owner.Tag
       & "-"
       & Carthage.Handles.Planets.Get (City.Planet).Tag
       & "-"
       & Carthage.Handles.Structures.Get (City.Structure).Tag
       & "-"
       & Carthage.Handles.Tiles.Position_Image
         (Carthage.Handles.Tiles.Get (Get (City).Tile).Position));

   function Is_Agora
     (This : City_Handle)
      return Boolean
   is (Carthage.Handles.Structures.Get (Get (This).Structure).Is_Agora);

   function Owner
     (City : City_Handle)
      return Carthage.Handles.Houses.House_Handle
   is (Carthage.Handles.Houses.Get (Get (City).Owner));

   function Structure
     (City : City_Handle)
      return Structure_Reference
   is (Get (City).Structure);

   function Tile
     (City : City_Handle)
      return Tile_Reference
   is (Get (City).Tile);

   function Planet
     (City : City_Handle)
      return Planet_Reference
   is (Get (City).Planet);

   function Agora
     (City : City_Handle)
      return City_Handle
   is (Get (Get (City).Agora));

   function Loyalty
     (City : City_Handle)
      return Loyalty_Type
   is (Get (City).Loyalty);

   function Health
     (City : City_Handle)
      return Health_Type
   is (Get (City).Health);

   function Agora_Buys_For
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Carthage.Money.Price_Type
   is (Get (This).Prices (Resource.Reference).Buy_Price);

   function Agora_Sells_For
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Carthage.Money.Price_Type
   is (Get (This).Prices (Resource.Reference).Sell_Price);

   procedure Add_Order
     (City      : City_Handle;
      Order     : City_Order_Record);

   ---------------
   -- Add_Order --
   ---------------

   procedure Add_Order
     (City      : City_Handle;
      Order     : City_Order_Record)
   is
      procedure Update (Rec : in out City_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out City_Record) is
      begin
         Rec.Orders.Append (Order);
      end Update;

   begin
      City_Vector.Update (City.Reference, Update'Access);
   end Add_Order;

   ----------------------
   -- After_Agora_Buys --
   ----------------------

   procedure After_Agora_Buys
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
   is

      use type Carthage.Quantities.Quantity_Type;

      procedure Update (Rec : in out City_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out City_Record) is
         use Carthage.Money;
         Buy_Price  : Price_Type renames
                        Rec.Prices (Resource.Reference).Buy_Price;
         Sell_Price : Price_Type renames
                        Rec.Prices (Resource.Reference).Sell_Price;
      begin
         Buy_Price :=
           Max (Adjust_Price (Buy_Price, 0.9),
                To_Price (1.0));
         Sell_Price :=
           Max (Adjust_Price (Sell_Price, 1.1),
                Adjust_Price (Buy_Price, 1.1));
      end Update;
   begin
      if False then
         if Quantity /= Carthage.Quantities.Zero then
            City_Vector.Update (This.Reference, Update'Access);

            This.Log
              ("price of "
               & Resource.Tag
               & " after buying "
               & Carthage.Quantities.Show (Quantity)
               & " now "
               & Carthage.Money.Show (This.Agora_Buys_For (Resource))
               & " buy, "
               & Carthage.Money.Show (This.Agora_Sells_For (Resource))
               & " sell");

         end if;
      end if;
   end After_Agora_Buys;

   -----------------------
   -- After_Agora_Sells --
   -----------------------

   procedure After_Agora_Sells
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
   is

      use type Carthage.Quantities.Quantity_Type;

      procedure Update (Rec : in out City_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out City_Record) is
         use Carthage.Money;
         Buy_Price  : Price_Type renames
                        Rec.Prices (Resource.Reference).Buy_Price;
         Sell_Price : Price_Type renames
                        Rec.Prices (Resource.Reference).Sell_Price;
      begin
         Buy_Price := Adjust_Price (Buy_Price, 1.1);
         Sell_Price := Max (Adjust_Price (Sell_Price, 1.1),
                            Adjust_Price (Buy_Price, 1.1));

         Log
           (This,
            "price of "
            & Resource.Tag
            & " after selling "
            & Carthage.Quantities.Show (Quantity)
            & " now "
            & Show (Buy_Price)
            & " buy, "
            & Show (Sell_Price)
            & " sell");
      end Update;
   begin
      if False then
         if Quantity /= Carthage.Quantities.Zero then
            City_Vector.Update (This.Reference, Update'Access);
         end if;
      end if;
   end After_Agora_Sells;

   ------------------
   -- Buy_Resource --
   ------------------

   procedure Buy_Resource
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Positive)
   is
   begin
      if Get (This).Agora /= Null_City_Reference then
         Log (This, "buy" & Quantity'Image & " " & Resource.Tag);

         Add_Order
           (City     => This,
            Order    => City_Order_Record'
              (Class      => Buy,
               Other_City => Get (This).Agora,
               Resource   => Resource.Reference,
               Quantity   => Quantity));
      end if;
   end Buy_Resource;

   -----------------
   -- Create_City --
   -----------------

   function Create_City (Rec : City_Record) return City_Reference is
      Reference : City_Reference;
   begin
      City_Vector.Append (Rec, Reference);
      if Carthage.Handles.Structures.Get (Rec.Structure).Is_Harvester then
         Harvester_List.Append (Reference);
      end if;
      return Reference;
   end Create_City;

   -----------------
   -- Description --
   -----------------

   function Description (City : City_Handle) return String is
   begin
      return City.Owner.Tag & "-"
        & Carthage.Handles.Structures.Get (City.Structure).Tag;
   end Description;

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (This       : City_Handle;
      Efficiency : Unit_Real;
      Factor     : Unit_Real)
   is
      Structure : constant Carthage.Handles.Structures.Structure_Handle :=
                    Carthage.Handles.Structures.Get (This.Structure);
      Resource  : constant Carthage.Handles.Resources.Resource_Handle :=
                    Structure.Production_Output;
      Quantity  : constant Carthage.Quantities.Quantity_Type :=
                    Structure.Execute_Production
                      (Stock      => This,
                       House      => This.Owner.Reference,
                       Tile       => This.Tile,
                       Efficiency => Efficiency,
                       Factor     => Factor);
   begin
      This.Add (Resource, Quantity);
      declare
         Message : constant Carthage.Messages.Message_Interface'Class :=
                     Carthage.Messages.Resources.Available
                       (House    => This.Owner,
                        Tile     =>
                          Carthage.Handles.Tiles.Get (This.Tile),
                        Resource => Resource,
                        Quantity => Quantity);
      begin
         Message.Send;
      end;
   end Execute_Production;

   --------------------
   -- For_All_Cities --
   --------------------

   procedure For_All_Cities
     (Process : not null access procedure (This : City_Handle))
   is
   begin
      for Reference in 1 .. City_Vector.Last_Index loop
         Process (Get (Reference));
      end loop;
   end For_All_Cities;

   -----------------------------------
   -- For_All_Cities_With_Structure --
   -----------------------------------

   procedure For_All_Cities_With_Structure
     (Structure : Structure_Reference;
      Process   : not null access procedure (This : City_Handle))
   is
   begin
      for Reference in 1 .. City_Vector.Last_Index loop
         if Get (Reference).Structure = Structure then
            Process (Get (Reference));
         end if;
      end loop;
   end For_All_Cities_With_Structure;

   ------------------------
   -- For_All_Harvesters --
   ------------------------

   procedure For_All_Harvesters
     (Process : not null access procedure (This : City_Handle))
   is
   begin
      for Reference of Harvester_List loop
         Process (Get (Reference));
      end loop;
   end For_All_Harvesters;

   -----------------------
   -- For_All_Producers --
   -----------------------

   procedure For_All_Producers
     (Resource  : Carthage.Handles.Resources.Resource_Handle;
      Process   : not null access procedure (This : City_Handle))
   is
   begin
      for Reference of Production_Vector (Resource.Reference) loop
         Process (Get (Reference));
      end loop;
   end For_All_Producers;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
      procedure Add_Empty_List
        (For_Resource : Carthage.Handles.Resources.Resource_Handle);

      --------------------
      -- Add_Empty_List --
      --------------------

      procedure Add_Empty_List
        (For_Resource : Carthage.Handles.Resources.Resource_Handle)
      is
         pragma Unreferenced (For_Resource);
      begin
         Production_Vector.Append (Reference_Lists.Empty_List);
      end Add_Empty_List;

   begin
      City_Vector.Read (Stream);

      Carthage.Handles.Resources.For_All_Resources
        (Add_Empty_List'Access);

      for Reference in 1 .. City_Vector.Last_Index loop
         declare
            Structure : constant Carthage.Handles.Structures.Structure_Handle
              := Carthage.Handles.Structures.Get (Get (Reference).Structure);
         begin
            if Structure.Is_Harvester then
               Harvester_List.Append (Reference);
            elsif Structure.Production_Output.Has_Element then
               Production_Vector (Structure.Production_Output.Reference)
                 .Append (Reference);
            end if;
         end;
      end loop;
   end Load;

   ----------
   -- Look --
   ----------

   procedure Look
     (This : City_Handle)
   is
      Spot : constant Positive := 5;

      procedure Reveal_Tile (Tile : Carthage.Handles.Tiles.Tile_Handle);

      -----------------
      -- Reveal_Tile --
      -----------------

      procedure Reveal_Tile
        (Tile : Carthage.Handles.Tiles.Tile_Handle)
      is
      begin
         Tile.Set_Currently_Visible_To (This.Owner);
      end Reveal_Tile;

      Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                 Carthage.Handles.Planets.Get (Get (This).Planet);
      Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
               Carthage.Handles.Tiles.Get (This.Tile);
   begin
      This.Log ("looking");
      Planet.Scan_Neighbours_Within
        (Start    => Tile.Position,
         Distance => Spot,
         Process  => Reveal_Tile'Access);
   end Look;

   --------------------
   -- Process_Orders --
   --------------------

   procedure Process_Orders
     (City    : City_Handle'Class;
      Process : not null access
        procedure (Order : City_Order_Record))
   is
      Orders : constant City_Order_Lists.List :=
                 Get (City_Handle (City)).Orders;

      procedure Update (Rec : in out City_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out City_Record) is
      begin
         Rec.Orders.Clear;
      end Update;

   begin
      City_Vector.Update (City.Reference, Update'Access);
      for Order of Orders loop
         Process (Order);
      end loop;
   end Process_Orders;

   --------------
   -- Quantity --
   --------------

   overriding function Quantity
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type
   is (Carthage.Handles.Tiles.Get (This.Tile).Resource_Quantity
       (This.Owner, Resource));

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      City_Vector.Write (Stream);
   end Save;

   ------------------------
   -- Scan_Harvest_Tiles --
   ------------------------

   procedure Scan_Harvest_Tiles
     (This : City_Handle;
      Process   : not null access
        procedure (Tile : Tile_Reference))
   is
   begin
      for Tile of Get (This).Harvest_Tiles loop
         Process (Tile);
      end loop;
   end Scan_Harvest_Tiles;

   -------------
   -- Seen_By --
   -------------

   function Seen_By
     (This  : City_Handle;
      House : Carthage.Handles.Houses.House_Handle)
      return Boolean
   is
   begin
      return House.Is_Element_Of (Get (This).Seen_By);
   end Seen_By;

   -------------------
   -- Sell_Resource --
   -------------------

   procedure Sell_Resource
     (This     : City_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Positive)
   is
   begin
      if Get (This).Agora /= Null_City_Reference then
         Add_Order
           (City  => This,
            Order => City_Order_Record'
              (Class      => Sell,
               Other_City => Get (This).Agora,
               Resource   => Resource.Reference,
               Quantity   => Quantity));
      end if;
   end Sell_Resource;

   ---------------
   -- Set_Agora --
   ---------------

   procedure Set_Agora
     (This     : City_Handle;
      Agora    : City_Handle)
   is
      procedure Update (Rec : in out City_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out City_Record) is
      begin
         Rec.Agora := Agora.Reference;
      end Update;

   begin
      City_Vector.Update (This.Reference, Update'Access);
   end Set_Agora;

   -----------------
   -- Set_Manager --
   -----------------

   procedure Set_Manager
     (This : City_Handle;
      Manager : not null access City_Manager_Interface'Class)
   is
   begin
      null;
   end Set_Manager;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (This     : City_Handle;
      Item     : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
   is
   begin
      Carthage.Handles.Tiles.Get (This.Tile)
        .Set_Resource_Quantity (This.Owner, Item, Quantity);
   end Set_Quantity;

   -----------------
   -- Set_Seen_By --
   -----------------

   procedure Set_Seen_By
     (This  : City_Handle;
      House : Carthage.Handles.Houses.House_Handle)
   is
      procedure Update (Rec : in out City_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out City_Record) is
      begin
         House.Include (Rec.Seen_By);
      end Update;

   begin
      City_Vector.Update (This.Reference, Update'Access);
   end Set_Seen_By;

end Carthage.Handles.Cities;
