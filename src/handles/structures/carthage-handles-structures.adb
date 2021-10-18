with WL.String_Maps;

with Carthage.Handles.Terrain;
with Carthage.Handles.Tiles;

package body Carthage.Handles.Structures is

   Log_Production : constant Boolean := False;

   package Structure_Vectors is
     new Ada.Containers.Vectors (Real_Structure_Reference, Structure_Record);

   package Structure_Maps is
     new WL.String_Maps (Structure_Reference);

   Structure_Vector : Structure_Vectors.Vector;
   Structure_Map : Structure_Maps.Map;

   function Get
     (Handle : Structure_Handle)
      return Structure_Vectors.Constant_Reference_Type
   is (Structure_Vector (Handle.Reference));

   function Exists (Tag : String) return Boolean
   is (Structure_Map.Contains (Tag));

   function Get (Tag : String) return Structure_Handle
   is (Get (Structure_Map (Tag)));

   overriding function Tag
     (Handle : Structure_Handle)
      return String
   is (-Get (Handle).Tag);

   function Radius
     (This      : Structure_Handle)
      return Natural
   is (Get (This).Area);

   function Is_Harvester
     (This      : Structure_Handle)
      return Boolean
   is (Get (This).Is_Harvester);

   function Is_Agora
     (This      : Structure_Handle)
      return Boolean
   is (Get (This).Agora);

   function Is_Bonus
     (This      : Structure_Handle)
      return Boolean
   is (Get (This).Is_Bonus);

   function Is_Palace
     (This      : Structure_Handle)
      return Boolean
   is (Get (This).Palace);

   function Is_Shield
     (This      : Structure_Handle)
      return Boolean
   is (Get (This).Shield);

   function Has_Production
     (This    : Structure_Handle)
      return Boolean
   is (Carthage.Handles.Resources.Get
       (Get (This).Production_Output)
       .Has_Element);

   function To_Production_Array
     (This : Carthage.Handles.Resources.Stock_Interface'Class)
      return Production_Array;

   function Production_Inputs
     (This    : Structure_Handle)
      return Production_Array
   is (To_Production_Array (Get (This).Production_Inputs));

   function Production_Output
     (This    : Structure_Handle)
      return Carthage.Handles.Resources.Resource_Handle
   is (Carthage.Handles.Resources.Get
       (Get (This).Production_Output));

   function Production_Quantity
     (This    : Structure_Handle)
      return Carthage.Quantities.Quantity_Type
   is (Get (This).Production_Quantity);

   function Produces
     (This      : Structure_Handle;
      Resource  : Carthage.Handles.Resources.Resource_Handle)
      return Boolean
   is (Resource.Reference = Get (This).Production_Output);

   function Consumes
     (This      : Structure_Handle;
      Resource  : Carthage.Handles.Resources.Resource_Handle)
      return Boolean
   is (Carthage.Quantities.">"
       (Get (This).Production_Inputs.Quantity (Resource),
          Carthage.Quantities.Zero));

   --------------------------
   -- Add_Bonus_Production --
   --------------------------

   procedure Add_Bonus_Production
     (Structure : Structure_Handle;
      Bonus     : Structure_Reference;
      Resource  : Resource_Reference;
      Quantity  : Natural)
   is
   begin
      Structure_Vector (Structure.Reference).Bonus_Production.Append
        (Bonus_Production_Record'
           (Bonus    => Bonus,
            Resource => Resource,
            Quantity => Quantity));
   end Add_Bonus_Production;

   ------------
   -- Create --
   ------------

   procedure Create (Structure : Structure_Record) is
   begin
      Structure_Vector.Append (Structure);
      Structure_Map.Insert (-Structure.Tag, Structure_Vector.Last_Index);
   end Create;

   ------------------------
   -- Execute_Production --
   ------------------------

   function Execute_Production
     (This       : Structure_Handle;
      Stock      : Carthage.Handles.Stocks.Stock_Handle_Interface'Class;
      Efficiency : Unit_Real;
      Factor     : Unit_Real)
      return Carthage.Quantities.Quantity_Type
   is
      Can_Proceed : Boolean := True;

      Consumption : Carthage.Handles.Resources.Resource_Stock;

      procedure Check
        (Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type);

      -----------
      -- Check --
      -----------

      procedure Check
        (Resource : Carthage.Handles.Resources.Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type)
      is
         use type Carthage.Quantities.Quantity_Type;
         Need : constant Carthage.Quantities.Quantity_Type :=
                  Carthage.Quantities.Scale (Quantity, Factor);
      begin
         if Need > Stock.Quantity (Resource) then
            if Log_Production then
               This.Log ("insufficient " & Resource.Tag
                         & " (require "
                         & Carthage.Quantities.Show (Need) & ")");
            end if;
            Can_Proceed := False;
         end if;
      end Check;

   begin

      Get (This).Production_Inputs.Scan_Stock (Check'Access);

      if Can_Proceed then
         Stock.Remove_Stock (Consumption);
         declare
            Produced_Quantity : constant Carthage.Quantities.Quantity_Type :=
                                  Carthage.Quantities.Scale
                                    (This.Production_Quantity,
                                     Efficiency * Factor);
         begin
            if Log_Production then
               This.Log
                 ("producing "
                  & Carthage.Quantities.Show (Produced_Quantity)
                  & " " & This.Production_Output.Tag);
            end if;
            return Produced_Quantity;
         end;
      else
         return Carthage.Quantities.Zero;
      end if;

   end Execute_Production;

   ------------------
   -- Get_By_Index --
   ------------------

   function Get_By_Index (Index : Positive) return Structure_Handle is
   begin
      for I in 1 .. Structure_Vector.Last_Index loop
         if Structure_Vector (I).Index = Index then
            return Get (I);
         end if;
      end loop;
      raise Constraint_Error with
        "no such structure index:" & Index'Image;
   end Get_By_Index;

   ------------------------
   -- Harvest_Production --
   ------------------------

   function Harvest_Production
     (This      : Structure_Handle;
      Tile      : Tile_Reference)
      return Production_Array
   is
      Result : Carthage.Handles.Resources.Resource_Stock;
   begin
      for Production of
        Get (This).Harvest_Production
      loop
         if Carthage.Handles.Tiles.Get (Tile).Has_Terrain
           (Carthage.Handles.Terrain.Get (Production.Terrain))
         then
            Result.Add (Carthage.Handles.Resources.Get (Production.Resource),
                        Production.Quantity);
         end if;
      end loop;
      return To_Production_Array (Result);
   end Harvest_Production;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Structure_Vectors.Vector'Read (Stream, Structure_Vector);
      for Reference in 1 .. Structure_Vector.Last_Index loop
         Structure_Map.Insert (Get (Reference).Tag, Reference);
      end loop;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Structure_Vectors.Vector'Write (Stream, Structure_Vector);
   end Save;

   -------------------------
   -- To_Production_Array --
   -------------------------

   function To_Production_Array
     (This : Carthage.Handles.Resources.Stock_Interface'Class)
      return Production_Array
   is
      Last   : Natural := 0;
      Result : Production_Array (1 .. Natural (Resource_Reference'Last));

      procedure Add (Resource : Carthage.Handles.Resources.Resource_Handle;
                     Quantity : Carthage.Quantities.Quantity_Type);

      ---------
      -- Add --
      ---------

      procedure Add (Resource : Carthage.Handles.Resources.Resource_Handle;
                     Quantity : Carthage.Quantities.Quantity_Type)
      is
      begin
         Last := Last + 1;
         Result (Last) := Resource_Quantity_Record'
           (Resource => Resource,
            Quantity => Quantity);
      end Add;

   begin
      This.Scan_Stock (Add'Access);
      return Result (1 .. Last);
   end To_Production_Array;

end Carthage.Handles.Structures;
