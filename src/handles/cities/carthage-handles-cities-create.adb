package body Carthage.Handles.Cities.Create is

   --------------
   -- New_City --
   --------------

   function New_City
     (Planet    : Carthage.Handles.Planets.Planet_Handle;
      Tile      : Carthage.Handles.Tiles.Tile_Handle;
      Structure : Carthage.Handles.Structures.Structure_Handle;
      Owner     : Carthage.Handles.Houses.House_Handle;
      Health    : Health_Type;
      Loyalty   : Loyalty_Type)
      return Carthage.Handles.Cities.City_Handle
   is
      Rec : City_Record := City_Record'
        (Identifier => Next_Identifier,
         Planet     => Planet.Reference,
         Tile       => Tile.Reference,
         Structure  => Structure.Reference,
         Owner      => Owner.Reference,
         Health     => Health,
         Loyalty    => Loyalty,
         Agora      => Null_City_Reference,
         Progress   => 0.0,
         Orders     => <>,
         Seen_By    => Empty_House_Set,
         Prices     => <>,
         Transfer   => <>);
   begin

      if Structure.Is_Agora then

         declare
            procedure Set_Price
              (Resource : Carthage.Handles.Resources.Resource_Handle);

            ---------------
            -- Set_Price --
            ---------------

            procedure Set_Price
              (Resource : Carthage.Handles.Resources.Resource_Handle)
            is
               use Carthage.Money;
               Base_Price : constant Price_Type := Resource.Base_Price;
               Buy_Price  : constant Price_Type :=
                              Max (Adjust_Price (Base_Price, 0.95),
                                   To_Price (1.0));
               Sell_Price : constant Price_Type :=
                              Adjust_Price (Buy_Price, 1.1);
            begin
               Rec.Prices (Resource.Reference) :=
                 Agora_Resource_Record'
                   (Buy_Price  => Buy_Price,
                    Sell_Price => Sell_Price);
            end Set_Price;

         begin
            Carthage.Handles.Resources.For_All_Resources
              (Set_Price'Access);
         end;
      end if;

      declare
         City : constant City_Reference :=
                  Create_City (Rec);
      begin
         Carthage.Handles.Tiles.Set_City (Tile, City);

         if Structure.Is_Palace then
            pragma Assert (not Planet.Has_Palace,
                           Planet.Tag & " has more than one palace");
            Planet.Set_Palace (City);
         end if;

         return Get (City);
      end;

   end New_City;

   --------------
   -- New_City --
   --------------

   procedure New_City
     (Planet    : Carthage.Handles.Planets.Planet_Handle;
      Tile      : Carthage.Handles.Tiles.Tile_Handle;
      Structure : Carthage.Handles.Structures.Structure_Handle;
      Owner     : Carthage.Handles.Houses.House_Handle;
      Health    : Health_Type;
      Loyalty   : Loyalty_Type)
   is
      City : constant City_Handle :=
               New_City (Planet, Tile, Structure, Owner, Health, Loyalty);
   begin
      pragma Unreferenced (City);
   end New_City;

end Carthage.Handles.Cities.Create;
