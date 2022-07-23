with Carthage.Handles.Planets;
with Carthage.Handles.Tiles;

with Carthage.Import;

package body Carthage.Handles.Houses.Configure is

   function Is_Land_Tile
     (Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Boolean
   is (not Carthage.Handles.Tiles.Is_Water (Tile))
     with Unreferenced;

   function Is_City_Tile
     (Tile : Carthage.Handles.Tiles.Tile_Handle)
      return Boolean
   is (Carthage.Handles.Tiles.Has_City (Tile))
     with Unreferenced;

   ---------------------
   -- Configure_House --
   ---------------------

   procedure Configure_House
     (Config : Tropos.Configuration)
   is

      Rec : constant House_Record := House_Record'
        (Tag           => +Config.Config_Name,
         Category      => House_Category'Value (Config.Get ("category")),
         Capital       => Null_Planet_Reference,
         Tax_Rate      => Real (Float'(Config.Get ("tax-rate", 0.1))),
         Tithe_Skim    => Real (Float'(Config.Get ("tithe-skim", 0.1))),
         Unit_Pay      => Real (Float'(Config.Get ("unit-pay", 0.75))),
         Cash          =>
           Carthage.Money.To_Money
             (Real (Float'(Config.Get ("cash", 5_000.0)))),
         Debt          =>
           Carthage.Money.To_Money
             (Real (Float'(Config.Get ("debt", 0.0)))),
         Color         => (if Config.Contains ("color")
                           then Carthage.Import.Palette_Color
                             (Config.Get ("color"))
                           else (0.5, 0.5, 0.5, 1.0)),
         Treaties      => (others => Neutral),
         Known_Planets => <>);

         House : constant House_Reference := Create (Rec);

   begin

      Get (House).Log ("created " & Get (House).Long_Name);

   end Configure_House;

   -----------------------------
   -- Configure_Initial_State --
   -----------------------------

   procedure Configure_Initial_State
     (Config : Tropos.Configuration)
   is
      House : constant House_Handle := Get (Config.Config_Name);
      Capital_Tag : constant String := Config.Get ("capital", "");
      Capital : constant Carthage.Handles.Planets.Planet_Handle :=
                      (if Capital_Tag /= "" then
                         (if Carthage.Handles.Planets.Exists (Capital_Tag)
                          then Carthage.Handles.Planets.Get (Capital_Tag)
                          else (raise Constraint_Error
                              with "no such capital planet "
                            & Capital_Tag
                            & " for house " & Config.Config_Name))
                       else Carthage.Handles.Planets.Empty_Handle);

      procedure Set_Known (Planet : Carthage.Handles.Planets.Planet_Handle);

      ---------------
      -- Set_Known --
      ---------------

      procedure Set_Known (Planet : Carthage.Handles.Planets.Planet_Handle) is
      begin
         House.Add_Known_Planet (Planet.Reference);
      end Set_Known;

   begin
      Carthage.Handles.Planets.For_All_Planets (Set_Known'Access);

      if Capital.Has_Element then
         Capital.Set_Owner (House.Reference);
         House.Set_Initial_Capital (Capital.Reference);
         Capital.Set_Seen_By (House.Reference);
      end if;

      if Config.Contains ("reveal") then
         for Revealed_Planet of Config.Child ("reveal") loop
            Carthage.Handles.Planets.Get
              (Revealed_Planet.Config_Name)
              .Set_Seen_By (House.Reference);
         end loop;
      end if;
   end Configure_Initial_State;

end Carthage.Handles.Houses.Configure;
