with Tropos.Reader;

package body Carthage.Settings is

   type Carthage_Settings_Record is
      record
         Daily_Production_Factor : Unit_Real := 0.1;
         Daily_Harvest_Factor    : Unit_Real := 0.01;
         Daily_Tax_Factor        : Unit_Real := 0.01;
         Daily_Laboratory_Cost   : Carthage.Money.Money_Type :=
                                     Carthage.Money.To_Money (2.0);
         Daily_Research_Points   : Non_Negative_Real := 5.0;
         Daily_City_Food         : Carthage.Quantities.Quantity_Type :=
                                     Carthage.Quantities.To_Quantity
                                       (0.1);
         Daily_Unit_Food_Factor  : Unit_Real := 0.1;
      end record;

   Current_Settings : Carthage_Settings_Record := (others => <>);

   -----------------------------
   -- Daily_Production_Factor --
   -----------------------------

   function Daily_Production_Factor return Unit_Real
   is (Current_Settings.Daily_Production_Factor);

   --------------------------
   -- Daily_Harvest_Factor --
   --------------------------

   function Daily_Harvest_Factor return Unit_Real
   is (Current_Settings.Daily_Harvest_Factor);

   ----------------------
   -- Daily_Tax_Factor --
   ----------------------

   function Daily_Tax_Factor return Unit_Real
   is (Current_Settings.Daily_Tax_Factor);

   ---------------------------
   -- Daily_Laboratory_Cost --
   ---------------------------

   function Daily_Laboratory_Cost return Carthage.Money.Money_Type
   is (Current_Settings.Daily_Laboratory_Cost);

   ---------------------------
   -- Daily_Research_Points --
   ---------------------------

   function Daily_Research_Points return Non_Negative_Real
   is (Current_Settings.Daily_Research_Points);

   ---------------------
   -- Daily_City_Food --
   ---------------------

   function Daily_City_Food return Carthage.Quantities.Quantity_Type
   is (Current_Settings.Daily_City_Food);

   ----------------------------
   -- Daily_Unit_Food_Factor --
   ----------------------------

   function Daily_Unit_Food_Factor return Unit_Real
   is (Current_Settings.Daily_Unit_Food_Factor);

   ---------------
   -- Configure --
   ---------------

   procedure Configure (Path : String) is
   begin
      Current_Settings := (others => <>);
      declare
         Config : constant Tropos.Configuration :=
                    Tropos.Reader.Read_Config (Path);

         procedure Set (Name  : String;
                        Field : in out Real);

         procedure Set (Name  : String;
                        Field : in out Carthage.Money.Money_Type);

         procedure Set
           (Name  : String;
            Field : in out Carthage.Quantities.Quantity_Type);

         ---------
         -- Set --
         ---------

         procedure Set (Name  : String;
                        Field : in out Real)
         is
            F : constant Long_Float :=
                  Config.Get (Name, Long_Float (Field));
         begin
            Field := Real (F);
         end Set;

         ---------
         -- Set --
         ---------

         procedure Set (Name  : String;
                        Field : in out Carthage.Money.Money_Type)
         is
            F : constant Long_Float :=
                  Config.Get (Name,
                              Long_Float
                                (Carthage.Money.To_Real (Field)));
         begin
            Field := Carthage.Money.To_Money (Real (F));
         end Set;

         ---------
         -- Set --
         ---------

         procedure Set
           (Name  : String;
            Field : in out Carthage.Quantities.Quantity_Type)
         is
            F : constant Long_Float :=
                  Config.Get (Name,
                              Long_Float
                                (Carthage.Quantities.To_Real (Field)));
         begin
            Field := Carthage.Quantities.To_Quantity (Real (F));
         end Set;

      begin
         Set ("daily-production-factor",
              Current_Settings.Daily_Production_Factor);
         Set ("daily-harvest-factor",
              Current_Settings.Daily_Harvest_Factor);
         Set ("daily-tax-factor",
              Current_Settings.Daily_Tax_Factor);
         Set ("daily-laboratory-cost",
              Current_Settings.Daily_Laboratory_Cost);
         Set ("daily-research-points",
              Current_Settings.Daily_Research_Points);
         Set ("daily-city-food",
              Current_Settings.Daily_City_Food);
         Set ("daily-unit-food-factor",
              Current_Settings.Daily_Unit_Food_Factor);
      end;
   end Configure;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Carthage_Settings_Record'Read (Stream, Current_Settings);
   end Load;

   ----------
   -- Save --
   ----------

   procedure
   Save (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Carthage_Settings_Record'Write (Stream, Current_Settings);
   end Save;

end Carthage.Settings;
