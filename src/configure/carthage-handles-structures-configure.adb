--  with WL.Random;

with Tropos.Reader;

with Carthage.Configure;
with Carthage.Handles.Resources;
with Carthage.Handles.Technology;
with Carthage.Handles.Terrain;
with Carthage.Handles.Worlds;

package body Carthage.Handles.Structures.Configure is

   procedure Configure_Harvest_Production
     (Production : in out Harvest_Production_Vectors.Vector;
      Config     : Tropos.Configuration);

   procedure Configure_Production
     (Inputs   : in out Carthage.Handles.Resources.Resource_Stock;
      Output   :    out Resource_Reference;
      Quantity : out Carthage.Quantities.Quantity_Type;
      Config   : Tropos.Configuration);

   --------------------------------
   -- Configure_Bonus_Production --
   --------------------------------

   procedure Configure_Bonus_Production
     (Config : Tropos.Configuration)
   is
   begin

      for Structure_Config of Config loop
         if not Exists (Structure_Config.Config_Name) then
            raise Constraint_Error with
              "bonus production: structure with tag "
              & Structure_Config.Config_Name
              & " was not found";
         end if;

         declare
            Item : constant Structure_Handle :=
                     Get (Structure_Config.Config_Name);
         begin

            for Bonus_Config of Structure_Config loop
               if not Exists (Bonus_Config.Config_Name) then
                  raise Constraint_Error with
                    "bonus production: structure with tag "
                    & Bonus_Config.Config_Name
                    & " was not found";
               end if;
               declare
                  Bonus_Structure : constant Structure_Handle :=
                                      Get (Bonus_Config.Config_Name);
               begin
                  for Resource_Config of Bonus_Config loop
                     declare
                        use Carthage.Handles.Resources;
                        Resource : constant Resource_Handle :=
                                     Get (Resource_Config.Config_Name);
                        Quantity : constant Natural :=
                                     Resource_Config.Value;
                     begin
                        Item.Add_Bonus_Production
                          (Bonus    => Bonus_Structure.Reference,
                           Resource => Resource.Reference,
                           Quantity => Quantity);
                     end;
                  end loop;
               end;
            end loop;

         end;
      end loop;
   end Configure_Bonus_Production;

   ----------------------------------
   -- Configure_Harvest_Production --
   ----------------------------------

   procedure Configure_Harvest_Production
     (Production : in out Harvest_Production_Vectors.Vector;
      Config     : Tropos.Configuration)
   is
      type State_Type is
        (Terrain_State, World_State, Resource_State, Count_State);
   begin
      for Item_Config of Config loop
         declare
            State    : State_Type := Terrain_State;
            Terrain  : Carthage.Handles.Terrain.Terrain_Handle;
            World    : Carthage.Handles.Worlds.World_Handle;
            City     : Boolean := False;
            Resource : Carthage.Handles.Resources.Resource_Handle;
            Count    : Carthage.Quantities.Quantity_Type;
         begin
            for Field_Config of Item_Config loop
               declare
                  Field : String := Field_Config.Config_Name;
               begin
                  for Ch of Field loop
                     if Ch = ' ' then
                        Ch := '_';
                     end if;
                  end loop;

                  case State is
                     when Terrain_State =>
                        if not Handles.Terrain.Is_Terrain_Tag (Field) then
                           raise Constraint_Error with
                             "production: no such terrain type: "
                             & Field;
                        end if;
                        Terrain :=
                          Carthage.Handles.Terrain.Get (Field);
                        State := World_State;
                     when World_State =>
                        if Field = "city" then
                           City := True;
                        elsif not Carthage.Handles.Worlds.Exists (Field) then
                           raise Constraint_Error with
                             "production: no such world type: "
                             & Field;
                        else
                           World :=
                             Carthage.Handles.Worlds.Get (Field);
                        end if;
                        State := Resource_State;
                     when Resource_State =>
                        if Field = "@" then
                           State := World_State;
                        else
                           Resource :=
                             Carthage.Handles.Resources.Get (Field);
                           State := Count_State;
                        end if;
                     when Count_State =>
                        Count :=
                          Carthage.Quantities.To_Quantity
                            (Natural'Value (Field));
                        State := Resource_State;

                        Production.Append
                          (Harvest_Production_Record'
                             (World     => World.Reference,
                              City      => City,
                              Terrain   => Terrain.Reference,
                              Resource  => Resource.Reference,
                              Quantity  => Count));

                        City := False;
                        World := Carthage.Handles.Worlds.Empty_Handle;
                  end case;
               end;
            end loop;
         end;
      end loop;
   end Configure_Harvest_Production;

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production
     (Inputs   : in out Carthage.Handles.Resources.Resource_Stock;
      Output   :    out Resource_Reference;
      Quantity :    out Carthage.Quantities.Quantity_Type;
      Config   :        Tropos.Configuration)
   is

      Have_Inputs : Boolean := False;
      Have_Outputs : Boolean := False;

   begin
      for Item of Config.Child ("input") loop
         Have_Inputs := True;
         Inputs.Add
           (Resource       =>
              Carthage.Handles.Resources.Get (Item.Config_Name),
            Added_Quantity => Item.Value);
      end loop;

      for Item of Config.Child ("output") loop
         if Have_Outputs then
            raise Constraint_Error with
            Config.Config_Name
              & ": multiple outputs not supported";
         end if;

         Have_Outputs := True;
         Output := Carthage.Handles.Resources.Get (Item.Config_Name)
           .Reference;
         Quantity := Carthage.Quantities.To_Quantity (Natural'(Item.Value));

      end loop;

      if not Have_Inputs and then not Have_Outputs then
         raise Constraint_Error with
           "while configuring production for "
           & Config.Config_Name & ": cannot find production";
      end if;

   end Configure_Production;

   ------------------------
   -- Configure_Structure --
   ------------------------

   procedure Configure_Structure
     (Config : Tropos.Configuration)
   is
      function G (Field_Name : String) return Boolean
      is (Config.Get (Field_Name));

      function G (Field_Name : String) return Integer
      is (Config.Get (Field_Name, 0));

      Structure : Structure_Record :=
                    Structure_Record'
                      (Tag          => +Config.Config_Name,
                       Index        => G ("index"),
                       Singular     => G ("singular"),
                       Can_Build    => G ("can_build"),
                       Palace       => G ("palace"),
                       Shield       => G ("shield"),
                       Church       => G ("church"),
                       Agora        => G ("agora"),
                       Water        => G ("water"),
                       Land         => G ("land"),
                       Road         => G ("road"),
                       Barren       => G ("barren"),
                       Neutral      => G ("neutral"),
                       Is_Harvester => G ("harvester"),
                       Is_Bonus     => Config.Contains ("bonus"),
                       Area         => G ("area"),
                       Maintenance  => G ("maintenance"),
                       Cost         => G ("cost"),
                       Build_Time   => G ("build_time"),
                       Enabled_By   =>
                         Carthage.Handles.Technology.Get
                           (Config.Get ("enabled-by", "nothing"))
                       .Reference,
                       Value        => G ("value"),
                       Production_Inputs => <>,
                       Production_Output => Null_Resource_Reference,
                       Production_Quantity => Carthage.Quantities.Zero,
                       Harvest_Production => <>,
                       Bonus_Production   => <>);
   begin
      if Config.Contains ("production") then
         if Structure.Is_Harvester then
            declare
               Harvest_Name : constant String :=
                                Config.Get ("production");
            begin
               Configure_Harvest_Production
                 (Production => Structure.Harvest_Production,
                  Config     =>
                    Tropos.Reader.Read_Config
                      (Carthage.Configure.Fading_Suns_Data_File
                           (Harvest_Name)));
            end;
         else
            Configure_Production
              (Inputs  => Structure.Production_Inputs,
               Output   => Structure.Production_Output,
               Quantity => Structure.Production_Quantity,
               Config  => Config.Child ("production"));
         end if;

         --  if Config.Contains ("bonus") then
         --     for Bonus_Config of Config.Child ("bonus") loop
         --        Structure.Chances.Insert
         --          (Bonus_Config.Config_Name,
         --           Bonus_Config.Value);
         --     end loop;
         --  end if;
      end if;

      Create (Structure);

   end Configure_Structure;

   ------------------
   -- Random_Bonus --
   ------------------

   --  function Random_Bonus
   --    (Planet_Category : String;
   --     Terrain_Id      : String)
   --     return Hira.Structure.Structure_Class
   --  is
   --  begin
   --     for Structure of Hira.Structure.Scan_By_Index loop
   --        declare
   --           Chance : constant Natural :=
   --                      Structure.Chance (Planet_Category, Terrain_Id);
   --        begin
   --           if Chance > 0 then
   --          if WL.Random.Random_Number (1, Chance_Against) <= Chance then
   --                 return Structure;
   --              end if;
   --           end if;
   --        end;
   --     end loop;
   --     return null;
   --  end Random_Bonus;

end Carthage.Handles.Structures.Configure;
