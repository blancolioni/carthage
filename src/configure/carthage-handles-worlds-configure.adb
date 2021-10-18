with Ada.Strings.Fixed;

with Tropos.Reader;
with Tropos.Writer;

with Carthage.Configure;
with Carthage.Import;
with Carthage.Handles.Terrain;

package body Carthage.Handles.Worlds.Configure is

   Have_Terrain_Config   : Boolean := False;
   Terrain_Color_Config : Tropos.Configuration;

   procedure Read_Terrain_Config;

   ------------------------------
   -- Configure_Movement_Costs --
   ------------------------------

   procedure Configure_Movement_Costs
     (Config : Tropos.Configuration)
   is
   begin
      for Terrain_Config of Config loop
         declare
            use Carthage.Handles.Terrain;
            Terrain_Name : constant String :=
                             (if Terrain_Config.Config_Name = "arid grass"
                              or else Terrain_Config.Config_Name = "arid"
                              then "arid_grass"
                              else Terrain_Config.Config_Name);
            Terrain      : constant Terrain_Handle :=
                             (if Is_Terrain_Tag (Terrain_Name)
                              then Get (Terrain_Name)
                              elsif Terrain_Name = "road"
                              then Empty_Handle
                              else raise Constraint_Error with
                                "no such terrain in movement config: "
                              & Terrain_Name);
         begin
            for World_Config of Terrain_Config loop
               if Exists (World_Config.Config_Name) then
                  declare
                     World : constant World_Handle :=
                               Get (World_Config.Config_Name);
                     Index    : Natural := 0;

                     type Unit_Category_Array is
                       array (1 .. 10) of Unit_Category;

                     To_Category : constant Unit_Category_Array :=
                                     (Foot, Wheel, Tread, Air, Naval,
                                      Space, Hover, Jump, Crawler, Lander);
                  begin
                     for Move_Config of World_Config loop
                        Index := Index + 1;
                        if Has_Element (Terrain) then
                           World.Set_Terrain_Movement
                             (Unit       => To_Category (Index),
                              Terrain    => Terrain.Reference,
                              Multiplier =>
                                Real (Float'(Move_Config.Value)));
                        else
                           World.Set_Road_Movement
                             (Unit       => To_Category (Index),
                              Multiplier =>
                                Real (Float'(Move_Config.Value)));
                        end if;
                     end loop;

                  end;
               end if;
            end loop;
         end;
      end loop;
   end Configure_Movement_Costs;

   ---------------------
   -- Configure_World --
   ---------------------

   procedure Configure_World
     (Config : Tropos.Configuration)
   is
   begin

      if not Have_Terrain_Config then
         Read_Terrain_Config;
         Tropos.Writer.Write_Config
           (Terrain_Color_Config, "terrain-config.txt");
      end if;

      Create_World (Config.Config_Name);

   end Configure_World;

   -------------------------
   -- Read_Terrain_Config --
   -------------------------

   procedure Read_Terrain_Config is
      Eofs_Config : constant Tropos.Configuration :=
                      Tropos.Reader.Read_Config
                        (Carthage.Configure.Fading_Suns_Data_File
                           ("TERCOLOR"));
      Name        : Boolean := True;
      Name_Config : Tropos.Configuration;

      function To_Id (Name : String) return String;

      -----------
      -- To_Id --
      -----------

      function To_Id (Name : String) return String is
         Result : String := Name;
      begin
         for Ch of Result loop
            if Ch = ' ' then
               Ch := '_';
            end if;
         end loop;
         return Result;
      end To_Id;

   begin
      Terrain_Color_Config := Tropos.New_Config ("terrain-Color");
      for Config of Eofs_Config.Child (1) loop
         if Name then
            Name_Config := Config;
         else
            declare
               Stat_Config : Tropos.Configuration :=
                               Tropos.New_Config
                                 (To_Id (Name_Config.Config_Name));
               Stats : Carthage.Import.Numeric_Settings (1 .. 5);
            begin
               Carthage.Import.Scan_Settings (Config.Config_Name, Stats);
               for Stat of Stats loop
                  Stat_Config.Add
                    (Tropos.New_Config
                       (Ada.Strings.Fixed.Trim
                            (Integer'Image (Stat),
                             Ada.Strings.Left)));
               end loop;
               Terrain_Color_Config.Add (Stat_Config);
            end;
         end if;
         Name := not Name;
      end loop;

      Have_Terrain_Config := True;
   end Read_Terrain_Config;

end Carthage.Handles.Worlds.Configure;
