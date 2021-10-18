with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Text_IO;

with WL.Localisation;
with WL.String_Sets;

with Tropos.Reader;
with Tropos.Writer;

with Carthage.Paths;

with Carthage.Handles.Structures.Configure;
with Carthage.Handles.Galaxy.Configure;
with Carthage.Handles.Houses.Configure;
--  with Carthage.Handles.Planets.Configure;
with Carthage.Handles.Resources.Configure;
with Carthage.Handles.Technology.Configure;
with Carthage.Handles.Terrain.Configure;
with Carthage.Handles.Units.Configure;
with Carthage.Handles.Worlds.Configure;

with Carthage.Import;

with Carthage.Options;

with Carthage.Import.Galaxy;

package body Carthage.Configure is

   Init_Config  : Tropos.Configuration;
   Eofs_Config  : Tropos.Configuration;

   function Scenario_File
     (Scenario_Name : String;
      File_Name     : String)
      return String
   is (Carthage.Paths.Config_File
       ("scenarios/" & Scenario_Name & "/" & File_Name));

   function Find (Path : String) return String;

   function Fading_Suns_Bin_File
     (Name : String)
      return String
   is (Find (Eofs_Config.Get ("path") & "/BIN/" & Name & ".BIN"));

   function Fading_Suns_Data_File
     (Name : String)
      return String
   is (Find (Eofs_Config.Get ("path") & "/DAT/" & Name & ".DAT"));

   function Fading_Suns_FLC_File
     (Name : String)
      return String
   is (Find (Eofs_Config.Get ("path") & "/FLC/" & Name & ".FLC"));

   function Fading_Suns_Rand_File
     (Name : String)
      return String
   is (Find (Eofs_Config.Get ("path") & "/RAND/" & Name & ".TXT"));

   procedure Delete_Contents
     (Directory_Path : String);

   procedure Load_Directory_Configuration
     (Directory_Name : String;
      Configure      : not null access
        procedure (Config : Tropos.Configuration));

   procedure Load_Scenario_Configuration
     (Scenario_Name : String;
      File_Name     : String;
      Configure     : not null access
        procedure (Config : Tropos.Configuration))
     with Unreferenced;

   procedure Import_Cities
     (Config : Tropos.Configuration);

   procedure Import_Technology
     (Config      : Tropos.Configuration;
      Write_Phase : Boolean);

   procedure Import_Resources
     (Config : Tropos.Configuration);

   procedure Import_Terrain
     (Color_Config      : Tropos.Configuration;
      Road_Cost_Config   : Tropos.Configuration;
      Move_Cost_Config   : Tropos.Configuration);

   procedure Import_Units
     (Config : Tropos.Configuration);

   procedure Import_Targets
     (Config : Tropos.Configuration);

   function To_Carthage_Id
     (Dat_Identifier   : String;
      Space_Substitute : Character := '-')
      return String;

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Natural, String);

   Technology_Vector : String_Vectors.Vector;

   ---------------------
   -- Delete_Contents --
   ---------------------

   procedure Delete_Contents
     (Directory_Path : String)
   is
   begin
      if Ada.Directories.Exists (Directory_Path) then
         Ada.Directories.Delete_Tree (Directory_Path);
         Ada.Directories.Create_Directory (Directory_Path);
      end if;
   end Delete_Contents;

   ----------
   -- Find --
   ----------

   function Find (Path : String) return String is
   begin
      if Ada.Directories.Exists (Path) then
         return Path;
      end if;

      for I in reverse Path'Range loop
         if Path (I) = '/' or else Path (I) = '\' then
            declare
               use Ada.Directories;
               Parent : constant String := Path (Path'First .. I - 1);
               File   : constant String :=
                          Ada.Characters.Handling.To_Lower
                            (Path (I + 1 .. Path'Last));
               Search : Search_Type;
            begin
               if not Ada.Directories.Exists (Parent) then
                  raise Constraint_Error with
                    "cannot find containing directory " & Parent;
               end if;
               Start_Search
                 (Search    => Search,
                  Directory => Parent,
                  Pattern   => "",
                  Filter    => (Ordinary_File => True, others => False));
               while More_Entries (Search) loop
                  declare
                     use Ada.Characters.Handling;
                     Next : Directory_Entry_Type;
                  begin
                     Get_Next_Entry (Search, Next);
                     if To_Lower (Simple_Name (Next)) = File then
                        return Full_Name (Next);
                     end if;
                  end;
               end loop;

               exit;
            end;
         end if;
      end loop;

      raise Constraint_Error with
        "cannot find: " & Path;

   end Find;

   -------------------
   -- Import_Cities --
   -------------------

   procedure Import_Cities
     (Config : Tropos.Configuration)
   is

      Index : Positive := 1;

      At_Name  : Boolean := False;
      At_Stats : Boolean := False;

      Name_Config : Tropos.Configuration;

      Normal_Bonus : constant Tropos.Configuration :=
                       Tropos.Reader.Read_Config
                         (Fading_Suns_Rand_File ("TYPE0"));

      City_Bonus : constant Tropos.Configuration :=
                       Tropos.Reader.Read_Config
                         (Fading_Suns_Rand_File ("TYPE1"));

      Jungle_Bonus   : constant Tropos.Configuration :=
                       Tropos.Reader.Read_Config
                         (Fading_Suns_Rand_File ("TYPE2"));

      Frozen_Bonus   : constant Tropos.Configuration :=
                       Tropos.Reader.Read_Config
                         (Fading_Suns_Rand_File ("TYPE3"));

      Barren_Bonus   : constant Tropos.Configuration :=
                         Tropos.Reader.Read_Config
                           (Fading_Suns_Rand_File ("TYPE4"));

      procedure Create_Bonus_Config
        (Base        : Tropos.Configuration;
         Base_Name   : String;
         Id_Number   : Natural;
         Target      : in out Tropos.Configuration;
         Target_Name : String;
         Index       : Natural);

      procedure Load_Bonus_Table
        (Structure_Config : in out Tropos.Configuration);

      procedure Load_Production_Table
        (Production_Config : in out Tropos.Configuration);

      -------------------------
      -- Create_Bonus_Config --
      -------------------------

      procedure Create_Bonus_Config
        (Base        : Tropos.Configuration;
         Base_Name   : String;
         Id_Number   : Natural;
         Target      : in out Tropos.Configuration;
         Target_Name : String;
         Index       : Natural)
      is
         Child_Index : Natural := 0;
         Last_Child  : constant Natural := Base.Child_Count - 1;
         Skip        : Boolean := False;
         Odds        : Natural := 0;
      begin
         for Config of Base loop
            Child_Index := Child_Index + 1;
            if Child_Index in 2 .. Last_Child then
               declare
                  Local_Index : constant Natural := (Child_Index - 2) mod 3;
                  Local_Value : constant Natural :=
                                  Natural'Value (Config.Config_Name);

               begin
                  if Local_Index = 0 then
                     Skip := Local_Value + 1 /= Id_Number;
                  elsif not Skip then
                     if Local_Index = 1 then
                        Odds := Local_Value;
                     elsif Local_Value = Index then
                        Target.Add
                          (Base_Name & "-" & Target_Name,
                           Odds);
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end Create_Bonus_Config;

      ----------------------
      -- Load_Bonus_Table --
      ----------------------

      procedure Load_Bonus_Table
        (Structure_Config : in out Tropos.Configuration)
      is

         Bonus_Config : Tropos.Configuration :=
                          Tropos.New_Config ("bonus");

         procedure Load (Source : Tropos.Configuration;
                         Name   : String);

         ----------
         -- Load --
         ----------

         procedure Load (Source : Tropos.Configuration;
                         Name   : String)
         is
         begin
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "ocean", 0);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "grass", 1);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "arid_grass", 2);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "desert", 3);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "ice", 4);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "tundra", 5);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "mountain", 6);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "hill", 7);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "tree", 8);
            Create_Bonus_Config
              (Source, Name,
               Structure_Config.Get ("index"),
               Bonus_Config, "river", 9);
         end Load;

      begin
         Load (Normal_Bonus, "normal");
         Load (City_Bonus, "city");
         Load (Jungle_Bonus, "jungle");
         Load (Frozen_Bonus, "frozen");
         Load (Barren_Bonus, "barren");
         if Bonus_Config.Child_Count > 0 then
            Structure_Config.Add (Bonus_Config);
         end if;
      end Load_Bonus_Table;

      ---------------------------
      -- Load_Production_Table --
      ---------------------------

      procedure Load_Production_Table
        (Production_Config : in out Tropos.Configuration)
      is
         Config : constant Tropos.Configuration :=
                    Tropos.Reader.Read_Config
                      (Fading_Suns_Data_File ("PROD"));
      begin
         for Item_Config of Config loop
            declare
               type State_Type is
                 (City_Key_State, Fac_Name_State, Need_State,
                  Resource_State, Quantity_State);
               Structure_Name : constant String := Item_Config.Get (2);
               State    : State_Type := City_Key_State;
               Making   : Boolean := False;
               Resource : Carthage.Handles.Resources.Resource_Handle;
               Quantity : Natural;
               Inputs   : Tropos.Configuration :=
                            Tropos.New_Config ("input");
               Outputs  : Tropos.Configuration :=
                            Tropos.New_Config ("output");
            begin
               for Field_Config of Item_Config loop
                  declare
                     Field : constant String := Field_Config.Config_Name;
                  begin
                     case State is
                        when City_Key_State =>
                           if Field /= "city" then
                              raise Constraint_Error with
                                "while configuring production for "
                                & Structure_Name & ": expected 'city'"
                                & " but found '" & Field & "'";
                           end if;
                           State := Fac_Name_State;

                        when Fac_Name_State =>
                           if Field /= Structure_Name then
                              raise Constraint_Error with
                                "while configuring production for "
                                & Structure_Name & ": expected Structure name"
                                & " but found '" & Field & "'";
                           end if;
                           State := Need_State;

                        when Need_State =>
                           if Field /= "need" and then Field /= "make" then
                              raise Constraint_Error with
                                "while configuring production for "
                                & Structure_Name & ": expected make or need"
                                & " but found '" & Field & "'";
                           end if;

                           Making := Field = "make";
                           State := Resource_State;

                        when Resource_State =>
                           if not Carthage.Handles.Resources.Exists
                             (Field)
                           then
                              raise Constraint_Error with
                                "while configuring production for "
                                & Structure_Name & ": expected a resource"
                                & " but found '" & Field & "'";
                           end if;

                           Resource :=
                             Carthage.Handles.Resources.Get (Field);
                           State := Quantity_State;

                        when Quantity_State =>
                           Quantity := Integer'Value (Field);

                           if Making then
                              Outputs.Add (Resource.Tag, Quantity);
                           else
                              Inputs.Add (Resource.Tag, Quantity);
                           end if;

                           State := Need_State;
                     end case;
                  end;
               end loop;

               declare
                  Prod : Tropos.Configuration :=
                           Tropos.New_Config (Structure_Name);
               begin
                  Prod.Add (Inputs);
                  Prod.Add (Outputs);
                  Production_Config.Add (Prod);
               end;
            end;
         end loop;

      end Load_Production_Table;

      Production_Config : Tropos.Configuration;

   begin

      Load_Production_Table (Production_Config);

      for Field_Config of Config.Child (1) loop
         declare
            Field : constant String := Field_Config.Config_Name;
         begin
            if Field = "name" then
               At_Name := True;
            elsif Field = "stats" then
               At_Stats := True;
            elsif At_Name then
               Name_Config := Field_Config;
               At_Name := False;
            elsif At_Stats then

               At_Stats := False;

               declare
                  Id          : constant String :=
                                  To_Carthage_Id (Name_Config.Config_Name);
                  Stats       : constant String := Field;
                  Output      : Tropos.Configuration :=
                                  Tropos.New_Config (Id);
                  Start_Index : Positive := Stats'First;

                  function Next_Number return Natural;

                  procedure Category
                    (Name  : String);

                  procedure Flag
                    (Name  : String;
                     Value : Natural);

                  procedure Number
                    (Name  : String;
                     Value : Natural);

                  --------------
                  -- Category --
                  --------------

                  procedure Category
                    (Name  : String)
                  is
                  begin
                     if Id = Name then
                        Output.Add (Name, "yes");
                     end if;
                  end Category;

                  ----------
                  -- Flag --
                  ----------

                  procedure Flag
                    (Name  : String;
                     Value : Natural)
                  is
                  begin
                     if Value /= 0 then
                        Output.Add (Name, "yes");
                     end if;
                  end Flag;

                  -----------------
                  -- Next_Number --
                  -----------------

                  function Next_Number return Natural is
                     Start : Positive;
                  begin
                     while Start_Index <= Stats'Last
                       and then not Ada.Characters.Handling.Is_Digit
                         (Stats (Start_Index))
                     loop
                        Start_Index := Start_Index + 1;
                     end loop;

                     Start := Start_Index;

                     while Start_Index <= Stats'Last
                       and then Ada.Characters.Handling.Is_Digit
                         (Stats (Start_Index))
                     loop
                        Start_Index := Start_Index + 1;
                     end loop;

                     return Natural'Value (Stats (Start .. Start_Index - 1));
                  end Next_Number;

                  ------------
                  -- Number --
                  ------------

                  procedure Number
                    (Name  : String;
                     Value : Natural)
                  is
                  begin
                     if Value /= 0 then
                        Output.Add (Name, Value);
                     end if;
                  end Number;

                  Water       : constant Natural := Next_Number;
                  Land        : constant Natural := Next_Number;
                  Road        : constant Natural := Next_Number;
                  Barren      : constant Natural := Next_Number;
                  Neutral     : constant Natural := Next_Number;
                  Build       : constant Natural := Next_Number;
                  Area        : constant Natural := Next_Number;
                  Maintenance : constant Natural := Next_Number;
                  Cost        : constant Natural := Next_Number;
                  Build_Time  : constant Natural := Next_Number;
                  Enabled_By  : constant Natural := Next_Number;
                  Value       : constant Natural := Next_Number;

               begin
                  Output.Add ("index", Index);

                  if Id = "farm"
                    or else Id = "mine"
                    or else Id = "well"
                    or else Id = "aborium"
                  then
                     Output.Add ("harvester", "yes");
                     Output.Add ("production", Id);
                  elsif Production_Config.Contains (Id) then
                     declare
                        Production : Tropos.Configuration :=
                                       Tropos.New_Config ("production");
                     begin
                        for Item of Production_Config.Child (Id) loop
                           Production.Add (Item);
                        end loop;
                        Output.Add (Production);
                     end;
                  end if;

                  Category ("agora");
                  Category ("palace");
                  Category ("shield");

                  Flag ("water", Water);
                  Flag ("land", Land);
                  Flag ("road", Road);
                  Flag ("barren", Barren);
                  Flag ("neutral", Neutral);
                  Flag ("build", Build);

                  Number ("area", Area);
                  Number ("maintenance", Maintenance);
                  Number ("cost", Cost);
                  Number ("build-time", Build_Time);
                  Number ("value", Value);

                  if Enabled_By /= 0 then
                     Output.Add ("enabled-by",
                                 Technology_Vector (Enabled_By));
                  end if;

                  Load_Bonus_Table (Output);

                  Tropos.Writer.Write_Config
                    (Output,
                     Carthage.Paths.Config_File
                       ("cities/" & Output.Config_Name & ".txt"));
                  Index := Index + 1;
               end;
            end if;
         end;
      end loop;
   end Import_Cities;

   ----------------------
   -- Import_Resources --
   ----------------------

   procedure Import_Resources
     (Config : Tropos.Configuration)
   is
      At_Name  : Boolean := False;
      At_Price : Boolean := False;
      At_Desc  : Boolean := False;
      Output   : Tropos.Configuration := Tropos.New_Config ("resources");
      Current  : Tropos.Configuration;
   begin
      for Res_Config of Config.Child (1) loop
         declare
            Name : constant String := Res_Config.Config_Name;
         begin
            if Name = "name" then
               At_Name := True;
            elsif At_Name then
               Current :=
                 Tropos.New_Config
                   (Ada.Characters.Handling.To_Lower
                      (Res_Config.Config_Name));
               At_Name := False;
            elsif Name = "price" then
               At_Price := True;
            elsif At_Price then
               Current.Add ("price", Res_Config.Config_Name);
               At_Price := False;
            elsif Name = "desc" then
               At_Desc := True;
            elsif At_Desc then
               Current.Add ("desc", Res_Config.Config_Name);
               At_Desc := False;
               Output.Add (Current);
            end if;
         end;
      end loop;
      Tropos.Writer.Write_Config
        (Output, Carthage.Paths.Config_File ("resources.txt"));
   end Import_Resources;

   --------------------
   -- Import_Targets --
   --------------------

   procedure Import_Targets
     (Config : Tropos.Configuration)
   is
      At_Name : Boolean := True;
      Name_Config : Tropos.Configuration;
      Output      : Tropos.Configuration :=
                      Tropos.New_Config ("targets");
   begin
      for Target_Config of Config.Child (1) loop
         if At_Name then
            Name_Config := Tropos.New_Config (Target_Config.Config_Name);
         else
            declare
               use Carthage.Handles;
               Weapon : Weapon_Category := Weapon_Category'First;
            begin
               for Ch of Target_Config.Config_Name loop
                  if Ch = '1' then
                     Name_Config.Add
                       (Tropos.New_Config
                          (Ada.Characters.Handling.To_Lower
                               (Weapon_Category'Image (Weapon))));
                  end if;
                  if (Ch = '1' or else Ch = '0')
                    and then Weapon /= Weapon_Category'Last
                  then
                     Weapon := Weapon_Category'Succ (Weapon);
                  end if;
               end loop;
               Output.Add (Name_Config);
            end;
         end if;
         At_Name := not At_Name;
      end loop;
      Tropos.Writer.Write_Config
        (Output, Carthage.Paths.Config_File ("targets.txt"));
   end Import_Targets;

   -----------------------
   -- Import_Technology --
   -----------------------

   procedure Import_Technology
     (Config      : Tropos.Configuration;
      Write_Phase : Boolean)
   is

      At_Name  : Boolean := False;
      At_Stats : Boolean := False;

      Name_Config : Tropos.Configuration;

      Tech_Index : Natural := 0;

   begin
      for Field_Config of Config.Child (1) loop
         declare
            Field : constant String := Field_Config.Config_Name;
         begin
            if Field = "name" then
               At_Name := True;
            elsif Field = "stats" then
               At_Stats := True;
            elsif At_Name then
               Name_Config := Field_Config;
               At_Name := False;
            elsif At_Stats then

               At_Stats := False;

               declare
                  Id          : constant String :=
                                  To_Carthage_Id (Name_Config.Config_Name);
                  Stats       : constant String := Field;
                  Output      : Tropos.Configuration :=
                                  Tropos.New_Config (Id);
                  Start_Index : Positive := Stats'First;

                  function Next_Number return Natural;

                  -----------------
                  -- Next_Number --
                  -----------------

                  function Next_Number return Natural is
                     Start : Positive;
                  begin
                     while Start_Index <= Stats'Last
                       and then not Ada.Characters.Handling.Is_Digit
                         (Stats (Start_Index))
                     loop
                        Start_Index := Start_Index + 1;
                     end loop;

                     Start := Start_Index;

                     while Start_Index <= Stats'Last
                       and then Ada.Characters.Handling.Is_Digit
                         (Stats (Start_Index))
                     loop
                        Start_Index := Start_Index + 1;
                     end loop;

                     return Natural'Value (Stats (Start .. Start_Index - 1));
                  end Next_Number;

                  Enabled_1  : constant Natural := Next_Number;
                  Enabled_2  : constant Natural := Next_Number;
                  Enabled_3  : constant Natural := Next_Number;
                  Cost       : constant Natural := Next_Number;
                  Like       : constant Natural := Next_Number;
               begin

                  if not Write_Phase then
                     Technology_Vector.Append (Id);
                  else
                     Output.Add ("index", Tech_Index);

                     if Enabled_1 /= 0 and then Enabled_1 < 800 then
                        Output.Add ("enabled-by",
                                    Technology_Vector (Enabled_1));
                     end if;

                     if Enabled_2 /= 0 then
                        Output.Add ("enabled-by",
                                    Technology_Vector (Enabled_2));
                     end if;

                     if Enabled_3 /= 0 then
                        Output.Add ("enabled-by",
                                    Technology_Vector (Enabled_3));
                     end if;

                     Output.Add ("cost", Cost);
                     Output.Add ("like", Like);

                     Tropos.Writer.Write_Config
                       (Output,
                        Carthage.Paths.Config_File
                          ("technology/" & Output.Config_Name & ".txt"));
                  end if;
                  Tech_Index := Tech_Index + 1;
               end;
            end if;
         end;
      end loop;
   end Import_Technology;

   --------------------
   -- Import_Terrain --
   --------------------

   procedure Import_Terrain
     (Color_Config      : Tropos.Configuration;
      Road_Cost_Config   : Tropos.Configuration;
      Move_Cost_Config   : Tropos.Configuration)
   is
      Output : array (1 .. Color_Config.Child (1).Child_Count / 2)
        of Tropos.Configuration;

      procedure Set_World_Settings
        (Config   : in out Tropos.Configuration;
         Name     : String;
         Source   : String);

      ------------------------
      -- Set_World_Settings --
      ------------------------

      procedure Set_World_Settings
        (Config   : in out Tropos.Configuration;
         Name     : String;
         Source   : String)
      is

         Settings : Carthage.Import.Numeric_Settings (1 .. 5);
         Out_Config : Tropos.Configuration :=
                        Tropos.New_Config (Name);
      begin
         Carthage.Import.Scan_Settings (Source, Settings);
         Out_Config.Add ("normal", Settings (1));
         Out_Config.Add ("city", Settings (2));
         Out_Config.Add ("ice", Settings (3));
         Out_Config.Add ("jungle", Settings (4));
         Out_Config.Add ("barren", Settings (5));
         Out_Config.Add ("desert", Settings (5));
         Config.Add (Out_Config);
      end Set_World_Settings;

   begin
      for I in Output'Range loop
         Output (I) :=
           Tropos.New_Config
             (To_Carthage_Id
                (Color_Config.Child (1).Get (I * 2 - 1),
                 Space_Substitute => '_'));
         pragma Assert
           (String'(Color_Config.Child (1).Get (I * 2 - 1))
              = Road_Cost_Config.Child (1).Get (I * 2 - 1));
         Set_World_Settings
           (Output (I), "Colors",
            Color_Config.Child (1).Get (I * 2));
         Set_World_Settings
           (Output (I), "road-cost",
            Road_Cost_Config.Child (1).Get (I * 2));
      end loop;

      for Config of Output loop
         if Config.Config_Name /= "road" then
            Tropos.Writer.Write_Config
              (Config,
               Carthage.Paths.Config_File
                 ("terrain/" & Config.Config_Name & ".txt"));
         end if;
      end loop;

      Carthage.Import.Import_Terrain_Cost (Move_Cost_Config);

   end Import_Terrain;

   ------------------
   -- Import_Units --
   ------------------

   procedure Import_Units
     (Config : Tropos.Configuration)
   is

      Index     : Natural := 0;
      Sub_Index : Natural;

      At_Index  : Boolean := False;
      At_Name   : Boolean := False;
      At_Abbrev : Boolean := False;
      At_Stats  : Boolean := False;
      At_Art    : Boolean := False;

      Name_Config   : Tropos.Configuration;
      Abbrev_Config : Tropos.Configuration;
      Output        : Tropos.Configuration;

      Names         : WL.String_Sets.Set;

   begin
      for Unit_Config of Config loop

         At_Index := True;
         Index := Index + 1;
         Sub_Index := 0;

         for Field_Config of Unit_Config loop
            declare
               Field : constant String := Field_Config.Config_Name;
            begin
               if Field = "name" then
                  At_Name := True;
               elsif Field = "abbrev" then
                  At_Abbrev := True;
               elsif Field = "stats" then
                  At_Stats := True;
               elsif Field = "art" then
                  At_Art := True;
               elsif At_Index then
                  Index := Natural'Value (Field);
                  At_Index := False;
               elsif At_Name then
                  Name_Config := Field_Config;
                  At_Name := False;
               elsif At_Abbrev then
                  Abbrev_Config := Field_Config;
                  At_Abbrev := False;
               elsif At_Art then
                  Output.Add ("icon", Ada.Directories.Base_Name (Field));
                  Tropos.Writer.Write_Config
                    (Output,
                     Carthage.Paths.Config_File
                       ("units/" & Output.Config_Name & ".txt"));
                  At_Art := False;
               elsif At_Stats then

                  At_Stats := False;
                  Sub_Index := Sub_Index + 1;

                  declare
                     Abbrev_Id   : constant String :=
                                     To_Carthage_Id
                                       (Abbrev_Config.Config_Name);
                     Name_Id     : constant String :=
                                     To_Carthage_Id
                                       (Name_Config.Config_Name);
                     Id          : constant String :=
                                     (if Names.Contains (Abbrev_Id)
                                      then (if Names.Contains (Name_Id)
                                        then (raise Constraint_Error with
                                            "no unique name for "
                                          & Abbrev_Id & "/" & Name_Id)
                                        else Name_Id)
                                      else Abbrev_Id);
                     Stats       : constant String := Field;
                     Start_Index : Positive := Stats'First;

                     function Next_Name return String;

                     function Next_Number return Integer
                     is (Integer'Value (Next_Name));

                     ---------------
                     -- Next_Name --
                     ---------------

                     function Next_Name return String is
                        Start : Positive;
                     begin
                        while Start_Index <= Stats'Last
                          and then Stats (Start_Index) = ' '
                        loop
                           Start_Index := Start_Index + 1;
                        end loop;

                        Start := Start_Index;

                        while Start_Index <= Stats'Last
                          and then Stats (Start_Index) /= ' '
                        loop
                           Start_Index := Start_Index + 1;
                        end loop;

                        return Stats (Start .. Start_Index - 1);

                     end Next_Name;

                     Category              : constant String := Next_Name;
                     Move                  : constant Natural := Next_Number;
                     Spot                  : constant Natural := Next_Number;
                     Camo                  : constant Natural := Next_Number;
                     Ag                    : constant Natural := Next_Number;
                     Armour                : constant Natural := Next_Number;
                     Psydef                : constant Natural := Next_Number;
                     Water_Attack          : constant Natural := Next_Number;
                     Water_Strength        : constant Natural := Next_Number;
                     Indirect_Attack       : constant Natural := Next_Number;
                     Indirect_Strength     : constant Natural := Next_Number;
                     Air_Attack            : constant Natural := Next_Number;
                     Air_Strength          : constant Natural := Next_Number;
                     Direct_Attack         : constant Natural := Next_Number;
                     Direct_Strength       : constant Natural := Next_Number;
                     Close_Attack          : constant Natural := Next_Number;
                     Close_Strength        : constant Natural := Next_Number;
                     Psy_Attack            : constant Natural := Next_Number;
                     Psy_Strength          : constant Natural := Next_Number;
                     Range_Space_Attack    : constant Natural := Next_Number;
                     Range_Space_Strength  : constant Natural := Next_Number;
                     Direct_Space_Attack   : constant Natural := Next_Number;
                     Direct_Space_Strength : constant Natural := Next_Number;
                     Close_Space_Attack    : constant Natural := Next_Number;
                     Close_Space_Strength  : constant Natural := Next_Number;
                     Cargo                 : constant Natural := Next_Number;
                     Can_Be_Cargo          : constant Natural := Next_Number;
                     Non_Combat            : constant Natural := Next_Number;
                     Maintenance           : constant Integer := Next_Number;
                     Credit_Cost           : constant Natural := Next_Number;
                     Food_Cost             : constant Natural := Next_Number;
                     Energy_Cost           : constant Natural := Next_Number;
                     Metal_Cost            : constant Natural := Next_Number;
                     Trace_Cost            : constant Natural := Next_Number;
                     Exotic_Cost           : constant Natural := Next_Number;
                     Chems_Cost            : constant Natural := Next_Number;
                     Bio_Cost              : constant Natural := Next_Number;
                     Elec_Cost             : constant Natural := Next_Number;
                     CSteel_Cost           : constant Natural := Next_Number;
                     Wet_Cost              : constant Natural := Next_Number;
                     Mono_Cost             : constant Natural := Next_Number;
                     Gems_Cost             : constant Natural := Next_Number;
                     Sing_Cost             : constant Natural := Next_Number;
                     Required_Unit         : constant Integer := Next_Number;
                     Required_Building     : constant Integer := Next_Number;
                     Turns_To_Build        : constant Integer := Next_Number;
                     Tech_1                : constant Natural := Next_Number;
                     Tech_2                : constant Natural := Next_Number;
                     Tech_3                : constant Natural := Next_Number;
                     Tech_4                : constant Natural := Next_Number;
                     Tax                   : constant Natural := Next_Number;
                     Flock                 : constant Natural := Next_Number;
                     Rng                   : constant Natural := Next_Number;
                     Eat                   : constant Natural := Next_Number;
                     Rank                  : constant Natural := Next_Number;
                     RoP                   : constant Natural := Next_Number;
                  begin
                     Names.Include (Id);
                     Output := Tropos.New_Config (Id);
                     Output.Add ("index", Index);
                     Output.Add ("category", Category);
                     Output.Add ("move", Move);
                     Output.Add ("cargo", Cargo);
                     Output.Add ("spot", Spot);
                     Output.Add ("camouflage", Camo);
                     Output.Add ("agility", Ag);
                     Output.Add ("armour", Armour);
                     Output.Add ("psy-defence", Psydef);

                     declare
                        Attacks : Tropos.Configuration :=
                                    Tropos.New_Config ("attack");

                        procedure Add_Attack
                          (Name     : String;
                           Accuracy : Natural;
                           Strength : Natural);

                        ----------------
                        -- Add_Attack --
                        ----------------

                        procedure Add_Attack
                          (Name     : String;
                           Accuracy : Natural;
                           Strength : Natural)
                        is
                        begin
                           if Accuracy > 0 then
                              declare
                                 Attack : Tropos.Configuration :=
                                            Tropos.New_Config (Name);
                              begin
                                 Attack.Add ("accuracy", Accuracy);
                                 Attack.Add ("strength", Strength);
                                 Attacks.Add (Attack);
                              end;
                           end if;
                        end Add_Attack;

                     begin
                        Add_Attack
                          ("water", Water_Attack, Water_Strength);
                        Add_Attack
                          ("indirect", Indirect_Attack, Indirect_Strength);
                        Add_Attack
                          ("air", Air_Attack, Air_Strength);
                        Add_Attack
                          ("direct", Direct_Attack, Direct_Strength);
                        Add_Attack
                          ("close", Close_Attack, Close_Strength);
                        Add_Attack
                          ("psy", Psy_Attack, Psy_Strength);
                        Add_Attack
                          ("ranged-space",
                           Range_Space_Attack, Range_Space_Strength);
                        Add_Attack
                          ("direct-space",
                           Direct_Space_Attack, Direct_Space_Strength);
                        Add_Attack
                          ("close-space",
                           Close_Space_Attack, Close_Space_Strength);

                        Output.Add (Attacks);

                     end;

                     if Cargo > 0 then
                        Output.Add ("cargo", Cargo);
                     end if;

                     if Can_Be_Cargo /= 0 then
                        Output.Add ("can-be-cargo", "yes");
                     end if;

                     if Non_Combat = 0 then
                        Output.Add ("combat", "yes");
                     end if;

                     Output.Add ("maintenance", Maintenance);
                     Output.Add ("credit-cost", Credit_Cost);
                     Output.Add ("tax", Tax);
                     Output.Add ("flock", Flock);
                     Output.Add ("range", Rng);
                     Output.Add ("eat", Eat);
                     Output.Add ("rank", Rank);
                     Output.Add ("rop", RoP);

                     declare
                        Resources : Tropos.Configuration :=
                                      Tropos.New_Config ("resources");

                        procedure Add_Resource
                          (Name     : String;
                           Quantity : Natural);

                        procedure Add_Resource
                          (Name     : String;
                           Quantity : Natural)
                        is
                        begin
                           if Quantity > 0 then
                              Resources.Add (Name, Quantity);
                           end if;
                        end Add_Resource;

                     begin
                        Add_Resource ("food", Food_Cost);
                        Add_Resource ("energy", Energy_Cost);
                        Add_Resource ("metal", Metal_Cost);
                        Add_Resource ("trace", Trace_Cost);
                        Add_Resource ("exotics", Exotic_Cost);
                        Add_Resource ("chemicals", Chems_Cost);
                        Add_Resource ("bio-chemicals", Bio_Cost);
                        Add_Resource ("electronics", Elec_Cost);
                        Add_Resource ("ceramsteel", CSteel_Cost);
                        Add_Resource ("wetware", Wet_Cost);
                        Add_Resource ("monopoles", Mono_Cost);
                        Add_Resource ("gems", Gems_Cost);
                        Add_Resource ("singularities", Sing_Cost);

                        if Resources.Child_Count > 0 then
                           Output.Add (Resources);
                        end if;

                     end;

                     if Required_Unit >= 0 then
                        Output.Add ("required-unit", Required_Unit);
                     end if;

                     if Required_Building >= 0 then
                        Output.Add ("required-building", Required_Building);
                     end if;

                     if Turns_To_Build >= 0 then
                        Output.Add ("turns-to-build", Turns_To_Build);
                     end if;

                     declare
                        Required_Tech : Tropos.Configuration;
                     begin
                        if Tech_1 /= 0 then
                           Required_Tech.Add ("tec", Tech_1);
                        end if;
                        if Tech_2 /= 0 then
                           Required_Tech.Add ("tec", Tech_2);
                        end if;
                        if Tech_3 /= 0 then
                           Required_Tech.Add ("tec", Tech_3);
                        end if;
                        if Tech_4 /= 0 then
                           Required_Tech.Add ("tec", Tech_4);
                        end if;
                     end;

                  end;
               end if;
            end;
         end loop;
      end loop;
   end Import_Units;

   ------------------------------
   -- Initialize_Configuration --
   ------------------------------

   procedure Initialize_Configuration is
   begin
      Ada.Text_IO.Put_Line ("loading configuration ...");
      Init_Config :=
        Tropos.Reader.Read_Config
          (Carthage.Paths.Config_File ("init.txt"));

      if not Ada.Directories.Exists
        (Carthage.Paths.Config_File ("eofs.txt"))
      then
         Eofs_Config := Tropos.New_Config ("eofs");
         Eofs_Config.Add
           ("path",
            Init_Config.Get
              ("eofs-path",
               Carthage.Paths.Config_File ("eofs/standard")));
         Tropos.Writer.Write_Config
           (Eofs_Config, Carthage.Paths.Config_File ("eofs.txt"));
      else
         Eofs_Config :=
           Tropos.Reader.Read_Config
             (Carthage.Paths.Config_File ("eofs.txt"));
      end if;

      Ada.Text_IO.Put_Line ("  localisation");
      WL.Localisation.Read_Localisation_Directory
        (Language => WL.Localisation.To_Language ("en"),
         Path     => Carthage.Paths.Config_File ("localisation/english"));

      --        Ada.Text_IO.Put_Line ("  surface graph");
      --        Carthage.Handles.Planets.Configure.Create_Surface_Graph;

   end Initialize_Configuration;

   ------------------------
   -- Load_Configuration --
   ------------------------

   procedure Load_Configuration is
   begin
      declare
         Technology_Path : constant String :=
                             Carthage.Paths.Config_File
                               ("technology");
      begin
         if not Ada.Directories.Exists (Technology_Path) then
            Ada.Directories.Create_Directory (Technology_Path);
         end if;

         if Carthage.Options.Clear_Import_Cache
           or else not Ada.Directories.Exists
             (Technology_Path & "/nothing.txt")
         then
            Ada.Text_IO.Put_Line ("  importing fading suns tech");

            Delete_Contents (Technology_Path);

            declare
               Config : constant Tropos.Configuration :=
                          Tropos.Reader.Read_Config
                            (Fading_Suns_Data_File ("TECH"));
            begin
               Import_Technology (Config, False);
               Import_Technology (Config, True);
            end;
         end if;
      end;

      declare
         Resources_Path : constant String :=
                            Carthage.Paths.Config_File ("resources.txt");
      begin
         if Carthage.Options.Clear_Import_Cache
           or else not Ada.Directories.Exists (Resources_Path)
         then
            Ada.Text_IO.Put_Line ("   importing fading suns resources");

            Import_Resources
              (Tropos.Reader.Read_Config
                 (Fading_Suns_Data_File ("RES")));
         end if;
      end;

      Ada.Text_IO.Put_Line ("  technology");
      Load_Directory_Configuration
        ("technology",
         Carthage.Handles.Technology.Configure.Configure_Technology'Access);
      Load_Directory_Configuration
        ("technology",
         Carthage.Handles.Technology.Configure.Configure_Tree'Access);

      Ada.Text_IO.Put_Line ("  resources");
      for Config of
        Tropos.Reader.Read_Config
          (Carthage.Paths.Config_File ("resources.txt"))
      loop
         Carthage.Handles.Resources.Configure.Configure_Resource (Config);
      end loop;

      declare
         Terrain_Path : constant String :=
                          Carthage.Paths.Config_File
                            ("terrain");
      begin
         if not Ada.Directories.Exists (Terrain_Path) then
            Ada.Directories.Create_Directory (Terrain_Path);
         end if;

         if Carthage.Options.Clear_Import_Cache
           or else not Ada.Directories.Exists
             (Terrain_Path & "/grass.txt")
         then
            Ada.Text_IO.Put_Line ("  importing fading suns terrain");
            Delete_Contents (Terrain_Path);
            Import_Terrain
              (Color_Config    =>
                 Tropos.Reader.Read_Config
                   (Fading_Suns_Data_File ("TERCOLOR")),
               Road_Cost_Config =>
                 Tropos.Reader.Read_Config
                   (Fading_Suns_Data_File ("ROADCOST")),
               Move_Cost_Config =>
                 Tropos.Reader.Read_Config
                   (Fading_Suns_Data_File ("TERRCOST")));
         end if;
      end;

      Ada.Text_IO.Put_Line ("  terrain");
      Load_Directory_Configuration
        ("terrain",
         Carthage.Handles.Terrain.Configure.Configure_Terrain'Access);

      Ada.Text_IO.Put_Line ("  planet categories");
      Load_Directory_Configuration
        ("worlds",
         Carthage.Handles.Worlds.Configure.Configure_World'Access);
      Carthage.Handles.Worlds.Configure.Configure_Movement_Costs
        (Tropos.Reader.Read_Config
           (Carthage.Paths.Config_File ("terrain-cost.txt")));

      declare
         Structure_Path : constant String :=
                            Carthage.Paths.Config_File
                              ("cities");
      begin
         if not Ada.Directories.Exists (Structure_Path) then
            Ada.Directories.Create_Directory (Structure_Path);
         end if;

         if Carthage.Options.Clear_Import_Cache
           or else not Ada.Directories.Exists
             (Structure_Path & "/palace.txt")
         then
            Ada.Text_IO.Put_Line ("  importing fading suns structures");
            Delete_Contents (Structure_Path);

            Import_Cities
              (Tropos.Reader.Read_Config
                 (Fading_Suns_Data_File ("STRBUILD")));
         end if;
      end;

      Ada.Text_IO.Put_Line ("  structures");
      Load_Directory_Configuration
        ("cities",
         Carthage.Handles.Structures.Configure.Configure_Structure'Access);

      Carthage.Handles.Structures.Configure.Configure_Bonus_Production
        (Tropos.Reader.Read_Config
           (Carthage.Paths.Config_File
                ("bonus/bonus.txt")));

      declare
         Unit_Path : constant String :=
                       Carthage.Paths.Config_File
                         ("units");
      begin
         if not Ada.Directories.Exists (Unit_Path) then
            Ada.Directories.Create_Directory (Unit_Path);
         end if;

         if Carthage.Options.Clear_Import_Cache
           or else not Ada.Directories.Exists
             (Unit_Path & "/noble-45.txt")
         then
            Ada.Text_IO.Put_Line ("  importing fading suns units");
            Delete_Contents (Unit_Path);
            Import_Units
              (Tropos.Reader.Read_Config
                 (Fading_Suns_Data_File ("UNIT")));
         end if;
      end;

      Ada.Text_IO.Put_Line ("  units");
      Load_Directory_Configuration
        ("units", Carthage.Handles.Units.Configure.Configure_Unit'Access);

      if Carthage.Options.Clear_Import_Cache
        or else not Ada.Directories.Exists
          (Carthage.Paths.Config_File ("targets.txt"))
      then
         Import_Targets
           (Tropos.Reader.Read_Config
              (Fading_Suns_Data_File ("TARGET")));
      end if;

      Carthage.Handles.Units.Configure.Configure_Targets
        (Tropos.Reader.Read_Config
           (Carthage.Paths.Config_File ("targets.txt")));

   end Load_Configuration;

   ----------------------------------
   -- Load_Directory_Configuration --
   ----------------------------------

   procedure Load_Directory_Configuration
     (Directory_Name : String;
      Configure      : not null access
        procedure (Config : Tropos.Configuration))
   is
   begin
      Tropos.Reader.Read_Config
        (Path      => Carthage.Paths.Config_File (Directory_Name),
         Extension => "txt",
         Configure => Configure);
   end Load_Directory_Configuration;

   -------------------------------
   -- Load_Fading_Suns_Scenario --
   -------------------------------

   procedure Load_Fading_Suns_Scenario is
   begin
      Carthage.Import.Galaxy.Import_Galaxy
        (Eofs_Config.Get ("path") & "/GALAXY.GAL");
   end Load_Fading_Suns_Scenario;

   -------------------
   -- Load_Scenario --
   -------------------

   procedure Load_Scenario (Name : String) is

      Start_Config : constant Tropos.Configuration :=
                       Tropos.Reader.Read_Config
                         (Scenario_File (Name, "start.txt"));

      procedure Configure_House_Start
        (House : Carthage.Handles.Houses.House_Handle);

      ---------------------------
      -- Configure_House_Start --
      ---------------------------

      procedure Configure_House_Start
        (House : Carthage.Handles.Houses.House_Handle)
      is
         Category_Name : constant String :=
                           Ada.Characters.Handling.To_Lower
                             (House.Category'Image);
      begin
         if Start_Config.Contains (Category_Name) then
            declare
               Config : constant Tropos.Configuration :=
                          Start_Config.Child (Category_Name);
            begin
               --  Carthage.Handles.Houses.Configure.Configure_Start
               --    (House, Config);
               pragma Unreferenced (Config);
            end;
         end if;
      end Configure_House_Start;

   begin
      Ada.Text_IO.Put_Line ("loading scenario: " & Name);

      Ada.Text_IO.Put_Line ("  planets");
      --  Load_Directory_Configuration
      --    ("scenarios/" & Name & "/planets",
      --     Carthage.Handles.Planets.Configure.Configure_Planet'Access);
      --  Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line ("  houses");
      Load_Directory_Configuration
        ("scenarios/" & Name & "/houses",
          Carthage.Handles.Houses.Configure.Configure_House'Access);

      Ada.Text_IO.Put_Line ("  gates");
      Carthage.Handles.Galaxy.Configure.Configure_Gates
        (Tropos.Reader.Read_Config
           (Carthage.Paths.Config_File
                ("scenarios/" & Name & "/gates.txt")));

      Ada.Text_IO.Put_Line ("  star map");
      --  Load_Scenario_Configuration
      --    (Name, "starmap",
      --     Carthage.Handles.Galaxy.Configure.Configure_Positions'Access);

      Ada.Text_IO.Put_Line ("  houses");
      Carthage.Handles.Houses.For_All_Houses
        (Configure_House_Start'Access);

   end Load_Scenario;

   ---------------------------------
   -- Load_Scenario_Configuration --
   ---------------------------------

   procedure Load_Scenario_Configuration
     (Scenario_Name : String;
      File_Name     : String;
      Configure     : not null access
        procedure (Config : Tropos.Configuration))
   is
   begin
      Configure
        (Tropos.Reader.Read_Config
           (Carthage.Paths.Config_File
                ("scenarios/" & Scenario_Name & "/" & File_Name & ".txt")));
   end Load_Scenario_Configuration;

   --------------------------
   -- Load_Standard_Houses --
   --------------------------

   procedure Load_Standard_Houses is
      procedure Load (Name : String);

      ----------
      -- Load --
      ----------

      procedure Load (Name : String) is
      begin
         Carthage.Handles.Houses.Configure.Configure_House
           (Tropos.Reader.Read_Config
              (Carthage.Paths.Config_File
                   ("scenarios/standard/houses/"
                    & Name & ".txt")));
      end Load;

   begin
      Load ("li-halan");
      Load ("hazat");
      Load ("decados");
      Load ("hawkwood");
      Load ("al-malik");
      Load ("league");
      Load ("church");
      Load ("symbiot");
      Load ("vau");
      Load ("imperial");
      Load ("fleet");
      Load ("stigmata");
      Load ("spy");
      Load ("rebel");
   end Load_Standard_Houses;

   --------------------
   -- To_Carthage_Id --
   --------------------

   function To_Carthage_Id
     (Dat_Identifier   : String;
      Space_Substitute : Character := '-')
      return String
   is
      Result : String := Dat_Identifier;
   begin
      for Ch of Result loop
         if Ada.Characters.Handling.Is_Upper (Ch) then
            Ch := Ada.Characters.Handling.To_Lower (Ch);
         elsif Ch = ' ' then
            Ch := Space_Substitute;
         end if;
      end loop;
      return Result;
   end To_Carthage_Id;

end Carthage.Configure;
