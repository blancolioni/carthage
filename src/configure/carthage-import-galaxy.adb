with Ada.Characters.Handling;
with Ada.Text_IO;

with WL.Binary_IO;                     use WL.Binary_IO;

with Tropos.Reader;

with Carthage.Handles.Assets.Create;
with Carthage.Handles.Cities.Create;

with Carthage.Handles.Houses.Configure;
with Carthage.Handles.Stacks;
with Carthage.Handles.Structures;
with Carthage.Handles.Terrain;
with Carthage.Handles.Tiles.Configure;
with Carthage.Handles.Planets.Configure;
with Carthage.Handles.Resources;
with Carthage.Handles.Stacks.Create;
with Carthage.Handles.Units;

with Carthage.Quantities;

with Carthage.Configure;
with Carthage.Options;
with Carthage.Paths;

package body Carthage.Import.Galaxy is

   type File_Unit is
      record
         Planet          : Word_16;
         X, Y            : Word_16;
         Owner           : Word_8;
         U_Type          : Word_8;
         Unknown_1       : Word_8;
         Loyalty         : Word_8;
         Orders          : Word_16;
         Experience      : Word_8;
         Move_Points     : Word_8;
         Relic           : Word_8;
         Quantity        : Word_16;
         Health          : Word_8;
         Sect            : Word_8;
         Unknown_2       : Word_8;
         Unit_Number     : Word_32;
         Flags           : Word_32;
         Used_Unit_Type  : Word_8;
         Used_Unit_Level : Word_8;
         Camouflage      : Word_8;
         Destination_X   : Word_8;
         Destination_Y   : Word_8;
         Unknown_3       : Word_8;
         Task_Force      : Word_8;
         Unknown_4       : Word_16;
         Wait_Level      : Word_8;
      end record;

   type File_City is
      record
         Planet          : Word_16;
         X, Y            : Word_16;
         C_Type          : Word_16;
         Owner           : Word_16;
         Unknown_1       : Word_16;
         Ruin_Type       : Word_16;
         Production_Info : Word_16;
         Turns_Left      : Word_16;
         City_Info       : Word_16;
         Unknown_2       : Word_16;
         Unit_Loyalty    : Word_16;
         Loyalty         : Word_16;
         Stack_Info      : Word_16;
         Used_Unit_Level : Word_16;
         Tech_Type       : Word_16;
         Health          : Word_16;
         Sect            : Word_16;
         Flags           : Word_32;
      end record;

   Map_Width  : constant := 50;
   Map_Height : constant := 48;

   End_Of_Section : constant := 16#FFFE#;
   End_Of_Group   : constant := 16#FFFD#;

   Planet_Orbit_Stacks : constant := 8;
   Planet_Name_Length  : constant := 32;

   Planet_File_Width   : constant := Planet_Width;
   Planet_File_Height  : constant := 65;

   type Tile_Land_Bits is mod 2 ** 3;
   Ocean_Mask      : constant := 2#1111#;
   Ocean_Tile      : constant := 2#0000#;

   Land_Mask       : constant := 2#0111#;
   Grass_Tile      : constant := 2#0001#;
   Arid_Grass_Tile : constant := 2#0010#;
   Desert_Tile     : constant := 2#0011#;
   Ice_Tile        : constant := 2#0100#;
   Tundra_Tile     : constant := 2#0101#;
   Mountain_Tile   : constant := 2#0110#;
   Hill_Tile       : constant := 2#0111#;

   Road_Mask       : constant := 2#1011#;
   Road_Tile       : constant := 2#1011#;

   Tree_Mask       : constant := 2#1000#;

   Unit_On_Ground_Mask : constant := 16#0001#;
   Unit_Sentry_Mask    : constant := 16#0004#;
   Unit_Cargo_Mask     : constant := 16#0040#;

   House_Map  : array (Word_8) of Carthage.Handles.Houses.House_Handle;
   Planet_Map : array (Word_16 range 0 .. 255)
     of Carthage.Handles.Planets.Planet_Handle;
   Next_Planet : Word_16 := 0;

   procedure Read_Galaxy_File
     (File : in out File_Type);

   function Read_City
     (File : in out File_Type)
      return Boolean;
   --  Read a city from the file.  Return False if there are
   --  no more cities

   function Read_Jump_Gate
     (File : in out File_Type)
      return Boolean;
   --  Read a jump gate from the file.  Return False if there are
   --  no more gates

   function Read_Planet
     (File           : in out File_Type;
      Galaxy_Version : Word_32)
      return Boolean;

   function Read_Unit
     (File : in out File_Type)
      return Boolean;
   --  Read a unit from the file.  Return False if there are
   --  no more units

   -------------------
   -- Import_Galaxy --
   -------------------

   procedure Import_Galaxy
     (Path : String)
   is
      File : File_Type;
   begin

      --  Galaxy file has no houses, so pull them from the standard scenario
      Carthage.Configure.Load_Standard_Houses;

      House_Map (0) := Carthage.Handles.Houses.Get ("li-halan");
      House_Map (1) := Carthage.Handles.Houses.Get ("hazat");
      House_Map (2) := Carthage.Handles.Houses.Get ("decados");
      House_Map (3) := Carthage.Handles.Houses.Get ("hawkwood");
      House_Map (4) := Carthage.Handles.Houses.Get ("al-malik");
      House_Map (5) := Carthage.Handles.Houses.Get ("league");
      House_Map (6) := Carthage.Handles.Houses.Get ("church");
      House_Map (7) := Carthage.Handles.Houses.Get ("symbiot");
      House_Map (8) := Carthage.Handles.Houses.Get ("vau");
      House_Map (9) := Carthage.Handles.Houses.Get ("imperial");
      House_Map (10) := Carthage.Handles.Houses.Get ("fleet");
      House_Map (11) := Carthage.Handles.Houses.Get ("stigmata");
      House_Map (12) := Carthage.Handles.Houses.Get ("spy");
      House_Map (13) := Carthage.Handles.Houses.Get ("rebel");

      Ada.Text_IO.Put_Line
        ("importing galaxy from: " & Path);

      Open (File, In_File, Path);
      Read_Galaxy_File (File);
      Close (File);

      declare

         procedure Initialize (House : Carthage.Handles.Houses.House_Handle);

         ----------------
         -- Initialize --
         ----------------

         procedure Initialize
           (House : Carthage.Handles.Houses.House_Handle)
         is
         begin
            Carthage.Handles.Houses.Configure.Configure_Initial_State
              (Tropos.Reader.Read_Config
                 (Carthage.Paths.Config_File
                      ("scenarios/standard/houses/"
                       & House.Tag & ".txt")));
         end Initialize;

      begin
         Carthage.Handles.Houses.For_All_Houses
           (Initialize'Access);
      end;

   end Import_Galaxy;

   ---------------
   -- Read_City --
   ---------------

   function Read_City
     (File : in out File_Type)
      return Boolean
   is

      City          : File_City;
      Check_Section : Word_16;
   begin
      Read (File, Check_Section);
      if Check_Section = End_Of_Section then
         return False;
      end if;

      City.Planet := Check_Section;
      Read (File, City.X);
      Read (File, City.Y);
      Read (File, City.C_Type);
      Read (File, City.Owner);
      Read (File, City.Unknown_1);
      Read (File, City.Ruin_Type);
      Read (File, City.Production_Info);
      Read (File, City.Turns_Left);
      Read (File, City.City_Info);
      Read (File, City.Unknown_2);
      Read (File, City.Unit_Loyalty);
      Read (File, City.Loyalty);
      Read (File, City.Stack_Info);
      Read (File, City.Used_Unit_Level);
      Read (File, City.Tech_Type);
      Read (File, City.Health);
      Read (File, City.Sect);
      Read (File, City.Flags);

      declare
         Planet   : constant Carthage.Handles.Planets.Planet_Handle :=
                      Planet_Map (City.Planet);
         Tile     : constant Carthage.Handles.Tiles.Tile_Handle :=
                      Planet.Get_Tile
                        (Tile_X (City.X + 1),
                         Tile_Y
                           (Word_16'Min (City.Y / 2 + 1, Planet_Height)));
         Structure : constant Carthage.Handles.Structures.Structure_Handle :=
                       Carthage.Handles.Structures.Get_By_Index
                         (Natural (City.C_Type) + 1);

         New_City : constant Carthage.Handles.Cities.City_Handle :=
                       Carthage.Handles.Cities.Create.New_City
                         (Planet    => Planet,
                          Tile      => Tile,
                          Structure => Structure,
                          Owner     => House_Map (Word_8 (City.Owner)),
                          Health    => Health_Type (City.Health),
                          Loyalty   => Loyalty_Type (City.Loyalty));
      begin
         pragma Unreferenced (New_City);
      end;

      return True;

   end Read_City;

   ----------------------
   -- Read_Galaxy_File --
   ----------------------

   procedure Read_Galaxy_File
     (File : in out File_Type)
   is
      Version    : Word_32;
      Unit_Count : Word_32;
      Map_Tiles  : array (1 .. Map_Width, 1 .. Map_Height) of Word_32;

   begin
      Read (File, Version);

      Ada.Text_IO.Put_Line ("reading galaxy version" & Version'Image);

      Read (File, Unit_Count);
      for Y in Map_Tiles'Range (2) loop
         for X in Map_Tiles'Range (1) loop
            Read (File, Map_Tiles (X, Y));
         end loop;
      end loop;

      while Read_Planet (File, Version) loop
         null;
      end loop;

      Ada.Text_IO.New_Line;

      while Read_Jump_Gate (File) loop
         null;
      end loop;

      while Read_Unit (File) loop
         null;
      end loop;

      while Read_City (File) loop
         null;
      end loop;

   end Read_Galaxy_File;

   --------------------
   -- Read_Jump_Gate --
   --------------------

   function Read_Jump_Gate
     (File : in out File_Type)
      return Boolean
   is
      X1, Y1, X2, Y2 : Word_16;
      Flags          : Word_32;
   begin
      Read (File, X1);
      if X1 = End_Of_Section then
         return False;
      end if;

      Read (File, Y1);
      Read (File, X2);
      Read (File, Y2);
      Read (File, Flags);

      Carthage.Handles.Planets.Configure.Import_Jump_Gate
        (Natural (X1), Natural (Y1), Natural (X2), Natural (Y2));

      return True;

   end Read_Jump_Gate;

   -----------------
   -- Read_Planet --
   -----------------

   function Read_Planet
     (File           : in out File_Type;
      Galaxy_Version : Word_32)
      return Boolean
   is
      X, Y, R          : Word_16;
      Orbiting_Stacks  : array (1 .. Planet_Orbit_Stacks) of Word_32;
      Name             : String (1 .. Planet_Name_Length);
      Name_Start       : Positive := 1;
      Name_End         : Natural := 0;
      Owner            : Word_16;
      Sect             : Word_16;
      Flags            : Word_32;
      Tile_Set         : Word_16;
      Map              : array
        (1 .. Planet_File_Width * Planet_File_Height) of Word_32;

      To_File_Map      : array (Tile_X, Tile_Y) of Positive;

   begin

      Read (File, X);

      if X = End_Of_Section then
         return False;
      end if;

      Read (File, Y);
      Read (File, R);

      for Stack of Orbiting_Stacks loop
         Read (File, Stack);
      end loop;

      declare
         Done : Boolean := False;
         X    : Word_8;
      begin
         for Ch of Name loop
            Read (File, X);
            Done := Done or else X = 0;
            Ch := Character'Val (X);
            if not Done then
               Name_End := Name_End + 1;
            end if;
         end loop;
      end;

      --  Madoc starts with a space!
      while Name (Name_Start) = ' ' loop
         Name_Start := Name_Start + 1;
      end loop;

      declare
         use Ada.Characters.Handling;
      begin
         for I in Name_Start .. Name_End loop
            if Is_Upper (Name (I)) then
               Name (I) := To_Lower (Name (I));
            elsif Name (I) = ' ' then
               Name (I) := '-';
            end if;
         end loop;
      end;

      Read (File, Owner);
      Read (File, Sect);
      Read (File, Flags);
      Read (File, Tile_Set);

      if Galaxy_Version < 961024 then
         Skip (File, 12);
      end if;

      Skip (File, 2);

      for Hex_Flags of Map loop
         Read (File, Hex_Flags);
      end loop;

      for File_X in 1 .. Planet_File_Width loop
         declare
            Row : Natural := 0;
         begin
            for File_Y in 1 .. Planet_File_Height - 1 loop
               declare
                  Index : constant Positive :=
                            Planet_File_Height * (File_X - 1) + File_Y;
               begin
                  if (File_X mod 2 = 1 and then File_Y mod 2 = 0)
                    or else (File_X mod 2 = 0 and then File_Y mod 2 = 1)
                  then
                     Row := Row + 1;
                     To_File_Map (File_X, Row) := Index;
                  end if;
               end;
            end loop;
         end;
      end loop;

      if Carthage.Options.Trace_Planet_Import then
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line
           ("--- " & Name (Name_Start .. Name_End) & " ---");
      end if;

      declare
         function Create_Tile
           (Planet : Carthage.Handles.Planet_Reference;
            X      : Tile_X;
            Y      : Tile_Y)
            return Carthage.Handles.Tile_Reference;

         -----------------
         -- Create_Tile --
         -----------------

         function Create_Tile
           (Planet : Carthage.Handles.Planet_Reference;
            X      : Tile_X;
            Y      : Tile_Y)
            return Carthage.Handles.Tile_Reference
         is
            Ts : Carthage.Handles.Tiles.Terrain_Layer_Array :=
                   (others => Carthage.Handles.Terrain.Empty_Handle);
            Last : Natural := 0;

            procedure Add (Id : String);

            ---------
            -- Add --
            ---------

            procedure Add (Id : String) is
            begin
               Last := Last + 1;
               Ts (Last) := Carthage.Handles.Terrain.Get (Id);
            exception
               when others =>
                  raise Constraint_Error with
                    "while importing " & Name (Name_Start .. Name_End)
                    & ": no such terrain id: " & Id;
            end Add;

            Index : constant Positive := To_File_Map (X, Y);
            Flag  : Word_32  := Map (Index);
            Road  : constant Boolean := (Flag and Road_Mask) = Road_Tile;
         begin

            if Carthage.Options.Trace_Planet_Import then
               if X = Tile_X'First then
                  Ada.Text_IO.Put (Y'Image & ":");
                  Ada.Text_IO.Set_Col (11);
               end if;

               Ada.Text_IO.Put (" " & Hex_Image (Flag));

               if X = Tile_X'Last then
                  Ada.Text_IO.New_Line;
               end if;
            end if;

            if Road then
               Flag := Flag / 2 ** 9;
            end if;

            if (Flag and Ocean_Mask) = Ocean_Tile then
               Add ("ocean");
            else
               case Tile_Land_Bits (Flag and Land_Mask) is
                  when 0 =>
                     null;  --  forest-only tile
                  when Grass_Tile =>
                     Add ("grass");
                  when Arid_Grass_Tile =>
                     Add ("arid_grass");
                  when Desert_Tile =>
                     Add ("desert");
                  when Ice_Tile =>
                     Add ("ice");
                  when Tundra_Tile =>
                     Add ("tundra");
                  when Mountain_Tile =>
                     Add ("mountain");
                  when Hill_Tile =>
                     Add ("hill");
               end case;

               if (Flag and Tree_Mask) = Tree_Mask then
                  Add ("tree");
               end if;
            end if;

            declare
               Tile : constant Carthage.Handles.Tiles.Tile_Handle :=
                        Carthage.Handles.Tiles.Configure.Create_Tile
                          (Planet   => Planet,
                           Position => (X, Y),
                           Terrain  => Ts,
                           Road     => Road,
                           River    => False);
            begin
               return Tile.Reference;
            end;
         end Create_Tile;

         Planet : constant Carthage.Handles.Planets.Planet_Handle :=
                    Carthage.Handles.Planets.Configure.Import_Planet
                      (Ada.Characters.Handling.To_Lower
                         (Name (Name_Start .. Name_End)),
                       Natural (X), Natural (Y), Natural (Tile_Set),
                       Create_Tile'Access);
      begin
         Planet_Map (Next_Planet) := Planet;
         Next_Planet := Next_Planet + 1;
      end;

      if Carthage.Options.Trace_Planet_Import then
         Ada.Text_IO.New_Line;
      end if;

      return True;

   end Read_Planet;

   ---------------
   -- Read_Unit --
   ---------------

   function Read_Unit
     (File : in out File_Type)
      return Boolean
   is

      Unit : File_Unit;
      Check_Section : Word_16;
   begin
      Read (File, Check_Section);
      if Check_Section = End_Of_Section then
         return False;
      elsif Check_Section = End_Of_Group then
         Read (File, Check_Section);
         if Check_Section = End_Of_Section then
            return False;
         end if;
      end if;

      Unit.Planet := Check_Section;
      Read (File, Unit.X);
      Read (File, Unit.Y);
      Read (File, Unit.Owner);
      Read (File, Unit.U_Type);
      Read (File, Unit.Unknown_1);
      Read (File, Unit.Loyalty);
      Read (File, Unit.Orders);
      Read (File, Unit.Experience);
      Read (File, Unit.Move_Points);
      Read (File, Unit.Relic);
      Read (File, Unit.Quantity);
      Read (File, Unit.Health);
      Read (File, Unit.Sect);
      Read (File, Unit.Unknown_2);
      Read (File, Unit.Unit_Number);
      Read (File, Unit.Flags);
      Read (File, Unit.Used_Unit_Type);
      Read (File, Unit.Used_Unit_Level);
      Read (File, Unit.Camouflage);
      Read (File, Unit.Destination_X);
      Read (File, Unit.Destination_Y);
      Read (File, Unit.Unknown_3);
      Read (File, Unit.Task_Force);
      Read (File, Unit.Unknown_4);
      Read (File, Unit.Wait_Level);

      declare
         In_Space : constant Boolean :=
                      (Unit.Flags and Unit_On_Ground_Mask) = 0;
         Sentry   : constant Boolean :=
                      (Unit.Flags and Unit_Sentry_Mask)
                      = Unit_Sentry_Mask
           with Unreferenced;
         Cargo    : constant Boolean :=
                      (Unit.Flags and Unit_Cargo_Mask)
                      = Unit_Cargo_Mask;
         Planet   : constant Carthage.Handles.Planets.Planet_Handle :=
                      Planet_Map (Unit.Planet);
         --  Planet_Map (Word_8 (Unit.Planet));
         Tile     : constant Carthage.Handles.Tiles.Tile_Handle :=
                      (if In_Space or else Cargo
                       then Carthage.Handles.Tiles.Empty_Handle
                       else Planet.Get_Tile
                         (X    => Tile_X (Unit.X + 1),
                          Y    => Tile_Y (Unit.Y / 2 + 1)));
         Stack    : constant Carthage.Handles.Stack_Reference :=
                      (if In_Space
                       then Planet.Orbital_Stack
                         (House_Map (Unit.Owner).Reference)
                       elsif Tile.Has_Stacks
                       then Tile.First_Stack
                       else Carthage.Handles.Stacks.Create.New_Ground_Stack
                         (null, House_Map (Unit.Owner),
                          Planet.Reference, Tile)
                       .Reference);
         Asset    : constant Carthage.Handles.Assets.Asset_Handle :=
                      Carthage.Handles.Assets.Create.New_Asset
                        (Unit    => Carthage.Handles.Units.Get_By_Index
                           (Natural (Unit.U_Type) + 1),
                         Owner   => House_Map (Unit.Owner),
                         Stack   => Stack,
                         XP      =>
                           Carthage.Handles.Assets.Asset_Experience'Val
                             (Unit.Experience),
                         Loyalty => Loyalty_Type (Unit.Loyalty),
                         Health  => Health_Type (Unit.Health));
         Resource : constant Carthage.Handles.Resources.Resource_Handle :=
                      Carthage.Handles.Resources.Get_By_Index
                        (Natural (Unit.Relic) + 1);
      begin
         if Unit.Quantity > 0
           and then Resource.Has_Element
         then
            Asset.Set_Resource (Resource);
            Asset.Add_Quantity
              (Carthage.Quantities.To_Quantity (Real (Unit.Quantity)));
         end if;
      end;

      return True;

   end Read_Unit;

end Carthage.Import.Galaxy;
