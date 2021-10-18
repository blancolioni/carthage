with Ada.Strings.Fixed;

with WL.Binary_IO;                     use WL.Binary_IO;
with WL.Bitmap_IO;
with WL.Images.Bitmaps;

with Tropos.Writer;

with Carthage.Paths;

package body Carthage.Import is

   Have_Palette : Boolean := False;

   Palette      : array (Word_8) of WL.Images.Image_Color;

   procedure Import_Palette;

   ---------------------
   -- Import_Bin_File --
   ---------------------

   procedure Import_Bin_File
     (Image_Width      : Natural;
      Image_Height     : Natural;
      Bin_File_Path    : String;
      Destination_Path : String;
      Base_Name        : String)
   is
      use WL.Bitmap_IO;
      File : File_Type;
      BM    : Bitmap_Type := New_Bitmap (Image_Width, Image_Height);
      Index : Integer := 0;
   begin
      if not Have_Palette then
         Import_Palette;
      end if;

      Open (File, In_File, Bin_File_Path);

      while not End_Of_File (File) loop
         for Y in 1 .. Image_Height loop
            for X in 1 .. Image_Width loop
               declare
                  B : Word_8;
               begin
                  Read (File, B);

                  declare
                     Color : constant Color_Type :=
                               (R     => Color_Element (Palette (B).Red),
                                G     => Color_Element (Palette (B).Green),
                                B     => Color_Element (Palette (B).Blue),
                                Alpha => Color_Element (Palette (B).Alpha));
                  begin
                     Set_Color
                       (Item  => BM,
                        X     => X - 1,
                        Y     => Image_Height - Y,
                        Color => Color);
                  end;
               end;
            end loop;
         end loop;

         Index := Index - 1;

         Write (BM, Destination_Path & "/" & Base_Name
                & Integer'Image (Index) & ".bmp");
      end loop;

      Close (BM);

      Close (File);
   end Import_Bin_File;

   ------------------
   -- Import_Hexes --
   ------------------

   procedure Import_Hexes
     (Image_Width      : Natural;
      Image_Height     : Natural;
      Bin_File_Path    : String;
      Line_Width       : not null access
        function (Line : Natural) return Natural;
      On_Load          : not null access
        procedure (Image : WL.Images.Image_Type'Class))
   is
      use WL.Images;
      File   : File_Type;
      Image  : Image_Type;
      Offset : Word_32 := 0;
      Index  : Natural := 0;
--        Set_Name : constant String :=
--                     Ada.Directories.Simple_Name (Bin_File_Path);

   begin
      if not Have_Palette then
         Import_Palette;
      end if;

      Open (File, In_File, Bin_File_Path);

      Image.Create (Pixel_X_Count (Image_Width),
                    Pixel_Y_Count (Image_Height));

      while Offset < Length (File) loop

         Set_Offset (File, Offset);

         for Y in 1 .. Image.Height loop
            declare
               Width   : constant Natural := Line_Width (Natural (Y) - 1);
               Start   : constant Natural :=
                           abs (Image_Width / 2 - Width / 2);
               Finish  : constant Natural :=
                           Image_Width - Start - 1;
               Image_Y : constant Pixel_Y_Range :=
                           Image.Height + 1 - Y;
            begin
               for X in 1 .. Image.Width loop
                  if Integer (X) - 1 in Start .. Finish then
                     declare
                        B : Word_8;
                     begin
                        Read (File, B);
                        Image.Set_Color (X, Image_Y, Palette (B));
                     end;
                  else
                     Image.Set_Color (X, Image_Y, (0, 0, 0, 0));
                  end if;
               end loop;
            end;
         end loop;

         Index := Index + 1;

         On_Load (Image);

--           if True then
--              declare
--                 Writer : WL.Images.Bitmaps.Bitmap_Image_Writer;
--              begin
--                 Writer.Write
--                   (Set_Name & Integer'Image (-Index) & ".bmp",
--                    Image);
--              end;
--           end if;

--           Write (BM,
--                  Destination_Path & "/" & Base_Name
--                  & Integer'Image (-Index)
--                  & ".bmp");

         Offset := Offset + 1520;
      end loop;

      Close (File);
   end Import_Hexes;

   ------------------
   -- Import_Icons --
   ------------------

   procedure Import_Icons
     (Icon_Width       : Natural;
      Icon_Height      : Natural;
      Bin_File_Path    : String;
      On_Load          : not null access
        procedure (Image : WL.Images.Image_Type'Class))
   is
      use WL.Images;
      File   : File_Type;
      Image  : Image_Type;
      Index  : Natural := 0;
   begin
      if not Have_Palette then
         Import_Palette;
      end if;

      Open (File, In_File, Bin_File_Path);

      Image.Create (Pixel_X_Count (Icon_Width),
                    Pixel_Y_Count (Icon_Height));

      while Current_Offset (File) < Length (File) loop

         for Y in 1 .. Image.Height loop
            declare
               Image_Y : constant Pixel_Y_Range :=
                           Image.Height + 1 - Y;
            begin
               for X in 1 .. Image.Width loop
                  declare
                     B : Word_8;
                  begin
                     Read (File, B);

                     if B = 0 then
                        Image.Set_Color (X, Image_Y, (0, 0, 0, 0));
                     else
                        Image.Set_Color (X, Image_Y, Palette (B));
                     end if;
                  end;
               end loop;
            end;
         end loop;

         Index := Index + 1;

         On_Load (Image);

      end loop;

      Close (File);
   end Import_Icons;

   --------------------
   -- Import_Palette --
   --------------------

   procedure Import_Palette is
      use WL.Images;
      Image : Image_Type;
      Reader : Bitmaps.Bitmap_Image_Reader;
      Config : Tropos.Configuration;
   begin
      Reader.Read (Carthage.Paths.Config_File ("ui/bincolors.bmp"),
                   Image);
      for I in Word_8 loop
         Palette (I) := Image.Color (Pixel_X_Range (I mod 16 + 1),
                                     Pixel_Y_Range (I / 16 + 1));
         if I = 0 then
            Palette (I) := (1, 1, 1, 0);
         else
            Palette (I).Alpha := 255;
         end if;
      end loop;

      Config := Tropos.New_Config ("palette");
      for I in Palette'Range loop
         declare
            Item : Tropos.Configuration :=
                     Tropos.New_Config (Natural (I));
         begin
            Item.Add (Tropos.New_Config (Natural (Palette (I).Red)));
            Item.Add (Tropos.New_Config (Natural (Palette (I).Green)));
            Item.Add (Tropos.New_Config (Natural (Palette (I).Blue)));
            Item.Add (Tropos.New_Config (Natural (Palette (I).Alpha)));
            Config.Add (Item);
         end;
      end loop;

      Tropos.Writer.Write_Config
        (Config, Carthage.Paths.Config_File ("ui/palette.txt"));

      Have_Palette := True;
   end Import_Palette;

   -------------------------
   -- Import_Terrain_Cost --
   -------------------------

   procedure Import_Terrain_Cost
     (Config : Tropos.Configuration)
   is
      Cost_Config : Tropos.Configuration :=
                      Tropos.New_Config ("terrain-cost");
   begin
      for Item_Config of Config loop
         declare
            First : Boolean := True;
            World : Boolean := True;
            Terrain_Child : Tropos.Configuration;
            World_Child   : Tropos.Configuration;
         begin
            for Field_Config of Item_Config loop
               declare
                  Field : constant String := Field_Config.Config_Name;
               begin
                  if First then
                     Terrain_Child := Tropos.New_Config (Field);
                     First := False;
                  elsif World then
                     World_Child := Tropos.New_Config (Field);
                     World := False;
                  else
                     declare
                        Movement : Float_Settings (1 .. 10);
                     begin
                        Scan_Settings (Field, Movement);
                        for I in Movement'Range loop
                           World_Child.Add
                             (Ada.Strings.Fixed.Trim
                                (Positive'Image (I),
                                 Ada.Strings.Left),
                              Movement (I));
                        end loop;
                        Terrain_Child.Add (World_Child);
                        World := True;
                     end;
                  end if;
               end;
            end loop;
            Cost_Config.Add (Terrain_Child);
         end;
      end loop;

      Tropos.Writer.Write_Config
        (Cost_Config,
         Carthage.Paths.Config_File ("terrain-cost.txt"));

   end Import_Terrain_Cost;

   --------------------
   -- Palette_Color --
   --------------------

   function Palette_Color
     (Palette_Index : Natural)
      return Carthage.Colors.Color_Type
   is
   begin
      if not Have_Palette then
         Import_Palette;
      end if;

      declare
         Color : constant WL.Images.Image_Color :=
                    Palette (Word_8 (Palette_Index));
      begin
         return (Red   =>
                   Carthage.Colors.Color_Element
                     (Float (Color.Red) / 255.0),
                 Green =>
                   Carthage.Colors.Color_Element
                     (Float (Color.Green) / 255.0),
                 Blue  =>
                   Carthage.Colors.Color_Element
                     (Float (Color.Blue) / 255.0),
                 Alpha =>
                   Carthage.Colors.Color_Element
                     (Float (Color.Alpha) / 255.0));
      end;
   end Palette_Color;

   -------------------
   -- Scan_Settings --
   -------------------

   procedure Scan_Settings
     (Settings : String;
      Process  : not null access
        procedure (Index : Positive;
                   Setting : String))
   is
      Setting_Index : Natural := 0;
      Start_Index   : Positive := Settings'First;

      function Next_Name return String;

      ---------------
      -- Next_Name --
      ---------------

      function Next_Name return String is
         Start : Positive;
      begin
         while Start_Index <= Settings'Last
           and then Settings (Start_Index) = ' '
         loop
            Start_Index := Start_Index + 1;
         end loop;

         Start := Start_Index;

         while Start_Index <= Settings'Last
           and then Settings (Start_Index) /= ' '
         loop
            Start_Index := Start_Index + 1;
         end loop;

         return Settings (Start .. Start_Index - 1);

      end Next_Name;

   begin
      while Start_Index <= Settings'Last loop
         Setting_Index := Setting_Index + 1;
         Process (Setting_Index, Next_Name);
      end loop;
   end Scan_Settings;

   -------------------
   -- Scan_Settings --
   -------------------

   procedure Scan_Settings
     (Settings : String;
      Result   : in out Numeric_Settings)
   is
      procedure Set (Index : Positive;
                     Name  : String);

      ---------
      -- Set --
      ---------

      procedure Set (Index : Positive;
                     Name  : String)
      is
      begin
         if Index in Result'Range then
            Result (Index) := Integer'Value (Name);
         end if;
      end Set;

   begin
      Scan_Settings (Settings, Set'Access);
   end Scan_Settings;

   -------------------
   -- Scan_Settings --
   -------------------

   procedure Scan_Settings
     (Settings : String;
      Result   : in out Float_Settings)
   is
      procedure Set (Index : Positive;
                     Name  : String);

      ---------
      -- Set --
      ---------

      procedure Set (Index : Positive;
                     Name  : String)
      is
      begin
         if Index in Result'Range then
            Result (Index) := Float'Value (Name);
         end if;
      end Set;

   begin
      Scan_Settings (Settings, Set'Access);
   end Scan_Settings;

end Carthage.Import;
