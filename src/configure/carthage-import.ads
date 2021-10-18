with WL.Images;

with Tropos;

with Carthage.Colors;

package Carthage.Import is

   procedure Import_Bin_File
     (Image_Width      : Natural;
      Image_Height     : Natural;
      Bin_File_Path    : String;
      Destination_Path : String;
      Base_Name        : String);

   procedure Import_Icons
     (Icon_Width      : Natural;
      Icon_Height     : Natural;
      Bin_File_Path    : String;
      On_Load          : not null access
        procedure (Image : WL.Images.Image_Type'Class));

   procedure Import_Hexes
     (Image_Width      : Natural;
      Image_Height     : Natural;
      Bin_File_Path    : String;
      Line_Width       : not null access
        function (Line : Natural) return Natural;
      On_Load          : not null access
        procedure (Image : WL.Images.Image_Type'Class));

   function Palette_Color
     (Palette_Index : Natural)
      return Carthage.Colors.Color_Type;

   procedure Scan_Settings
     (Settings : String;
      Process  : not null access
        procedure (Index : Positive;
                   Setting : String));

   type Numeric_Settings is array (Positive range <>) of Integer;
   type Float_Settings is array (Positive range <>) of Float;

   procedure Scan_Settings
     (Settings : String;
      Result   : in out Numeric_Settings);

   procedure Scan_Settings
     (Settings : String;
      Result   : in out Float_Settings);

   procedure Import_Terrain_Cost
     (Config : Tropos.Configuration);

end Carthage.Import;
