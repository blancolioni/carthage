with Carthage.Import;

package body Carthage.Handles.Terrain.Configure is

   -----------------------
   -- Configure_Terrain --
   -----------------------

   procedure Configure_Terrain
     (Config : Tropos.Configuration)
   is
      Ocean : constant Boolean := Config.Config_Name = "ocean";
      Water : constant Boolean := Ocean or else Config.Get ("water");

      Color_Config : constant Tropos.Configuration :=
                       Config.Child ("colors");
      Road_Config  : constant Tropos.Configuration :=
                       Config.Child ("road-cost");

      procedure Configure_Category
        (Category_Name : String;
         Color         : out Carthage.Colors.Color_Type;
         Road_Cost     : out Natural);

      ------------------------
      -- Configure_Category --
      ------------------------

      procedure Configure_Category
        (Category_Name : String;
         Color         : out Carthage.Colors.Color_Type;
         Road_Cost     : out Natural)
      is
      begin
         Color :=
           Carthage.Import.Palette_Color (Color_Config.Get (Category_Name));
         Road_Cost :=
           Road_Config.Get (Category_Name);
      end Configure_Category;

   begin
      Create
        (Tag                => Config.Config_Name,
         Ocean              => Ocean,
         Water              => Water,
         Configure_Category => Configure_Category'Access);
   end Configure_Terrain;

end Carthage.Handles.Terrain.Configure;
