with WL.String_Maps;

package body Carthage.UI.Models is

   package Model_Maps is
     new WL.String_Maps (Lui.Models.Object_Model, Lui.Models."=");

   Model_Map : Model_Maps.Map;

   ------------------
   -- After_Render --
   ------------------

--     overriding procedure After_Render
--       (Model    : in out Root_Carthage_Model;
--        Renderer : in out Lui.Rendering.Root_Renderer'Class)
--     is null;

   -------------------
   -- Before_Render --
   -------------------

--     overriding procedure Before_Render
--       (Model    : in out Root_Carthage_Model;
--        Renderer : in out Lui.Rendering.Root_Renderer'Class)
--     is null;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (House : Carthage.Handles.Houses.House_Handle;
      Key   : String)
      return Lui.Models.Object_Model
   is
   begin
      return Model_Map.Element (House.Tag & "--" & Key);
   end Get_Model;

   ----------------
   -- Have_Model --
   ----------------

   function Have_Model
     (House : Carthage.Handles.Houses.House_Handle;
      Key   : String)
      return Boolean
   is
   begin
      return Model_Map.Contains (House.Tag & "--" & Key);
   end Have_Model;

   ----------------
   -- Save_Model --
   ----------------

   procedure Save_Model
     (Model    : not null access Lui.Models.Root_Object_Model'Class;
      Class_Id : String)
   is
      M : constant Carthage_Model := Carthage_Model (Model);
   begin
      Model_Map.Insert
        (M.House.Tag & "--" & Class_Id,
         Lui.Models.Object_Model (M));
   end Save_Model;

   -------------------
   -- To_Lui_Color --
   -------------------

   function To_Lui_Color
     (Color : Carthage.Colors.Color_Type)
      return Lui.Colors.Color_Type
   is
      use Carthage.Colors;
      use Lui.Colors;
   begin
      return Apply_Alpha
        (To_Color
           (Red   => Color_Byte (Color.Red * 255.0),
            Green => Color_Byte (Color.Green * 255.0),
            Blue  => Color_Byte (Color.Blue * 255.0)),
         Lui.Unit_Real (Color.Alpha));
   end To_Lui_Color;

end Carthage.UI.Models;
