with Tropos;

package Carthage.Colors is

   type Color_Element is new Float range 0.0 .. 1.0;

   type Color_Type is
      record
         Red, Green, Blue, Alpha : Color_Element;
      end record;

   function Configure
     (Config : Tropos.Configuration)
      return Color_Type;

   function Encode (Color : Color_Type) return Natural;
   function Decode (Value : Natural) return Color_Type;

end Carthage.Colors;
