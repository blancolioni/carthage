private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Tropos;

with Carthage.Handles.Houses;
with Carthage.Handles.Planets;

with Carthage.Colors;

package Carthage.UI.Maps is

   type Layer_Element_Type is
     (Background_Hex_Tile, Hex_Tile);

   type Tile_Layers is tagged limited private;

   procedure Get_Tile_Layers
     (Planet   : Carthage.Handles.Planets.Planet_Handle;
      House    : Carthage.Handles.Houses.House_Handle;
      Position : Tile_Position;
      Layers   : in out Tile_Layers'Class);

   procedure Scan_Layers
     (Layers  : Tile_Layers'Class;
      Process : not null access
        procedure (Element : Layer_Element_Type;
                   Resource_Name : String;
                   Color : Carthage.Colors.Color_Type));

   procedure Configure_Tile_Resources
     (Config : Tropos.Configuration);

private

   type Layer_Element (Length : Natural) is
      record
         Layer_Element : Layer_Element_Type;
         Resource_Name : String (1 .. Length);
         Color         : Carthage.Colors.Color_Type;
      end record;

   package Resource_Name_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Layer_Element);

   type Tile_Layers is tagged limited
      record
         List : Resource_Name_Lists.List;
      end record;

end Carthage.UI.Maps;
