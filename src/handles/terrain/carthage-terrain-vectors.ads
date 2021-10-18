with Carthage.Objects.Vectors;

generic
   type Element_Type is private;
   Default_Value : Element_Type;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Carthage.Handles.Terrain.Vectors is

   type Vector is tagged private;

   function Element (Container : Vector;
                     Terrain     : not null access constant Any_Instance)
                     return Element_Type;

   procedure Replace_Element
     (Container   : in out Vector;
      Terrain     : not null access constant Any_Instance;
      New_Element : Element_Type);

private

   package Object_Vectors is
     new Carthage.Objects.Vectors
       (Instance, Element_Type, Default_Value, "=");

   type Vector is new Object_Vectors.Vector with null record;

   overriding function Element
     (Container : Vector;
      Terrain     : not null access constant Any_Instance)
      return Element_Type
   is (Object_Vectors.Vector (Container).Element (Terrain));

end Carthage.Handles.Terrain.Vectors;
