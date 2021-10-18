package body Carthage.Handles.Terrain.Vectors is

   ---------------------
   -- Replace_Element --
   ---------------------

   overriding procedure Replace_Element
     (Container   : in out Vector;
      Terrain     :        not null access constant Any_Instance;
      New_Element :        Element_Type)
   is
   begin
      Object_Vectors.Vector (Container).Replace_Element (Terrain, New_Element);
   end Replace_Element;

end Carthage.Handles.Terrain.Vectors;
