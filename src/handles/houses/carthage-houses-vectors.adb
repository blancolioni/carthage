package body Carthage.Handles.Houses.Vectors is

   ---------------------
   -- Replace_Element --
   ---------------------

   overriding procedure Replace_Element
     (Container : in out Vector;
      This      :        not null access constant Any_Instance;
      New_Value :        Element_Type)
   is
   begin
      Object_Vectors.Vector (Container).Replace_Element (This, New_Value);
   end Replace_Element;

end Carthage.Handles.Houses.Vectors;
