private with Ada.Containers.Vectors;

with Ada.Iterator_Interfaces;
with Ada.Streams;

generic
   type Index_Type is range <>;
   type Element_Type (<>) is private;
   Contents_Name : String;
   --  with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Carthage.Handles.Vectors is

   type Vector is tagged limited private
     with
       Constant_Indexing => Constant_Reference,
       Default_Iterator  => Iterate,
       Iterator_Element  => Element_Type;

   type Cursor is private;

   No_Element : constant Cursor;

   function Has_Element (Position : Cursor) return Boolean;

   subtype Extended_Index is
     Index_Type'Base range Index_Type'First - 1 .. Index_Type'Last;

   package Vector_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   Empty_Vector : constant Vector;

   function Last_Index
     (Container : Vector)
      return Extended_Index;

   function Element
     (Container : Vector;
      Reference : Index_Type)
      return Element_Type;

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is
   private
     with
       Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Vector;
      Reference : Index_Type)
      return Constant_Reference_Type;
   pragma Inline (Constant_Reference);

   procedure Append
     (Container : in out Vector;
      New_Item  : Element_Type;
      Index     : out Index_Type);

   procedure Update
     (Container : in out Vector;
      Index     : Index_Type;
      Proc      : not null access
        procedure (Element : in out Element_Type));

   function Iterate
     (Container : Vector)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   function Iterate
     (Container : Vector; Start : Cursor)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class;

   procedure Read
     (Container : in out Vector;
      Stream    : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Write
     (Container : in out Vector;
      Stream    : not null access Ada.Streams.Root_Stream_Type'Class);

private

   type Constant_Reference_Type
     (Element : not null access constant Element_Type) is
      record
         null;
      end record;

   type Element_Access is access all Element_Type;

   package Element_Vectors is
     new Ada.Containers.Vectors (Index_Type, Element_Access);

   protected type Synchronized_Vector is

      function Last_Index return Extended_Index;

      function Element
        (Index : Index_Type)
         return Element_Type;

      function Constant_Reference
        (Index : Index_Type)
         return Constant_Reference_Type;

      procedure Append (New_Item : Element_Type;
                        Index    : out Index_Type);

      procedure Update
        (Index     : Index_Type;
         Proc      : not null access
           procedure (Element : in out Element_Type));

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class);
      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   private

      Vector : Element_Vectors.Vector;

   end Synchronized_Vector;

   type Vector is tagged limited
      record
         Internal : Synchronized_Vector;
      end record;

   type Cursor is
      record
         Index : Extended_Index;
         Container : access constant Vector;
      end record;

   No_Element : constant Cursor := (Extended_Index'First, null);

   Empty_Vector : constant Vector :=
                    (Internal => <>);

end Carthage.Handles.Vectors;
