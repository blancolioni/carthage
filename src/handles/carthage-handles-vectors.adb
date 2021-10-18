with Carthage.Logging;

package body Carthage.Handles.Vectors is

   type Vector_Iterator is
     new Vector_Iterator_Interfaces.Reversible_Iterator with
      record
         Container : access constant Vector;
         Current   : Cursor;
      end record;

   overriding function First
     (Object : Vector_Iterator)
      return Cursor
   is (if Object.Container.Last_Index = Extended_Index'First
       then No_Element
       else (Index_Type'First, Object.Container));

   overriding function Next
     (Object   : Vector_Iterator;
      Position : Cursor)
      return Cursor
   is (if Position.Index < Object.Container.Last_Index - 1
       then (Position.Index + 1, Position.Container)
       else No_Element);

   overriding function Last
     (Object : Vector_Iterator)
      return Cursor
   is (if Object.Container.Last_Index = Extended_Index'First
       then No_Element
       else (Object.Container.Last_Index, Object.Container));

   overriding function Previous
     (Object   : Vector_Iterator;
      Position : Cursor)
      return Cursor
   is (if Position.Index > Index_Type'First
       then (Position.Index - 1, Position.Container)
       else No_Element);

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Vector;
                     New_Item  : Element_Type;
                     Index     : out Index_Type) is
   begin
      Container.Internal.Append (New_Item, Index);
   end Append;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Vector; Reference : Index_Type)
      return Constant_Reference_Type
   is
   begin
      return (Element =>
                Container.Internal.Constant_Reference (Reference).Element);
   end Constant_Reference;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Vector; Reference : Index_Type) return Element_Type
   is
   begin
      return Container.Internal.Element (Reference);
   end Element;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (Position : Cursor) return Boolean is
   begin
      return Position /= No_Element;
   end Has_Element;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Container : Vector)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is
   begin
      return Vector_Iterator'
        (Current => Cursor'(Index     => Index_Type'First,
                            Container => Container'Unchecked_Access),
         Container => Container'Unchecked_Access);
   end Iterate;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Container : Vector;
      Start     : Cursor)
      return Vector_Iterator_Interfaces.Reversible_Iterator'Class
   is
   begin
      return Vector_Iterator'
        (Current   => Start,
         Container => Container'Unchecked_Access);
   end Iterate;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index (Container : Vector) return Extended_Index is
   begin
      return Container.Internal.Last_Index;
   end Last_Index;

   ----------
   -- Read --
   ----------

   procedure Read
     (Container : in out Vector;
      Stream    : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Container.Internal.Read (Stream);
   end Read;

   ------------
   -- Update --
   ------------

   procedure Update
     (Container : in out Vector;
      Index     : Index_Type;
      Proc      : not null access
        procedure (Element : in out Element_Type))
   is
   begin
      if False then
         Carthage.Logging.Log
           (Contents_Name & ": begin update" & Index'Image);
      end if;
      Container.Internal.Update (Index, Proc);
      if False then
         Carthage.Logging.Log
           (Contents_Name & ": end update" & Index'Image);
      end if;
   end Update;

   -----------
   -- Write --
   -----------

   procedure Write
     (Container : in out Vector;
      Stream    : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Container.Internal.Write (Stream);
   end Write;

   -------------------------
   -- Synchronized_Vector --
   -------------------------

   protected body Synchronized_Vector is

      ------------
      -- Append --
      ------------

      procedure Append (New_Item : Element_Type;
                        Index    : out Index_Type)
      is
         New_Element : constant Element_Access := new Element_Type'(New_Item);
      begin
         Vector.Append (New_Element);
         Index := Vector.Last_Index;
      end Append;

      ------------------------
      -- Constant_Reference --
      ------------------------

      function Constant_Reference
        (Index : Index_Type)
         return Constant_Reference_Type
      is
      begin
         return (Element => Vector.Element (Index));
      end Constant_Reference;

      -------------
      -- Element --
      -------------

      function Element
        (Index : Index_Type)
         return Element_Type
      is
      begin
         return Vector.Element (Index).all;
      end Element;

      ----------------
      -- Last_Index --
      ----------------

      function Last_Index return Extended_Index is
      begin
         return Vector.Last_Index;
      end Last_Index;

      ----------
      -- Read --
      ----------

      procedure Read
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      is
         Last_Index : Extended_Index;
      begin
         Extended_Index'Read (Stream, Last_Index);
         for I in 1 .. Last_Index loop
            declare
               Element : constant Element_Type := Element_Type'Input (Stream);
            begin
               Vector.Append (new Element_Type'(Element));
            end;
         end loop;
      end Read;

      ------------
      -- Update --
      ------------

      procedure Update
        (Index     : Index_Type;
         Proc      : not null access
           procedure (Element : in out Element_Type))
      is
         Element : Element_Type renames Vector (Index).all;
      begin
         Proc (Element);
      end Update;

      -----------
      -- Write --
      -----------

      procedure Write
        (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
      is
      begin
         Extended_Index'Write (Stream, Vector.Last_Index);
         for Element of Vector loop
            Element_Type'Output (Stream, Element.all);
         end loop;
      end Write;

   end Synchronized_Vector;

end Carthage.Handles.Vectors;
