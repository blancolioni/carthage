with Ada.Containers.Vectors;

package body Carthage.Handles.Resources is

   type Resource_Record is
      record
         Tag        : Ada.Strings.Unbounded.Unbounded_String;
         Base_Price : Carthage.Money.Price_Type;
      end record;

   package Resource_Vectors is
     new Ada.Containers.Vectors (Real_Resource_Reference, Resource_Record);

   package Resource_Maps is
     new WL.String_Maps (Resource_Reference);

   Resource_Vector : Resource_Vectors.Vector;
   Resource_Map    : Resource_Maps.Map;

   function Get
     (Handle : Resource_Handle)
      return Resource_Vectors.Constant_Reference_Type
   is (Resource_Vector (Handle.Reference));

   overriding function Tag
     (Handle : Resource_Handle)
      return String
   is (-Get (Handle).Tag);

   function Base_Price
     (This : Resource_Handle)
      return Carthage.Money.Price_Type
   is (Get (This).Base_Price);

   function Exists (Tag : String) return Boolean
   is (Resource_Map.Contains (Tag));

   function Get (Tag : String) return Resource_Handle
   is (Get (Resource_Map (Tag)));

   ---------
   -- Add --
   ---------

   overriding procedure Add
     (Stock          : in out Resource_Stock;
      Resource       : Resource_Handle'Class;
      Added_Quantity : Carthage.Quantities.Quantity_Type)
   is
      use Carthage.Quantities;
      use Quantity_Maps;
      Position : constant Cursor := Stock.Map.Find (Resource.Tag);
   begin
      if Has_Element (Position) then
         declare
            Quantity : Quantity_Type renames Stock.Map (Position);
         begin
            Quantity := Quantity + Added_Quantity;
         end;
      else
         Stock.Map.Insert (Resource.Tag, Added_Quantity);
      end if;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (Stock          : in out Stock_Interface'Class;
      Resource       : Resource_Handle;
      Added_Quantity : Natural)
   is
   begin
      Stock.Add (Resource, Carthage.Quantities.To_Quantity (Added_Quantity));
   end Add;

   ---------------
   -- Add_Stock --
   ---------------

   procedure Add_Stock
     (To   : in out Stock_Interface'Class;
      From : Stock_Interface'Class)
   is

      procedure Move
        (Resource : Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type);

      ----------
      -- Move --
      ----------

      procedure Move
        (Resource : Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type)
      is
      begin
         To.Add (Resource, Quantity);
      end Move;

   begin
      From.Scan_Stock (Move'Access);
   end Add_Stock;

   -----------------
   -- Clear_Stock --
   -----------------

   procedure Clear_Stock
     (Stock    : in out Stock_Interface'Class)
   is
   begin
      for Reference in 1 .. Resource_Vector.Last_Index loop
         declare
            use type Carthage.Quantities.Quantity_Type;
            Quantity : constant Carthage.Quantities.Quantity_Type :=
                         Stock.Quantity (Get (Reference));
         begin
            if Quantity > Carthage.Quantities.Zero then
               declare
                  Taken : Carthage.Quantities.Quantity_Type with Unreferenced;
               begin
                  Stock.Take (Get (Reference), Quantity, Taken);
               end;
            end if;
         end;
      end loop;
   end Clear_Stock;

   ------------
   -- Create --
   ------------

   procedure Create
     (Tag        : String;
      Base_Price : Carthage.Money.Price_Type)
   is
   begin
      Resource_Vector.Append
        (Resource_Record'
           (Tag        => +Tag,
            Base_Price => Base_Price));
      Resource_Map.Insert (Tag, Resource_Vector.Last_Index);
   end Create;

   -----------------------
   -- For_All_Resources --
   -----------------------

   procedure For_All_Resources
     (Process : not null access
        procedure (Resource : Resource_Handle))
   is
   begin
      for Reference in 1 .. Resource_Vector.Last_Index loop
         Process (Get (Reference));
      end loop;
   end For_All_Resources;

   ------------------
   -- Get_By_Index --
   ------------------

   function Get_By_Index (Index : Natural) return Resource_Handle is
      Reference : constant Resource_Reference :=
                    (if Index < Natural (Resource_Reference'Last)
                     then Resource_Reference (Index)
                     else Null_Resource_Reference);
   begin
      if Reference /= Null_Resource_Reference then
         return Handle : constant Resource_Handle := Get (Reference) do
            null;
         end return;
      else
         return Empty_Handle;
      end if;
   end Get_By_Index;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (Stock : Stock_Reader_Interface'Class)
      return Boolean
   is
      use Carthage.Quantities;
   begin
      for Reference in 1 .. Resource_Vector.Last_Index loop
         if Stock.Quantity (Get (Reference)) > Zero then
            return False;
         end if;
      end loop;
      return True;
   end Is_Empty;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Resource_Vectors.Vector'Read (Stream, Resource_Vector);
      for Reference in 1 .. Resource_Vector.Last_Index loop
         Resource_Map.Insert (Get (Reference).Tag, Reference);
      end loop;
   end Load;

   --------------
   -- Quantity --
   --------------

   overriding function Quantity
     (Stock    : Resource_Stock;
      Resource : Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type
   is
      Position : constant Quantity_Maps.Cursor :=
                   Stock.Map.Find (Resource.Tag);
   begin
      if Quantity_Maps.Has_Element (Position) then
         return Quantity_Maps.Element (Position);
      else
         return Carthage.Quantities.Zero;
      end if;
   end Quantity;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (From  : in out Stock_Interface'Class;
      Stock : Stock_Interface'Class)
   is
      procedure Move
        (Resource : Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type);

      ----------
      -- Move --
      ----------

      procedure Move
        (Resource : Resource_Handle;
         Quantity : Carthage.Quantities.Quantity_Type)
      is
         Taken : Carthage.Quantities.Quantity_Type with Unreferenced;
      begin
         From.Take (Resource, Quantity, Taken);
      end Move;

   begin
      Stock.Scan_Stock (Move'Access);
   end Remove_Stock;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Resource_Vectors.Vector'Write (Stream, Resource_Vector);
   end Save;

   ----------------
   -- Scan_Stock --
   ----------------

   procedure Scan_Stock
     (Stock    : Stock_Reader_Interface'Class;
      Process  : not null access
        procedure (Resource : Resource_Handle;
                   Quantity : Carthage.Quantities.Quantity_Type))
   is
   begin
      for Reference in 1 .. Resource_Vector.Last_Index loop
         declare
            use Carthage.Quantities;
            Resource : constant Resource_Handle := Get (Reference);
            Quantity : constant Carthage.Quantities.Quantity_Type :=
                         Stock.Quantity (Resource);
         begin
            if Quantity > Zero then
               Process (Resource, Quantity);
            end if;
         end;
      end loop;
   end Scan_Stock;

   ----------
   -- Take --
   ----------

   overriding procedure Take
     (Stock    : in out Resource_Stock;
      Resource : Resource_Handle'Class;
      Quantity : Carthage.Quantities.Quantity_Type;
      Received : out Carthage.Quantities.Quantity_Type)
   is
      use Carthage.Quantities;
      use Quantity_Maps;
      Position : constant Cursor := Stock.Map.Find (Resource.Tag);
   begin
      if Has_Element (Position) then
         declare
            Available : Quantity_Type renames Stock.Map (Position);
         begin
            Received := Min (Quantity, Available);
            Available := Available - Received;
         end;
      else
         Received := Carthage.Quantities.Zero;
      end if;
   end Take;

end Carthage.Handles.Resources;
