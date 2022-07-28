with Carthage.Handles.Vectors;

package body Carthage.Handles.Stocks is

   type Stock_Record is
      record
         Stock : Carthage.Handles.Resources.Resource_Stock;
      end record;

   package Stock_Vectors is
     new Carthage.Handles.Vectors
       (Real_Stock_Reference, Stock_Record, "stock");

   Stock_Vector : Stock_Vectors.Vector;

   function Get
     (Handle : Stock_Handle)
      return Stock_Vectors.Constant_Reference_Type
   is (Stock_Vector (Handle.Reference));

   overriding function Quantity
     (Stock    : Stock_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type
   is (Get (Stock).Stock.Quantity (Resource));

   ---------
   -- Add --
   ---------

   overriding procedure Add
     (Stock          : Stock_Handle;
      Resource       : Carthage.Handles.Resources.Resource_Handle'Class;
      Added_Quantity : Carthage.Quantities.Quantity_Type)
   is
      procedure Update (Rec : in out Stock_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stock_Record) is
      begin
         Rec.Stock.Add (Resource, Added_Quantity);
      end Update;

   begin
      Stock_Vector.Update (Stock.Reference, Update'Access);
   end Add;

   ------------------
   -- Create_Stock --
   ------------------

   function Create_Stock
     return Stock_Reference
   is
      Reference : Stock_Reference;
      Rec       : Stock_Record;
   begin
      Stock_Vector.Append (Rec, Reference);
      return Reference;
   end Create_Stock;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Stock_Vector.Read (Stream);
   end Load;

   ------------------
   -- Remove_Stock --
   ------------------

   procedure Remove_Stock
     (From  : Stock_Handle_Interface'Class;
      Stock : Carthage.Handles.Resources.Stock_Reader_Interface'Class)
   is
      procedure Go (Resource : Carthage.Handles.Resources.Resource_Handle;
                    Quantity : Carthage.Quantities.Quantity_Type);

      --------
      -- Go --
      --------

      procedure Go (Resource : Carthage.Handles.Resources.Resource_Handle;
                    Quantity : Carthage.Quantities.Quantity_Type)
      is
         Taken : Carthage.Quantities.Quantity_Type with Unreferenced;
      begin
         From.Take (Resource, Quantity, Taken);
      end Go;

   begin
      Stock.Scan_Stock (Go'Access);
   end Remove_Stock;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Stock_Vector.Write (Stream);
   end Save;

   ----------
   -- Take --
   ----------

   overriding procedure Take
     (Stock    : Stock_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class;
      Quantity : Carthage.Quantities.Quantity_Type;
      Received : out Carthage.Quantities.Quantity_Type)
   is

      procedure Update (Rec : in out Stock_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Stock_Record) is
      begin
         Rec.Stock.Take (Resource, Quantity, Received);
      end Update;

   begin
      Stock_Vector.Update (Stock.Reference, Update'Access);
   end Take;

end Carthage.Handles.Stocks;
