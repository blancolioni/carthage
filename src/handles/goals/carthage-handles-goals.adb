with Carthage.Handles.Vectors;

package body Carthage.Handles.Goals is

   type Goal_Queue_Record is
      record
         Name  : Ada.Strings.Unbounded.Unbounded_String;
         Queue : Carthage.Goals.Goal_Queue;
      end record;

   package Goal_Queue_Vectors is
     new Carthage.Handles.Vectors
       (Real_Goal_Queue_Reference, Goal_Queue_Record, "goal");

   Goal_Queue_Vector : Goal_Queue_Vectors.Vector;

   function Get
     (Handle : Goal_Queue_Handle)
      return Goal_Queue_Vectors.Constant_Reference_Type
   is (Goal_Queue_Vector (Handle.Reference));

   overriding function Short_Name
     (Handle : Goal_Queue_Handle)
      return String
   is (-Get (Handle).Name);

   function Is_Empty (Queue : Goal_Queue_Handle) return Boolean
   is (Get (Queue).Queue.Is_Empty);

   function First_Goal
     (Queue : Goal_Queue_Handle)
      return Carthage.Goals.Goal_Record'Class
   is (Get (Queue).Queue.First_Goal);

   -----------------------
   -- Create_Goal_Queue --
   -----------------------

   function Create_Goal_Queue
     (Name : String)
      return Goal_Queue_Reference
   is
      Reference : Goal_Queue_Reference;
      Rec       : constant Goal_Queue_Record := Goal_Queue_Record'
        (Name  => +Name,
         Queue => <>);
   begin
      Goal_Queue_Vector.Append (Rec, Reference);
      return Reference;
   end Create_Goal_Queue;

   ------------------
   -- Delete_First --
   ------------------

   procedure Delete_First
     (Queue : Goal_Queue_Handle)
   is
      procedure Update (Rec : in out Goal_Queue_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Goal_Queue_Record) is
      begin
         Rec.Queue.Delete_First;
      end Update;

   begin
      Goal_Queue_Vector.Update (Queue.Reference, Update'Access);
   end Delete_First;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Queue : Goal_Queue_Handle;
      Goal  : Carthage.Goals.Goal_Record'Class)
   is

      procedure Update (Rec : in out Goal_Queue_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Goal_Queue_Record) is
      begin
         Rec.Queue.Insert (Goal);
      end Update;

   begin
      Goal_Queue_Vector.Update (Queue.Reference, Update'Access);
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Queue   : Goal_Queue_Handle;
      Process : not null access
        procedure (Goal : Carthage.Goals.Goal_Record'Class))
   is

      procedure Update (Rec : in out Goal_Queue_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Goal_Queue_Record) is
      begin
         Rec.Queue.Iterate (Process);
      end Update;

   begin
      Goal_Queue_Vector.Update (Queue.Reference, Update'Access);
   end Iterate;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Goal_Queue_Vector.Read (Stream);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Goal_Queue_Vector.Write (Stream);
   end Save;

   ------------
   -- Update --
   ------------

   procedure Update
     (Queue   : Goal_Queue_Handle;
      Process : not null access
        procedure (Goal     : in out Carthage.Goals.Goal_Record'Class;
                   Complete : out Boolean))
   is

      procedure Update (Rec : in out Goal_Queue_Record);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Goal_Queue_Record) is
      begin
         Rec.Queue.Update (Process);
      end Update;

   begin
      Goal_Queue_Vector.Update (Queue.Reference, Update'Access);
   end Update;

end Carthage.Handles.Goals;
