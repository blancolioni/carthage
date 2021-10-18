with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with WL.String_Maps;

with Reiko.Updates;

with Carthage.Handles.Goals;
with Carthage.Handles.Vectors;

with Carthage.Logging;
with Carthage.Random;

package body Carthage.Handles.Managers is

   Log_Goals : constant Boolean := False;

   package Manager_Maps is
     new WL.String_Maps (Manager_Reference);

   Manager_Map : Manager_Maps.Map;

   function To_Key
     (Class  : Manager_Class;
      House  : House_Reference;
      Planet : Planet_Reference;
      City   : City_Reference)
      return String;

   type Manager_Access is access all Root_Manager_Record'Class;

   package Manager_Vectors is
     new Carthage.Handles.Vectors
       (Real_Manager_Reference, Manager_Access, "manager");

   Manager_Vector : Manager_Vectors.Vector;

   function Get
     (Handle : Manager_Handle)
      return Manager_Access
   is (Manager_Vector.Element (Handle.Reference));

   overriding function Identifier
     (Handle : Manager_Handle)
      return Object_Identifier
   is (Get (Handle).Identifier);

   overriding function Short_Name
     (This : Manager_Handle)
      return String
   is (Get (This).Name);

   overriding function Quantity
     (This     : Manager_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type
   is (Carthage.Handles.Stocks.Get (Get (This).Stock).Quantity (Resource));

   overriding function Get_Stock_Handle
     (This : Manager_Handle)
      return Carthage.Handles.Stocks.Stock_Handle'Class
   is (Carthage.Handles.Stocks.Get (Get (This).Stock));

   function Reference
     (Handle : Manager_Handle)
      return Manager_Reference
   is (Handle.Reference);

   function Get (Reference : Manager_Reference) return Manager_Handle
   is (Manager_Handle'(Reference /= Null_Manager_Reference, Reference));

   function Empty_Handle return Manager_Handle
   is (Manager_Handle'(False, Null_Manager_Reference));

   function Class
     (Handle : Manager_Handle)
      return Manager_Class
   is (Get (Handle).Class);

   function Is_Active
     (Handle : Manager_Handle)
      return Boolean
   is (Get (Handle).Active);

   function Is_Scheduled
     (Handle : Manager_Handle)
      return Boolean
   is (Get (Handle).Scheduled);

   function Next_Update_At
     (Handle : Manager_Handle)
      return Carthage.Calendar.Time
   is (Get (Handle).Next_Update_At);

   type Manager_Update is
     new Reiko.Root_Update_Type with
      record
         Handle : Manager_Handle;
      end record;

   overriding function Name
     (Update : Manager_Update)
      return String
   is (Update.Handle.Short_Name);

   overriding procedure Execute
     (Update : Manager_Update);

   function To_Update
     (Reference : Manager_Reference)
      return Manager_Update'Class
   is (Manager_Update'
         (Reiko.Root_Update_Type with
            Handle => Get (Reference)));

   protected Pending_Goal_List is

      procedure Add (Manager : Manager_Reference;
                     Goal    : Carthage.Goals.Goal_Record'Class);

      procedure Get (List : out Pending_Goal_Lists.List);

   private
      Goals : Pending_Goal_Lists.List;
   end Pending_Goal_List;

   protected body Pending_Goal_List is

      ---------
      -- Add --
      ---------

      procedure Add (Manager : Manager_Reference;
                     Goal    : Carthage.Goals.Goal_Record'Class)
      is
      begin
         Goals.Append (Pending_Goal'
                         (Manager => Manager,
                          Goal    => Goal_Holders.To_Holder (Goal)));
      end Add;

      ---------
      -- Get --
      ---------

      procedure Get (List : out Pending_Goal_Lists.List) is
      begin
         List := Goals;
         Goals.Clear;
      end Get;

   end Pending_Goal_List;

   --------------
   -- Activate --
   --------------

   procedure Activate (Handle : Manager_Handle) is
      Manager : constant Manager_Access := Get (Handle);
   begin
      Manager.Scheduled := False;
      Manager.On_Activated;
      Manager.First_Update := False;

      declare
         Pending_Goals  : Pending_Goal_Lists.List;
      begin
         Pending_Goal_List.Get (Pending_Goals);

         for Pending_Goal of Pending_Goals loop
            Get (Pending_Goal.Manager).Add_Goal
              (Pending_Goal.Goal.Constant_Reference);
         end loop;
      end;

   end Activate;

   --------------
   -- Add_Goal --
   --------------

   procedure Add_Goal
     (Handle : Manager_Handle;
      Goal   : Carthage.Goals.Goal_Record'Class)
   is
      procedure Update (Rec : in out Manager_Access);

      ------------
      -- Update --
      ------------

      procedure Update (Rec : in out Manager_Access) is
      begin
         Carthage.Handles.Goals.Get (Rec.Goals).Insert (Goal);
         if not Rec.Scheduled then
            Rec.Schedule_Next_Update (60.0);
            if Log_Goals then
               Rec.Log ("goal triggers update at "
                        & Carthage.Calendar.Image (Rec.Next_Update_At, True));
            end if;
         end if;
      end Update;

   begin

      if Log_Goals then
         Handle.Log ("new goal: " & Goal.Show);
      end if;

      Manager_Vector.Update (Handle.Reference, Update'Access);
   end Add_Goal;

   ----------------------
   -- Add_Pending_Goal --
   ----------------------

   procedure Add_Pending_Goal
     (To_Manager : Manager_Handle'Class;
      Goal       : Carthage.Goals.Goal_Record'Class)
   is
   begin
      Pending_Goal_List.Add (To_Manager.Reference, Goal);
   end Add_Pending_Goal;

   --------------------
   -- Create_Manager --
   --------------------

   function Create_Manager
     (Rec : Root_Manager_Record'Class) return Manager_Handle
   is
      Manager : constant Manager_Access := new Root_Manager_Record'Class'(Rec);
      Reference : Manager_Reference;
   begin
      Manager_Vector.Append (Manager, Reference);
      Manager.Reference := Reference;
      return Get (Reference);
   end Create_Manager;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Update : Manager_Update)
   is
   begin
      Update.Handle.Activate;
   end Execute;

   -----------------------------
   -- For_All_Active_Managers --
   -----------------------------

   procedure For_All_Active_Managers
     (Process : not null access
        procedure (Handle : Manager_Handle))
   is
   begin
      for Reference in 1 .. Manager_Vector.Last_Index loop
         declare
            Handle : constant Manager_Handle :=
                       Get (Reference);
         begin
            if Handle.Is_Active then
               Process (Handle);
            end if;
         end;
      end loop;
   end For_All_Active_Managers;

   -----------------
   -- Get_Manager --
   -----------------

   function Get_Manager
     (Class  : Manager_Class;
      House  : House_Reference;
      Planet : Planet_Reference := Null_Planet_Reference;
      City   : City_Reference := Null_City_Reference)
      return Manager_Handle
   is
      Key : constant String :=
              To_Key (Class  => Class,
                      House  => House,
                      Planet => Planet,
                      City   => City);
   begin
      if Manager_Map.Contains (Key) then
         return Get (Manager_Map.Element (Key));
      else
         return Empty_Handle;
      end if;
   end Get_Manager;

   ------------------------------
   -- Ground_Transport_Manager --
   ------------------------------

   function Ground_Transport_Manager
     (House  : House_Reference;
      Planet : Planet_Reference)
      return Manager_Handle
   is
      Key : constant String :=
              To_Key (Class  => Ground_Transport,
                      House  => House,
                      Planet => Planet,
                      City   => Null_City_Reference);
   begin
      if Manager_Map.Contains (Key) then
         return Get (Manager_Map.Element (Key));
      else
         return Empty_Handle;
      end if;
   end Ground_Transport_Manager;

   -----------------
   -- Information --
   -----------------

   procedure Information
     (Rec     : Root_Manager_Record'Class;
      Message : String)
   is
   begin
      Carthage.Logging.Log
        (Level   => Carthage.Logging.Log_Level'Last,
         Message => Rec.Name & ": " & Message);
   end Information;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Rec   : in out Root_Manager_Record'Class;
      Class : Manager_Class;
      House : Carthage.Handles.Houses.House_Handle)
   is
   begin
      Rec.Identifier := Next_Identifier;
      Rec.Reference := Null_Manager_Reference;
      Rec.Class := Class;
      Rec.Active    := True;
      Rec.Scheduled := False;
      Rec.House     := House.Reference;
      Rec.Next_Update_At := Carthage.Calendar.Clock;
      Rec.First_Update   := True;
      Rec.Goals          :=
        Carthage.Handles.Goals.Create_Goal_Queue
          ("goals");
      Rec.Stock :=
        Carthage.Handles.Stocks.Create_Stock;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Rec         : in out Root_Manager_Record'Class;
      Class       : Manager_Class;
      House       : Carthage.Handles.Houses.House_Handle;
      First_Event : Carthage.Calendar.Time)
   is
   begin
      Rec.Initialize (Class, House);
      Rec.Next_Update_At := First_Event;
      Rec.Scheduled := True;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Rec          : in out Root_Manager_Record'Class;
      Class        : Manager_Class;
      House        : Carthage.Handles.Houses.House_Handle;
      First_Event  : Duration;
      Random_Start : Boolean)
   is
      use type Carthage.Calendar.Time;
      Start : constant Carthage.Calendar.Time :=
                Carthage.Calendar.Clock
                  + (if Random_Start
                     then Duration
                       (Real (First_Event)
                        * Carthage.Random.Unit_Random)
                     else First_Event);
   begin
      Rec.Initialize (Class, House, Start);
   end Initialize;

   ----------
   -- Load --
   ----------

   procedure Load (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
      Last_Reference : Manager_Reference;
   begin
      Manager_Reference'Read (Stream, Last_Reference);

      for I in 1 .. Last_Reference loop
         declare
            Rec : constant Root_Manager_Record'Class :=
                    Root_Manager_Record'Class'Input (Stream);
            Ref : Manager_Reference;
         begin
            Manager_Vector.Append (new Root_Manager_Record'Class'(Rec), Ref);
            pragma Assert (Ref = I);
         end;
      end loop;
      Carthage.Logging.Log ("loaded" & Last_Reference'Image & " managers");

   end Load;

   ---------
   -- Log --
   ---------

   procedure Log
     (Rec     : Root_Manager_Record'Class;
      Message : String)
   is
   begin
      Carthage.Logging.Log (Rec.Name & ": " & Message);
   end Log;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Manager_Reference'Write (Stream, Manager_Vector.Last_Index);
      for Reference in 1 .. Manager_Vector.Last_Index loop
         Root_Manager_Record'Class'Output
           (Stream, Manager_Vector.Element (Reference).all);
      end loop;
   end Save;

   ------------------
   -- Save_Manager --
   ------------------

   procedure Save_Manager
     (Manager : Root_Manager_Record;
      Planet  : Planet_Reference := Null_Planet_Reference;
      City    : City_Reference := Null_City_Reference)
   is
      use Manager_Maps;
      Key      : constant String :=
                   To_Key (Class  => Manager.Class,
                           House  => Manager.House,
                           Planet => Planet,
                           City   => City);
      Position : constant Cursor := Manager_Map.Find (Key);
   begin
      if Has_Element (Position) then
         Manager.Log ("replacing existing manager for " & Key);
         Manager_Map.Replace_Element (Position, Manager.Reference);
      else
         Manager_Map.Insert (Key, Manager.Reference);
      end if;
   end Save_Manager;

   ----------------
   -- Scan_Goals --
   ----------------

   procedure Scan_Goals
     (Rec     : Root_Manager_Record'Class;
      Test    : not null access
        function (Goal : Carthage.Goals.Goal_Record'Class) return Boolean;
      Process : not null access
        procedure (Goal : Carthage.Goals.Goal_Record'Class))
   is
      procedure Check_Goal
        (Goal     : Carthage.Goals.Goal_Record'Class);

      ----------------
      -- Check_Goal --
      ----------------

      procedure Check_Goal
        (Goal     : Carthage.Goals.Goal_Record'Class)
      is
      begin
         if Test (Goal) then
            Process (Goal);
         end if;
      end Check_Goal;

   begin
      Carthage.Handles.Goals.Get (Rec.Goals).Iterate (Check_Goal'Access);
   end Scan_Goals;

   --------------------------
   -- Schedule_Next_Update --
   --------------------------

   procedure Schedule_Next_Update
     (Rec  : in out Root_Manager_Record'Class;
      Wait : Duration)
   is
      use type Carthage.Calendar.Time;
   begin
      Rec.Schedule_Next_Update_At
        (Carthage.Calendar.Clock + Wait);
   end Schedule_Next_Update;

   -----------------------------
   -- Schedule_Next_Update_At --
   -----------------------------

   procedure Schedule_Next_Update_At
     (Rec    : in out Root_Manager_Record'Class;
      Clock  : Carthage.Calendar.Time)
   is
   begin
      Rec.Scheduled := True;
      Rec.Next_Update_At := Clock;
      Reiko.Updates.Add_Update_At
        (Update    => To_Update (Rec.Reference),
         Update_At => Reiko.Reiko_Time (Carthage.Calendar.To_Days (Clock)));
   end Schedule_Next_Update_At;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (This     : Manager_Handle;
      Item     : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type)
   is
   begin
      Get (This).Set_Quantity (Item, Quantity);
   end Set_Quantity;

   ------------------
   -- Set_Quantity --
   ------------------

   overriding procedure Set_Quantity
     (This         : in out Root_Manager_Record;
      Resource     : Carthage.Handles.Resources.Resource_Handle'Class;
      New_Quantity : Carthage.Quantities.Quantity_Type)
   is
   begin
      Carthage.Handles.Stocks.Get (This.Stock)
        .Set_Quantity (Carthage.Handles.Resources.Resource_Handle (Resource),
                       New_Quantity);
   end Set_Quantity;

   -----------
   -- Start --
   -----------

   procedure Start (Handle : Manager_Handle) is
      Rec : constant Manager_Access := Get (Handle);
   begin
      Rec.Register;
      if Rec.Scheduled then
         Rec.Schedule_Next_Update_At (Rec.Next_Update_At);
      end if;
   end Start;

   ------------
   -- To_Key --
   ------------

   function To_Key
     (Class  : Manager_Class;
      House  : House_Reference;
      Planet : Planet_Reference;
      City   : City_Reference)
      return String
   is
      function Trim (X : String) return String
      is (Ada.Strings.Fixed.Trim (X, Ada.Strings.Left));
   begin
      return Ada.Characters.Handling.To_Lower (Class'Image)
        & "-" & Trim (House'Image)
        & "-" & Trim (Planet'Image)
        & "-" & Trim (City'Image);
   end To_Key;

   ------------------
   -- Update_Goals --
   ------------------

   procedure Update_Goals
     (Rec     : in out Root_Manager_Record'Class;
      Test    : not null access
        function (Goal : Carthage.Goals.Goal_Record'Class) return Boolean;
      Process : not null access
        procedure (Goal : in out Carthage.Goals.Goal_Record'Class;
                   Complete : out Boolean))
   is
      procedure Check_Goal
        (Goal : in out Carthage.Goals.Goal_Record'Class;
         Complete : out Boolean);

      ----------------
      -- Check_Goal --
      ----------------

      procedure Check_Goal
        (Goal     : in out Carthage.Goals.Goal_Record'Class;
         Complete : out Boolean)
      is
      begin
         if Test (Goal) then
            Process (Goal, Complete);
         else
            Complete := False;
         end if;
      end Check_Goal;

   begin
      Carthage.Handles.Goals.Get (Rec.Goals).Update (Check_Goal'Access);
   end Update_Goals;

end Carthage.Handles.Managers;
