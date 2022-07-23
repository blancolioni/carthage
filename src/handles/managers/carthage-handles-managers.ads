private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;

with Ada.Streams;

with Carthage.Calendar;
with Carthage.Goals;
with Carthage.Quantities;

with Carthage.Handles.Houses;
with Carthage.Handles.Resources;
with Carthage.Handles.Stocks;

package Carthage.Handles.Managers is

   type Manager_Class is (Ground, Space, House);

   type Manager_Authority is
     (General, Budget,
      Planet_Management,
      Transport_Management,
      Asset_Management,
      Resource_Management,
      Production_Management);

   Manager_Version : constant Object_Version := (0, 1, 0);

   type Manager_Handle is
     new Root_Carthage_Handle
     and Dynamic_Object_Interface
     and Carthage.Handles.Stocks.Stock_Handle_Interface
     and Carthage.Handles.Stocks.Has_Stock_Handle_Interface
   with private;

   overriding function Get_Stock_Handle
     (This : Manager_Handle)
      return Carthage.Handles.Stocks.Stock_Handle'Class;

   overriding function Quantity
     (This     : Manager_Handle;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type;

   overriding procedure Set_Quantity
     (This     : Manager_Handle;
      Item     : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Carthage.Quantities.Quantity_Type);

   function Reference
     (Handle : Manager_Handle)
      return Manager_Reference;

   function Get (Reference : Manager_Reference) return Manager_Handle;

   function Empty_Handle return Manager_Handle;

   function Is_Active
     (Handle : Manager_Handle)
      return Boolean;

   function Is_Scheduled
     (Handle : Manager_Handle)
      return Boolean;

   function Class
     (Handle : Manager_Handle)
      return Manager_Class;

   function Next_Update_At
     (Handle : Manager_Handle)
      return Carthage.Calendar.Time
     with Pre => Handle.Is_Scheduled;

   procedure Start (Handle : Manager_Handle);

   procedure Activate
     (Handle : Manager_Handle);

   procedure Add_Goal
     (Handle : Manager_Handle;
      Goal   : Carthage.Goals.Goal_Record'Class);

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   overriding function Short_Name
     (This : Manager_Handle)
      return String;

   type Root_Manager_Record is
     abstract new Carthage.Handles.Resources.Stock_Interface with private;

   function Name (Manager : Root_Manager_Record) return String is abstract;

   procedure Register (Manager : Root_Manager_Record) is abstract;

   procedure Save_Manager
     (Manager : Root_Manager_Record;
      Planet  : Planet_Reference := Null_Planet_Reference;
      City    : City_Reference := Null_City_Reference);

   procedure Initialize
     (Rec         : in out Root_Manager_Record'Class;
      Class       : Manager_Class;
      Authority   : Manager_Authority;
      House       : Carthage.Handles.Houses.House_Handle);

   procedure Initialize
     (Rec         : in out Root_Manager_Record'Class;
      Class       : Manager_Class;
      Authority   : Manager_Authority;
      House       : Carthage.Handles.Houses.House_Handle;
      First_Event : Carthage.Calendar.Time);

   procedure Initialize
     (Rec          : in out Root_Manager_Record'Class;
      Class        : Manager_Class;
      Authority    : Manager_Authority;
      House        : Carthage.Handles.Houses.House_Handle;
      First_Event  : Duration;
      Random_Start : Boolean);
   --  Initialize manager record and schedule the first event
   --  at a time between 0 and First_Event in the future

   procedure On_Activated
     (Rec : in out Root_Manager_Record)
   is abstract;

   procedure Schedule_Next_Update
     (Rec  : in out Root_Manager_Record'Class;
      Wait : Duration);

   procedure Schedule_Next_Update_At
     (Rec    : in out Root_Manager_Record'Class;
      Clock  : Carthage.Calendar.Time);

   procedure Add_Pending_Goal
     (To_Manager : Manager_Handle'Class;
      Goal       : Carthage.Goals.Goal_Record'Class);
   --  Goal will be added to To_Manager after the current
   --  activation completes.

   procedure Information
     (Rec     : Root_Manager_Record'Class;
      Message : String);

   procedure Log
     (Rec     : Root_Manager_Record'Class;
      Message : String);

   function House
     (Manager : Root_Manager_Record)
      return Carthage.Handles.Houses.House_Handle;

   function Create_Manager
     (Rec : Root_Manager_Record'Class)
      return Manager_Handle;

   procedure Scan_Goals
     (Rec     : Root_Manager_Record'Class;
      Test    : not null access
        function (Goal : Carthage.Goals.Goal_Record'Class) return Boolean;
      Process : not null access
        procedure (Goal : Carthage.Goals.Goal_Record'Class));

   procedure Update_Goals
     (Rec     : in out Root_Manager_Record'Class;
      Process : not null access
        procedure (Goal : in out Carthage.Goals.Goal_Record'Class;
                   Complete : out Boolean));

   procedure Update_Goals
     (Rec     : in out Root_Manager_Record'Class;
      Test    : not null access
        function (Goal : Carthage.Goals.Goal_Record'Class) return Boolean;
      Process : not null access
        procedure (Goal : in out Carthage.Goals.Goal_Record'Class;
                   Complete : out Boolean));

   function Get_Manager
     (Class       : Manager_Class;
      Authority   : Manager_Authority;
      House       : House_Reference;
      Planet      : Planet_Reference := Null_Planet_Reference;
      City        : City_Reference := Null_City_Reference)
      return Manager_Handle;

   procedure For_All_Active_Managers
     (Process : not null access
        procedure (Handle : Manager_Handle));

private

   package Goal_Holders is
     new Ada.Containers.Indefinite_Holders
       (Carthage.Goals.Goal_Record'Class,
        Carthage.Goals."=");

   type Pending_Goal is
      record
         Manager : Manager_Reference;
         Goal    : Goal_Holders.Holder;
      end record;

   package Pending_Goal_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Pending_Goal);

   type Manager_Handle is
     new Root_Carthage_Handle
     and Dynamic_Object_Interface
     and Carthage.Handles.Stocks.Stock_Handle_Interface
     and Carthage.Handles.Stocks.Has_Stock_Handle_Interface with
      record
         Reference : Manager_Reference := 0;
      end record;

   overriding function Identifier
     (Handle : Manager_Handle)
      return Object_Identifier;

   type Root_Manager_Record is
     abstract new Carthage.Handles.Resources.Stock_Interface with
      record
         Identifier     : Object_Identifier;
         Reference      : Manager_Reference;
         Class          : Manager_Class;
         Authority      : Manager_Authority;
         Active         : Boolean;
         Scheduled      : Boolean;
         House          : House_Reference;
         First_Update   : Boolean := True;
         Next_Update_At : Carthage.Calendar.Time;
         Goals          : Goal_Queue_Reference;
         Stock          : Stock_Reference;
      end record;

   overriding function Quantity
     (This     : Root_Manager_Record;
      Resource : Carthage.Handles.Resources.Resource_Handle'Class)
      return Carthage.Quantities.Quantity_Type
   is (Carthage.Handles.Stocks.Get (This.Stock).Quantity (Resource));

   overriding procedure Set_Quantity
     (This         : in out Root_Manager_Record;
      Resource     : Carthage.Handles.Resources.Resource_Handle'Class;
      New_Quantity : Carthage.Quantities.Quantity_Type);

   function House
     (Manager : Root_Manager_Record)
      return Carthage.Handles.Houses.House_Handle
   is (Carthage.Handles.Houses.Get (Manager.House));

end Carthage.Handles.Managers;
