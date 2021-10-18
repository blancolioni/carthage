private with WL.Indefinite_Heaps;
private with WL.String_Maps;
private with Reiko;

with Carthage.Goals;

with Carthage.Handles.Cities;
with Carthage.Handles.Resources;
with Carthage.Handles.Houses;
with Carthage.Handles.Stacks;

package Carthage.Managers is

   type Stack_Meta_Manager_Access is
     access all Carthage.Handles.Stacks.Asset_Meta_Manager_Interface'Class;

   type Root_Manager_Type is
     abstract new Carthage.Handles.Stacks.Asset_Meta_Manager_Interface
   with private;

   type Manager_Type is access all Root_Manager_Type'Class;

   function Name
     (Manager : Root_Manager_Type)
      return String
      is abstract;

   function Target_Id
     (Manager : Root_Manager_Type)
      return String
      is abstract;

   function Description
     (Manager : Root_Manager_Type)
      return String
   is ("manager");

   function House
     (Manager : Root_Manager_Type)
      return Carthage.Handles.Houses.House_Handle;

   function Log_Identifier
     (Manager : Root_Manager_Type)
      return String
   is (Root_Manager_Type'Class (Manager).Name);

   procedure Log
     (Manager : Root_Manager_Type'Class;
      Message : String);

   procedure Initialize
     (Manager : not null access Root_Manager_Type)
   is null;

   function Have_Immediate_Capacity
     (Manager : Root_Manager_Type;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;

   type Resource_Stock is tagged private;

   procedure Set_Quantity
     (To       : in out Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Resource_Quantity);

   procedure Add
     (To       : in out Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Natural);

   procedure Add
     (To       : in out Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Resource_Quantity);

   procedure Remove
     (To       : in out Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Natural);

   procedure Remove
     (To       : in out Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : Resource_Quantity);

   procedure Clear
     (Stock : in out Resource_Stock'Class);

   function Quantity
     (Stock : Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Resource_Quantity;

   function Whole_Quantity
     (Stock    : Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Natural;

   procedure Scan_Stock
     (Stock : Resource_Stock;
      Process : not null access
        procedure (Resource : Carthage.Handles.Resources.Resource_Handle;
                   Quantity : Positive));

   procedure Get_Resource_Requirements
     (Manager : in out Root_Manager_Type;
      Minimum : in out Resource_Stock'Class;
      Desired : in out Resource_Stock'Class);

   procedure Transfer_Resources
     (Manager : in out Root_Manager_Type;
      From    : in out Carthage.Handles.Resources.Stock_Interface'Class;
      Max     : Resource_Stock'Class);

   procedure Transfer_Resources
     (Manager : in out Root_Manager_Type;
      From    : Carthage.Handles.Cities.City_Handle;
      Max     : Resource_Stock'Class);

   function Check_Goal
     (Manager : not null access Root_Manager_Type;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean;
   --  Return True if goal has been processed and can be removed
   --  False if goal has not been processed and checking should stop

   procedure Add_Goal
     (Manager : in out Root_Manager_Type'Class;
      Goal    : Carthage.Goals.Goal_Record'Class);

   procedure Check_Goals
     (Manager : not null access Root_Manager_Type'Class);

   function Average_Update_Frequency
     (Manager : Root_Manager_Type)
      return Duration
      is abstract;

   function Update
     (Manager : not null access Root_Manager_Type)
      return Duration
      is abstract;

   function Get_Manager
     (Name : String;
      Id   : String)
      return Manager_Type;

private

   package Resource_Maps is
     new WL.String_Maps (Resource_Quantity);

   type Resource_Stock is tagged
      record
         Map : Resource_Maps.Map;
      end record;

   function Quantity
     (Stock    : Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Resource_Quantity
   is (if Stock.Map.Contains (Resource.Tag)
       then Stock.Map.Element (Resource.Tag)
       else 0.0);

   function Whole_Quantity
     (Stock    : Resource_Stock'Class;
      Resource : Carthage.Handles.Resources.Resource_Handle)
      return Natural
   is (if Stock.Map.Contains (Resource.Tag)
       then Natural (Stock.Map.Element (Resource.Tag))
       else 0);

   package Goal_Queues is
     new WL.Indefinite_Heaps
       (Key_Type     => Carthage.Goals.Goal_Priority,
        Element_Type => Carthage.Goals.Goal_Record'Class,
        "<"          => Carthage.Goals."<",
        "="          => Carthage.Goals."=");

   type Root_Manager_Type is
     abstract new Carthage.Handles.Stacks.Asset_Meta_Manager_Interface with
      record
         House     : Carthage.Handles.Houses.House_Handle;
         Goals     : Goal_Queues.Heap;
         Resources : Resource_Stock;
      end record;

   function House
     (Manager : Root_Manager_Type)
      return Carthage.Handles.Houses.House_Handle
   is (Manager.House);

   function Check_Goal
     (Manager : not null access Root_Manager_Type;
      Goal    : Carthage.Goals.Goal_Record'Class)
      return Boolean
   is (True);

   type Manager_Update is
     new Reiko.Root_Update_Type with
      record
         Manager : Manager_Type;
      end record;

   overriding function Name
     (Update : Manager_Update)
      return String
   is (Update.Manager.Description);

   overriding procedure Execute
     (Update : Manager_Update);

   procedure Add_Manager
     (Manager : not null access Root_Manager_Type'Class);

end Carthage.Managers;
