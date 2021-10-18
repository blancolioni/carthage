private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;

private with WL.String_Maps;
private with WL.String_Sets;

private with Carthage.Calendar;
private with Carthage.Handles.Assets;

with Carthage.Handles.Cities;
with Carthage.Handles.Houses;
with Carthage.Handles.Planets;
with Carthage.Handles.Stacks;
with Carthage.Handles.Tiles;

package Carthage.Managers.Assets is

   function Ground_Asset_Manager
     (Meta_Manager : Stack_Meta_Manager_Access;
      House        : Carthage.Handles.Houses.House_Handle;
      Planet       : Carthage.Handles.Planets.Planet_Handle)
      return Manager_Type;

   function Space_Asset_Manager
     (Meta_Manager : Stack_Meta_Manager_Access;
      House        : Carthage.Handles.Houses.House_Handle)
      return Manager_Type;

   function Tile_Reconnaissance_Goal
     (Tile    : Carthage.Handles.Tiles.Tile_Handle)
      return Carthage.Goals.Goal_Record'Class;

   function Tile_Capture_Goal
     (Tile     : Carthage.Handles.Tiles.Tile_Handle;
      Strength : Natural)
      return Carthage.Goals.Goal_Record'Class;

   function Transfer_Cargo_Goal
     (From, To : Carthage.Handles.Cities.City_Handle;
      Cargo    : Resource_Stock'Class)
      return Carthage.Goals.Goal_Record'Class;

   function Planet_Reconnaissance_Goal
     (Planet : Carthage.Handles.Planets.Planet_Handle)
      return Carthage.Goals.Goal_Record'Class;

private

   type Goal_Class is (None, Recon, Capture, Transfer);

   type Goal_Parameter is (Speed, Spot, Military);
   type Relative_Value is (Low, Medium, High);

   function Default_Priority
     (Class : Goal_Class)
      return Carthage.Goals.Goal_Priority
   is (case Class is
          when None     => Carthage.Goals.Lowest_Priority,
          when Recon    => 20,
          when Capture  => 10,
          when Transfer => 25);

   type Goal_Parameter_Record is
      record
         Speed     : Relative_Value := Low;
         Spot      : Relative_Value := Low;
         Military  : Relative_Value := Low;
         Cargo     : Relative_Value := Low;
         Strength  : Natural        := 0;
      end record;

   type Asset_Manager_Goal is
     new Carthage.Goals.Goal_Record with
      record
         Class      : Goal_Class := None;
         Planet     : Carthage.Handles.Planets.Planet_Handle;
         Tile       : Carthage.Handles.Tiles.Tile_Handle;
         City_1     : Carthage.Handles.Cities.City_Handle;
         City_2     : Carthage.Handles.Cities.City_Handle;
         Stock      : Resource_Stock;
         Parameters : Goal_Parameter_Record;
      end record;

   overriding function Show
     (Goal : Asset_Manager_Goal)
      return String
   is ("assets: " & Goal.Class'Image & " "
       & (if Goal.Tile.Has_Element
         then Carthage.Handles.Tiles.Position_Image
           (Goal.Tile.Position)
          elsif Goal.Planet.Has_Element
          then Goal.Planet.Tag
          else ""));

   package Asset_Manager_Goal_Holders is
     new Ada.Containers.Indefinite_Holders (Asset_Manager_Goal);

   type Managed_Stack_Record is
      record
         Stack        : Carthage.Handles.Stacks.Stack_Handle;
         Goal         : Asset_Manager_Goal_Holders.Holder;
      end record;

   package Managed_Stack_List is
     new Ada.Containers.Doubly_Linked_Lists
       (Managed_Stack_Record);

   package Managed_Stack_Maps is
     new WL.String_Maps (Managed_Stack_List.Cursor, Managed_Stack_List."=");

   type Managed_Asset_Record is
      record
         Asset  : Carthage.Handles.Assets.Asset_Handle;
         Stack  : Managed_Stack_List.Cursor;
         Planet : Carthage.Handles.Planets.Planet_Handle;
         Tile   : Carthage.Handles.Tiles.Tile_Handle;
         Goal   : Asset_Manager_Goal_Holders.Holder;
      end record;

   package Managed_Asset_List is
     new Ada.Containers.Doubly_Linked_Lists
       (Managed_Asset_Record);

   package Asset_Classification_List is
     new Ada.Containers.Doubly_Linked_Lists
       (Managed_Asset_List.Cursor, Managed_Asset_List."=");

   type Root_Asset_Manager_Record is
     abstract new Root_Manager_Type
     and Carthage.Handles.Stacks.Stack_Manager_Interface with
      record
         Meta_Manager  : Stack_Meta_Manager_Access;
         Assets        : Managed_Asset_List.List;
         Spotters      : Asset_Classification_List.List;
         Idle_Spotters : WL.String_Sets.Set;
         Movers        : Asset_Classification_List.List;
         Ground_Cargo  : Asset_Classification_List.List;
      end record;

   overriding function Target_Id
     (Manager : Root_Asset_Manager_Record)
      return String
   is (Manager.House.Tag & "-assets");

   procedure Load_Assets
     (Manager : in out Root_Asset_Manager_Record)
   is abstract;

--     function Is_Idle
--       (Manager : Root_Asset_Manager_Record;
--        Asset   : Carthage.Handles.Assets.Any_Reference)
--        return Boolean
--        is abstract;
--
--     function Score
--       (Manager : Root_Asset_Manager_Record;
--        Asset   : Carthage.Handles.Assets.Any_Reference;
--        Goal    : Asset_Manager_Goal'Class)
--        return Boolean
--        is abstract;
--
   overriding procedure Initialize
     (Manager : not null access Root_Asset_Manager_Record);

   overriding function Update
     (Manager : not null access Root_Asset_Manager_Record)
      return Duration;

   overriding procedure Get_Resource_Requirements
     (Manager : in out Root_Asset_Manager_Record;
      Minimum : in out Resource_Stock'Class;
      Desired : in out Resource_Stock'Class);

   overriding procedure Take_Resource
     (From     : in out Root_Asset_Manager_Record;
      Resource : Carthage.Handles.Resources.Resource_Handle;
      Quantity : in out Resource_Quantity);

   overriding function Average_Update_Frequency
     (Manager : Root_Asset_Manager_Record)
      return Duration
   is (Carthage.Calendar.Days (1));

end Carthage.Managers.Assets;
